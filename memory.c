#include "memory.h"

#include <string.h>
#include <stdio.h>

static void check_alloc(void* ptr) {
    if (ptr == NULL) {
        fprintf(stderr, "fatal error: out of memory (failed allocation)");
        abort();
    }
}

/*
 * RAM abstraction.
 */

/* We want a RAM block to be able to store many data with consecutive addresses.
 * However, it is just not possible to allocate a large array of gigabytes.
 * So, the solution is to mimic how your OS works and how virtual memory is implemented.
 * Our RAM block is decomposed into memory pages (that have the same size as the
 * real ones from the OS for performance reasons). When accessing the RAM, we
 * retrieve the corresponding memory page or create one on the fly.
 * The effective mapping between the addresses and the memory pages (and real
 * physical memory address) are done using a hash table. To remain simple,
 * the hash table uses open addressing (linear probing).
 */

// Initial RAM hast table bucket count. Must be a power of 2.
#define INITIAL_RAM_HT_SIZE 64
#define BASE_ADDR_MASK

// There is some platform-specific code to retrieve the current configured
// memory page size. The code is quite self-contained and has a default behavior
// in case of unsupported platform, so this is not too bad.

// We need to detect POSIX (to know if we can include <unistd.h>).
// GCC defines __unix__ on most POSIX systems except the Apple ones.
#if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
#define IS_POSIX 1
#else
#define IS_POSIX 0
#endif

#ifdef _WIN32
#include <windows.h>
#elif IS_POSIX
#include <unistd.h>
#endif

/** Returns the currently used memory page of the underlying OS. */
static size_t get_os_memory_page()
{
#ifdef _WIN32
    SYSTEM_INFO sys_info;
    GetSystemInfo(&sys_info);
    return sys_info.dwPageSize;
#elif IS_POSIX
    return sysconf(_SC_PAGESIZE);
#else
    return 4096;
#endif
}

typedef struct ram_page_t
{
    addr_t base_addr;
    word_t *data;
} ram_page_t;

struct ram_t
{
    ram_page_t *buckets;
    // page_count must always be a power of 2.
    addr_t bucket_count;
    addr_t page_count;

    // Size, in bytes, of a RAM's page size.
    addr_t page_size;
};

// Hash the given integer to have a better distribution.
addr_t hash(addr_t x)
{
    // From https://stackoverflow.com/a/12996028
    // addr_t is assumed to be 32-bits
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    return x;
}

// Implements a hash table lookup where buckets and bucket_count is the hash table's
// bucket array and base_addr is the key. The function returns the index where the
// search slot should be. However, if the key is not present in the hash table, then
// the returned index is one where the key should be inserted (the first empty bucket
// that was found in the search algorithm).
static addr_t ht_find(ram_page_t *buckets, addr_t bucket_count, addr_t base_addr)
{
    // AND mask by (capacity - 1) to be sure that index is not out of bounds.
    // This works because bucket count is always a power of two.
    uint32_t index = hash(base_addr) & (bucket_count - 1);

    // We do a simple linear search in the hash table (open addressing).
    while (buckets[index].data != NULL)
    {
        // If we found the memory page then access the correct memory cell and return it.
        if (buckets[index].base_addr == base_addr)
        {
            return index;
        }

        // If not, we continue to search.
        index += 1;
        if (index >= bucket_count)
            index = 0;
    }

    // Returns the place where it should belongs if it was present.
    return index;
}

static void init_ram_page(ram_t *ram, ram_page_t *page, addr_t base_addr)
{
    page->base_addr = base_addr;
    page->data = (word_t *)malloc(ram->page_size);
    check_alloc(page->data);
}

ram_t *ram_create()
{
    ram_t *ram = (ram_t *)malloc(sizeof(ram_t));
    check_alloc(ram);

    // Precompute the RAM's page size.
    ram->page_size = get_os_memory_page();

    ram->buckets = (ram_page_t *)calloc(INITIAL_RAM_HT_SIZE, sizeof(ram_page_t));
    check_alloc(ram->buckets);
    ram->bucket_count = INITIAL_RAM_HT_SIZE;
    ram->page_count = 1;

    // Initialize the first memory's page (the region [0..PAGE_SIZE]).
    init_ram_page(ram, &ram->buckets[ht_find(ram->buckets, ram->bucket_count, 0)], 0);

    return ram;
}

void ram_destroy(ram_t *ram)
{
    if (ram == NULL)
        return;

    for (addr_t i = 0; i < ram->bucket_count; ++i)
    {
        // free() is well defined on NULL pointers, so we don't need to check
        // if ram->buckets[i] is a used page or NULL one.
        free(ram->buckets[i].data);
    }

    free(ram->buckets);
    free(ram);
}

// Returns the memory's page corresponding to the given addr. This effectively
// does a hash table lookup internally. However, this also adds the memory's
// page if it was not already present (and therefore may resize the hash table).
static ram_page_t *get_ram_page(ram_t *ram, addr_t addr)
{
    // We clear the lower bits of the address to get the page's base address.
    const addr_t base_addr = addr & ~(ram->page_size - 1);

    // Try to find a corresponding memory's page.
    addr_t index = ht_find(ram->buckets, ram->bucket_count, base_addr);
    if (ram->buckets[index].base_addr == base_addr)
    {
        // We found one!
        return &ram->buckets[index];
    }

    // We failed to find the corresponding memory's page. Now, let's create it.

    // But if there is not enough space, resize the hash table.
    if (ram->page_count == ram->bucket_count)
    {
        // Allocate new buckets.
        addr_t old_bucket_count = ram->bucket_count;
        ram->bucket_count *= 2;
        ram_page_t *new_pages = (ram_page_t *)malloc(sizeof(ram_page_t) * ram->bucket_count);
        check_alloc(new_pages);
        
        // Rehash the table (we just reinsert individually each of the previous memory pages
        // into the new hash table).
        for (addr_t i = 0; i < old_bucket_count; ++i)
        {
            ram_page_t *page = ram->buckets + i;
            if (page->data != NULL)
            {
                new_pages[ht_find(new_pages, ram->bucket_count, page->base_addr)] = *page;
            }
        }

        // And finally, free and swap the old bucket array and the new one.
        free(ram->buckets);
        ram->buckets = new_pages;
    }

    init_ram_page(ram, &ram->buckets[index], base_addr);
    ram->page_count += 1;

    return &ram->buckets[index];
}

word_t ram_get(ram_t *ram, addr_t addr)
{
    ram_page_t *page = get_ram_page(ram, addr);
    return page->data[addr - page->base_addr];
}

void ram_set(ram_t *ram, addr_t addr, word_t value)
{
    ram_page_t *page = get_ram_page(ram, addr);
    page->data[addr - page->base_addr] = value;
}

word_t ram_get_set(ram_t *ram, addr_t addr, word_t value)
{
    ram_page_t *page = get_ram_page(ram, addr);
    addr_t in_page_addr = addr - page->base_addr;
    word_t old_value = page->data[in_page_addr];
    page->data[in_page_addr] = value;
    return old_value;
}

/*
 * ROM abstraction.
 */

/* We do not use the same algorithms and data structures
 * for the ROM as it is much simpler. The ROM is only initialized
 * once and can never be modifier. Moreover, the ROM initial
 * value must fit in the real memory, so no need to be smart here:
 * we just use a raw array to represent the RAM. */

rom_t rom_create(const word_t *data, size_t data_len)
{
    word_t *rom_data = (word_t *)malloc(sizeof(word_t) * data_len);
    check_alloc(rom_data);

    memcpy(rom_data, data, sizeof(word_t) * data_len);

    rom_t rom;
    rom.data = rom_data;
    return rom;
}

void rom_destroy(rom_t rom)
{
    free((word_t*)rom.data);
}
