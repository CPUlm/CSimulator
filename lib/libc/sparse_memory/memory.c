// Copyright (c) 2024 Hubert Gruniaux
// This file is part of SparseMemory which is released under the MIT license.
// See file LICENSE.txt for full license details.

#include "memory.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

void check_alloc(void *ptr) {
  if (ptr == NULL) {
    fprintf(stderr, "error: out of memory (failed allocation)\n");
    abort();
  }
}

static word_t *read_file(const char *filename, addr_t *data_len) {
  assert(data_len != NULL);

  FILE *file = fopen(filename, "rb");
  if (file == NULL)
    return NULL;

  // We declare data here, so it is always correctly defined
  // when we enter the error block.
  word_t *data = NULL;

  // Get file size (in bytes).
  fseek(file, 0, SEEK_END);
  long file_size = ftell(file);
  if (file_size % sizeof(word_t) != 0)
    goto error; // the file does not contain word (of 4 bytes)
  rewind(file);

  data = (word_t *)malloc(file_size);
  if (data == NULL)
    goto error; // memory error

  size_t read_words =
      fread(data, sizeof(word_t), file_size / sizeof(word_t), file);
  if (read_words * sizeof(word_t) != file_size)
    goto error; // failed to read all the file

  *data_len = read_words;

  fclose(file);
  return data;

error:
  free(data);
  fclose(file);
  return NULL;
}

#ifdef __GNUC__
#define MEM_NORETURN __attribute__((__noreturn__))
#elif defined(_MSC_VER)
#define MEM_NORETURN __declspec(noreturn)
#else
#define MEM_NORETURN
#endif

MEM_NORETURN static void file_error(const char *filename) {
  fprintf(stderr, "error: failed to read file '%s'\n", filename);
  abort();
}

/*
 * RAM abstraction.
 */

/* We want a RAM block to be able to store many data with consecutive addresses.
 * However, it is just not possible to allocate a large array of gigabytes.
 * So, the solution is to mimic how your OS works and how virtual memory is
 * implemented. Our RAM block is decomposed into memory pages (that have the
 * same size as the real ones from the OS for performance reasons). When
 * accessing the RAM, we retrieve the corresponding memory page or create one on
 * the fly. The effective mapping between the addresses and the memory pages
 * (and real physical memory address) are done using a hash table. To remain
 * simple, the hash table uses open addressing (linear probing).
 */

// Initial RAM hast table bucket count. Must be a power of 2.
#define INITIAL_RAM_HT_SIZE 64
#define BASE_ADDR_MASK

// There is some platform-specific code to retrieve the current configured
// memory page size. The code is quite self-contained and has a default behavior
// in case of an unsupported platform, so this is not too bad.

// We need to detect POSIX (to know if we can include <unistd.h>).
// GCC defines __unix__ on most POSIX systems except the Apple ones.
#if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
#define IS_POSIX 1
#else
#define IS_POSIX 0
#endif

#ifdef _WIN32
#include <Windows.h>
#elif IS_POSIX
#include <unistd.h>
#endif

/** Returns the currently used memory page of the underlying OS. */
static size_t get_os_memory_page() {
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

#ifndef RAM_NO_READ_LISTENER
struct ram_read_listener_t {
  ram_read_listener_fn_t callback;
  struct ram_read_listener_t *next;
  addr_t addr_low, addr_high;
};
#endif // !RAM_NO_READ_LISTENER

#ifndef RAM_NO_WRITE_LISTENER
struct ram_write_listener_t {
  ram_write_listener_fn_t callback;
  struct ram_write_listener_t *next;
  addr_t addr_low, addr_high;
};
#endif // !RAM_NO_WRITE_LISTENER

typedef struct ram_page_t {
  addr_t base_addr;
  word_t *data;
} ram_page_t;

struct ram_t {
  ram_page_t *buckets;
  // page_count must always be a power of 2.
  addr_t bucket_count;
  addr_t page_count;

#ifndef RAM_NO_READ_LISTENER
  struct ram_read_listener_t *read_listener;
#endif // !RAM_NO_READ_LISTENER
#ifndef RAM_NO_WRITE_LISTENER
  struct ram_write_listener_t *write_listener;
#endif // !RAM_NO_WRITE_LISTENER

  // Size, in words, of a RAM's page size.
  addr_t page_size;
};

// Hash the given integer to have a better distribution.
addr_t hash(addr_t x) {
  // From https://stackoverflow.com/a/12996028
  // addr_t is assumed to be 32-bits
  x = ((x >> 16) ^ x) * 0x45d9f3b;
  x = ((x >> 16) ^ x) * 0x45d9f3b;
  x = (x >> 16) ^ x;
  return x;
}

// Implements a hash table lookup where buckets and bucket_count is the hash
// table's bucket array and base_addr is the key. The function returns the index
// where the search slot should be. However, if the key is not present in the
// hash table, then the returned index is one where the key should be inserted
// (the first empty bucket that was found in the search algorithm).
static addr_t ht_find(ram_page_t *buckets, addr_t bucket_count,
                      addr_t base_addr) {
  // AND mask by (capacity - 1) to be sure that index is not out of bounds.
  // This works because bucket count is always a power of two.
  uint32_t index = hash(base_addr) & (bucket_count - 1);

  // We do a simple linear search in the hash table (open addressing).
  while (buckets[index].data != NULL) {
    // If we found the memory page then access the correct memory cell and
    // return it.
    if (buckets[index].base_addr == base_addr) {
      return index;
    }

    // If not, we continue to search.
    index += 1;
    if (index >= bucket_count)
      index = 0;
  }

  // Returns the place where it should belong if it was present.
  return index;
}

static void init_ram_page(ram_t *ram, ram_page_t *page, addr_t base_addr) {
  page->base_addr = base_addr;
  page->data = (word_t *)calloc(sizeof(word_t), ram->page_size);
  check_alloc(page->data);
}

ram_t *ram_create() {
  ram_t *ram = (ram_t *)malloc(sizeof(ram_t));
  check_alloc(ram);

#ifndef RAM_NO_READ_LISTENER
  ram->read_listener = NULL;
#endif // !RAM_NO_READ_LISTENER
#ifndef RAM_NO_WRITE_LISTENER
  ram->write_listener = NULL;
#endif // !RAM_NO_WRITE_LISTENER

  // Precompute the RAM's page size.
  ram->page_size = get_os_memory_page() / sizeof(word_t);

  ram->buckets = (ram_page_t *)calloc(INITIAL_RAM_HT_SIZE, sizeof(ram_page_t));
  check_alloc(ram->buckets);
  ram->bucket_count = INITIAL_RAM_HT_SIZE;
  ram->page_count = 1;

  // Initialize the first memory's page (the region [0..PAGE_SIZE)).
  init_ram_page(ram, &ram->buckets[ht_find(ram->buckets, ram->bucket_count, 0)],
                0);

  return ram;
}

// Returns the memory's page corresponding to the given addr. This effectively
// does a hash table lookup internally. However, this also adds the memory's
// page if it was not already present (and therefore may resize the hash table).
static ram_page_t *get_ram_page(ram_t *ram, addr_t addr) {
  // We clear the lower bits of the address to get the page's base address.
  const addr_t base_addr = addr & ~(ram->page_size - 1);

  // Try to find a corresponding memory's page.
  addr_t index = ht_find(ram->buckets, ram->bucket_count, base_addr);
  if (ram->buckets[index].base_addr == base_addr) {
    // We found one!
    return &ram->buckets[index];
  }

  // We failed to find the corresponding memory's page. Now, let's create it.

  // But if there is not enough space, resize the hash table.
  if (ram->page_count == ram->bucket_count) {
    // Allocate new buckets.
    addr_t old_bucket_count = ram->bucket_count;
    ram->bucket_count *= 2;
    ram_page_t *new_pages =
        (ram_page_t *)calloc(ram->bucket_count, sizeof(ram_page_t));
    check_alloc(new_pages);

    // Rehash the table (we just reinsert individually each of the previous
    // memory pages into the new hash table).
    for (addr_t i = 0; i < old_bucket_count; ++i) {
      ram_page_t *page = ram->buckets + i;
      if (page->data != NULL) {
        new_pages[ht_find(new_pages, ram->bucket_count, page->base_addr)] =
            *page;
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

void ram_init(ram_t* ram, const word_t *data, size_t data_len) {
  assert(ram != NULL && data != NULL);

  addr_t base_address = 0;
  while (base_address < data_len) {
    ram_page_t *page = get_ram_page(ram, base_address);
    assert(page != NULL);
    addr_t words_to_copy =
        ((data_len - base_address < ram->page_size) ? (data_len - base_address)
                                                    : ram->page_size);
    memcpy(page->data, data + base_address, sizeof(word_t) * words_to_copy);
    base_address += ram->page_size;
  }
}

ram_t *ram_from_file(const char *filename) {
  addr_t data_len = 0;
  word_t *data = read_file(filename, &data_len);
  if (data == NULL)
    file_error(filename);

  ram_t *ram = ram_create();
  ram_init(ram, data, data_len);
  return ram;
}

void ram_destroy(ram_t *ram) {
  if (ram == NULL)
    return;

#ifndef RAM_NO_READ_LISTENER
  // Free all read listeners.
  struct ram_read_listener_t *read_it = ram->read_listener;
  while (read_it != NULL) {
    struct ram_read_listener_t *next = read_it->next;
    free(read_it);
    read_it = next;
  }
#endif // !RAM_NO_READ_LISTENER

#ifndef RAM_NO_WRITE_LISTENER
  // Free all write listeners.
  struct ram_write_listener_t *write_it = ram->write_listener;
  while (write_it != NULL) {
    struct ram_write_listener_t *next = write_it->next;
    free(write_it);
    write_it = next;
  }
#endif // !RAM_NO_WRITE_LISTENER

  for (addr_t i = 0; i < ram->bucket_count; ++i) {
    // free() is well defined on NULL pointers, so we don't need to check
    // if ram->buckets[i] is a used page or NULL one.
    free(ram->buckets[i].data);
  }

  free(ram->buckets);
  free(ram);
}

static void handle_read_listeners(ram_t *ram, addr_t addr) {
#ifndef RAM_NO_READ_LISTENER
  struct ram_read_listener_t *it = ram->read_listener;
  while (it != NULL) {
    if (addr >= it->addr_low && addr <= it->addr_high)
      it->callback(addr);
    it = it->next;
  }
#endif // !RAM_NO_READ_LISTENER
}

static void handle_write_listeners(ram_t *ram, addr_t addr, word_t new_word) {
#ifndef RAM_NO_WRITE_LISTENER
  struct ram_write_listener_t *it = ram->write_listener;
  while (it != NULL) {
    if (addr >= it->addr_low && addr <= it->addr_high)
      it->callback(addr, new_word);
    it = it->next;
  }
#endif // !RAM_NO_WRITE_LISTENER
}

word_t ram_get(ram_t *ram, addr_t addr) {
  ram_page_t *page = get_ram_page(ram, addr);
  handle_read_listeners(ram, addr);
  return page->data[addr - page->base_addr];
}

void ram_set(ram_t *ram, addr_t addr, word_t value) {
  ram_page_t *page = get_ram_page(ram, addr);
  page->data[addr - page->base_addr] = value;
  handle_write_listeners(ram, addr, value);
}

word_t ram_get_set(ram_t *ram, addr_t addr, word_t value) {
  ram_page_t *page = get_ram_page(ram, addr);
  addr_t in_page_addr = addr - page->base_addr;
  handle_read_listeners(ram, addr);
  word_t old_value = page->data[in_page_addr];
  page->data[in_page_addr] = value;
  handle_write_listeners(ram, addr, value);
  return old_value;
}

#ifndef RAM_NO_READ_LISTENER
void ram_install_read_listener(ram_t *ram, addr_t addr_low, addr_t addr_high,
                               ram_read_listener_fn_t callback) {
  struct ram_read_listener_t *listener =
      (struct ram_read_listener_t *)malloc(sizeof(struct ram_read_listener_t));
  check_alloc(listener);

  listener->callback = callback;
  listener->next = NULL;
  listener->addr_low = addr_low;
  listener->addr_high = addr_high;

  // Insert the listener to the RAM block.
  if (ram->read_listener == NULL) {
    ram->read_listener = listener;
  } else {
    struct ram_read_listener_t *it = ram->read_listener;
    while (it->next != NULL)
      it = it->next;
    it->next = listener;
  }
}
#endif // !RAM_NO_READ_LISTENER

#ifndef RAM_NO_WRITE_LISTENER
void ram_install_write_listener(ram_t *ram, addr_t addr_low, addr_t addr_high,
                                ram_write_listener_fn_t callback) {
  struct ram_write_listener_t *listener = (struct ram_write_listener_t *)malloc(
      sizeof(struct ram_write_listener_t));
  check_alloc(listener);

  listener->callback = callback;
  listener->next = NULL;
  listener->addr_low = addr_low;
  listener->addr_high = addr_high;

  // Insert the listener to the RAM block.
  if (ram->write_listener == NULL) {
    ram->write_listener = listener;
  } else {
    struct ram_write_listener_t *it = ram->write_listener;
    while (it->next != NULL)
      it = it->next;
    it->next = listener;
  }
}
#endif // !RAM_NO_WRITE_LISTENER

/*
 * ROM abstraction.
 */

/* We do not use the same algorithms and data structures
 * for the ROM as it is much simpler. The ROM is only initialized
 * once and can never be modifier. Moreover, the ROM initial
 * value must fit in the real memory, so no need to be smart here:
 * we just use a raw array to represent the RAM. */

rom_t rom_create(const word_t *data, size_t data_len) {
  word_t *rom_data = (word_t *)malloc(sizeof(word_t) * data_len);
  check_alloc(rom_data);

  memcpy(rom_data, data, sizeof(word_t) * data_len);

  rom_t rom;
  rom.data = rom_data;
  return rom;
}

rom_t rom_from_file(const char *filename) {
  addr_t data_len;
  const word_t *data = read_file(filename, &data_len);
  if (data == NULL)
    file_error(filename);

  return rom_create(data, data_len);
}

void rom_destroy(rom_t rom) { free((word_t *)rom.data); }
