#ifndef CPULM_MEMORY_MAPPING_H
#define CPULM_MEMORY_MAPPING_H

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

typedef uint32_t word_t;
typedef uint32_t addr_t;

/*
 * RAM abstraction.
 */

typedef struct ram_t ram_t;

/** Creates an infinite-size RAM block. */
ram_t *ram_create();
/** Destroys the given @a ram block. */
void ram_destroy(ram_t* ram);
/** Gets the word at the given @a addr of the given @a ram block. */
word_t ram_get(ram_t* ram, addr_t addr);
/** Sets the word at the given @a addr to @a value of the given @a ram block. */
void ram_set(ram_t* ram, addr_t addr, word_t value);
/** Same as ram_get() then ram_set(), but faster. */
word_t ram_get_set(ram_t* ram, addr_t addr, word_t value);

/*
 * ROM abstraction.
 */

typedef word_t rom_t;

/** Creates a ROM block with the given initial @a data of length @a data_len. */
const rom_t *rom_create(const word_t *data, size_t data_len);
/** Destroys the given @a rom block. */
void rom_destroy(const rom_t *rom);
/** Gets the word at the given @a addr of the given @a rom block. */
static inline word_t rom_get(const rom_t *rom, addr_t addr) { return rom[addr]; }

#endif // !CPULM_MEMORY_MAPPING_H
