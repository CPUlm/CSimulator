// Copyright (c) 2024 Hubert Gruniaux
// This file is part of SparseMemory which is released under the MIT license.
// See file LICENSE.txt for full license details.

#ifndef CPULM_MEMORY_MAPPING_H
#define CPULM_MEMORY_MAPPING_H

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

typedef uint32_t word_t;
typedef uint32_t addr_t;

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

/** Checks if the given @a ptr is not NULL.
 *
 * If it NULL, dumps an error message and abort. */
void check_alloc(void *ptr);

/*
 * RAM abstraction.
 */

typedef struct ram_t ram_t;

/** Creates an infinite size RAM block. */
ram_t *ram_create();
/** Initializes the given RAM block with the given initial data. */
void ram_init(ram_t* ram, const word_t *data, size_t data_len);
/** Creates an infinite size RAM block first initialized with the data stored at
 * @a filename. */
ram_t *ram_from_file(const char *filename);
/** Destroys the given @a ram block. */
void ram_destroy(ram_t *ram);
/** Gets the word at the given @a addr of the given @a ram block. */
word_t ram_get(ram_t *ram, addr_t addr);
/** Sets the word at the given @a addr to @a value of the given @a ram block. */
void ram_set(ram_t *ram, addr_t addr, word_t value);
/** Same as ram_get() then ram_set(), but faster. */
word_t ram_get_set(ram_t *ram, addr_t addr, word_t value);

#ifndef RAM_NO_READ_LISTENER
typedef void (*ram_read_listener_fn_t)(addr_t);
/** Installs a RAM read listener for the memory range [@a addr_low,@a
 * addr_high].
 *
 * The function @a callback will be called just before each RAM reads to the
 * memory region [@a addr_low,@a addr_high]. The read address is given as an
 * argument to @a callback. */
void ram_install_read_listener(ram_t *ram, addr_t addr_low, addr_t addr_high,
                               ram_read_listener_fn_t callback);
#endif // !RAM_NO_READ_LISTENER

#ifndef RAM_NO_WRITE_LISTENER
typedef void (*ram_write_listener_fn_t)(addr_t, word_t);
/** Installs a RAM write listener for the memory range [@a addr_low,@a
 * addr_high].
 *
 * The function @a callback will be called just after each RAM writes to the
 * memory region [@a addr_low,@a addr_high]. The write address and the write
 * value are given as arguments to @a callback. */
void ram_install_write_listener(ram_t *ram, addr_t addr_low, addr_t addr_high,
                                ram_write_listener_fn_t callback);
#endif // !RAM_NO_WRITE_LISTENER

/*
 * ROM abstraction.
 */

typedef struct rom_t {
  const word_t *data;
} rom_t;

/** Creates a ROM block with the given initial @a data of length @a data_len. */
rom_t rom_create(const word_t *data, size_t data_len);
/** Creates a ROM block with the data stored in the file @a filename. */
rom_t rom_from_file(const char *filename);
/** Destroys the given @a rom block. */
void rom_destroy(rom_t rom);
/** Gets the word at the given @a addr of the given @a rom block. */
static inline word_t rom_get(rom_t rom, addr_t addr) { return rom.data[addr]; }

#ifdef __cplusplus
}
#endif // __cplusplus

#endif // !CPULM_MEMORY_MAPPING_H
