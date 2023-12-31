/*
 * This file is auto-generated by simulator_c.ml.
 * Do not modify it directly!
 */

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>

#include "memory.h"

const unsigned long long nbSteps = 0xFFFFFFFFFF;

typedef struct gate_t
{
    word_t (*fct)();
    word_t value;
    bool is_calculated;
} gate_t;

static inline void gate_init(gate_t *gate)
{
    gate->is_calculated = false;
    gate->value = 0;
}

static inline word_t gate_calc(gate_t *gate)
{
    if (!gate->is_calculated)
    {
        gate->value = gate->fct();
        gate->is_calculated = true;
    }

    return gate->value;
}

typedef struct reg_t
{
    uint32_t (*fct)();
    word_t value;
    word_t old_value;
    bool is_calculated;
} reg_t;

static inline void reg_init(reg_t *gate)
{
    gate->is_calculated = false;
    gate->value = 0;
    gate->old_value = gate->value;
}

static inline word_t reg_calc(reg_t *gate)
{
    if (!gate->is_calculated)
    {
        gate->value = gate->fct();
        gate->is_calculated = true;
    }

    return gate->value;
}

static inline void ram_write(ram_t *ram, int write_enabled, addr_t addr, word_t value)
{
    if (write_enabled)
        ram_set(ram, addr, value);
}

$DEFS$

/** Returns true if @a s ends with @a t (equivalently if @a t is a suffix of @a s). */
static int strendswith(const char *s, const char *t)
{
    size_t slen = strlen(s);
    size_t tlen = strlen(t);
    if (tlen > slen) return 1;
    return strcmp(s + slen - tlen, t);
}

int main(int argc, char *argv[])
{
    size_t ram_file_count = 0, rom_file_count = 0;
    const char **ram_files = malloc(sizeof(const char *) * (argc - 1));
    const char **rom_files = malloc(sizeof(const char *) * (argc - 1));
    check_alloc(ram_files);
    check_alloc(rom_files);
    for (int i = 1; i < argc; ++i)
    {
        if (argv[i][0] != '-')
        {
            if (strendswith(argv[i], ".do") || strendswith(argv[i], ".ram") || strendswith(argv[i], ".data")) {
                ram_files[ram_file_count++] = argv[i];
            } else if (strendswith(argv[i], ".po") || strendswith(argv[i], ".rom") || strendswith(argv[i], ".code")) {
                rom_files[rom_file_count++] = argv[i];
            } else {
                fprintf(stderr, "error: unknown type for file '%s'\n", argv[i]);
            }
        } else {
            fprintf(stderr, "error: unknown option '%s'\n", argv[i]);
        }
    }

    size_t cur_ram_file_idx = 0, cur_rom_file_idx = 0;
$INIT$

    free(ram_files);
    free(rom_files);

    for (unsigned long long iStep = 0; iStep != nbSteps; iStep++)
    {
        printf("Step %lld:\n", iStep + 1);
$CYCLE$
    }
}