#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

#ifdef MODE_64_BIT
typedef uint_fast64_t value_t;
#else
typedef uint_fast32_t value_t;
#endif

typedef uint_fast32_t cycle_t;
typedef unsigned char bus_size_t;

void print_value(FILE *stream, value_t v, bus_size_t size);

bool do_cycle(cycle_t *cycle_id);

void init_rom(const char *rom_file);
void init_ram(const char *ram_file);

void end_simulation();