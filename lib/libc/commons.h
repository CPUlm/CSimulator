#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

#include "memory.h"

typedef uint_fast32_t value_t;
typedef uint_fast32_t cycle_t;
typedef unsigned char bus_size_t;

void print_header(FILE *stream);
void print_variable(FILE *stream, const char *var_name, value_t v, bus_size_t size);
value_t get_input(const char *var_name, bus_size_t bus_size);

bool do_cycle(cycle_t *cycle_id);

void init_rom(const char *rom_file);
void init_ram(const char *ram_file);

void end_simulation();
void clock_tick(ram_t *ram);