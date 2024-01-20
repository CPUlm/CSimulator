#include "commons.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>

static char *char_repr[256] = {
    "00000000", "00000001", "00000010", "00000011", "00000100", "00000101", "00000110", "00000111", "00001000",
    "00001001", "00001010", "00001011", "00001100", "00001101", "00001110", "00001111", "00010000", "00010001",
    "00010010", "00010011", "00010100", "00010101", "00010110", "00010111", "00011000", "00011001", "00011010",
    "00011011", "00011100", "00011101", "00011110", "00011111", "00100000", "00100001", "00100010", "00100011",
    "00100100", "00100101", "00100110", "00100111", "00101000", "00101001", "00101010", "00101011", "00101100",
    "00101101", "00101110", "00101111", "00110000", "00110001", "00110010", "00110011", "00110100", "00110101",
    "00110110", "00110111", "00111000", "00111001", "00111010", "00111011", "00111100", "00111101", "00111110",
    "00111111", "01000000", "01000001", "01000010", "01000011", "01000100", "01000101", "01000110", "01000111",
    "01001000", "01001001", "01001010", "01001011", "01001100", "01001101", "01001110", "01001111", "01010000",
    "01010001", "01010010", "01010011", "01010100", "01010101", "01010110", "01010111", "01011000", "01011001",
    "01011010", "01011011", "01011100", "01011101", "01011110", "01011111", "01100000", "01100001", "01100010",
    "01100011", "01100100", "01100101", "01100110", "01100111", "01101000", "01101001", "01101010", "01101011",
    "01101100", "01101101", "01101110", "01101111", "01110000", "01110001", "01110010", "01110011", "01110100",
    "01110101", "01110110", "01110111", "01111000", "01111001", "01111010", "01111011", "01111100", "01111101",
    "01111110", "01111111", "10000000", "10000001", "10000010", "10000011", "10000100", "10000101", "10000110",
    "10000111", "10001000", "10001001", "10001010", "10001011", "10001100", "10001101", "10001110", "10001111",
    "10010000", "10010001", "10010010", "10010011", "10010100", "10010101", "10010110", "10010111", "10011000",
    "10011001", "10011010", "10011011", "10011100", "10011101", "10011110", "10011111", "10100000", "10100001",
    "10100010", "10100011", "10100100", "10100101", "10100110", "10100111", "10101000", "10101001", "10101010",
    "10101011", "10101100", "10101101", "10101110", "10101111", "10110000", "10110001", "10110010", "10110011",
    "10110100", "10110101", "10110110", "10110111", "10111000", "10111001", "10111010", "10111011", "10111100",
    "10111101", "10111110", "10111111", "11000000", "11000001", "11000010", "11000011", "11000100", "11000101",
    "11000110", "11000111", "11001000", "11001001", "11001010", "11001011", "11001100", "11001101", "11001110",
    "11001111", "11010000", "11010001", "11010010", "11010011", "11010100", "11010101", "11010110", "11010111",
    "11011000", "11011001", "11011010", "11011011", "11011100", "11011101", "11011110", "11011111", "11100000",
    "11100001", "11100010", "11100011", "11100100", "11100101", "11100110", "11100111", "11101000", "11101001",
    "11101010", "11101011", "11101100", "11101101", "11101110", "11101111", "11110000", "11110001", "11110010",
    "11110011", "11110100", "11110101", "11110110", "11110111", "11111000", "11111001", "11111010", "11111011",
    "11111100", "11111101", "11111110", "11111111"};

static char strbuf[256] = {0};

const char *to_binary_str(value_t v, bus_size_t size)
{
#ifdef MODE_64_BIT
    char *begin = strbuf + 2;

    strncpy(begin + 0, char_repr[(v >> 56) & 0xff], 8);
    strncpy(begin + 8, char_repr[(v >> 48) & 0xff], 8);
    strncpy(begin + 16, char_repr[(v >> 40) & 0xff], 8);
    strncpy(begin + 24, char_repr[(v >> 32) & 0xff], 8);
    strncpy(begin + 32, char_repr[(v >> 24) & 0xff], 8);
    strncpy(begin + 40, char_repr[(v >> 16) & 0xff], 8);
    strncpy(begin + 48, char_repr[(v >> 8) & 0xff], 8);
    strncpy(begin + 56, char_repr[(v >> 0) & 0xff], 8);
    begin[size] = '\0';

    char *str = strbuf + 64 - size;
    return strncpy(str, "0b", 2);
#endif
    char *begin = strbuf + 2;

    strncpy(begin + 0, char_repr[(v >> 24) & 0xff], 8);
    strncpy(begin + 8, char_repr[(v >> 16) & 0xff], 8);
    strncpy(begin + 16, char_repr[(v >> 8) & 0xff], 8);
    strncpy(begin + 24, char_repr[(v >> 0) & 0xff], 8);
    begin[32] = '\0';

    char *str = strbuf + 32 - size;
    return strncpy(str, "0b", 2);
}

void print_value(FILE *stream, value_t v, bus_size_t size)
{
    uint64_t un_sigval = v;
    int64_t sigval = (int64_t)v;

    if (v >> (size - 1))
    {
        // Negative number so sign extend
        sigval = (int64_t)(-1) << size | sigval;
    }

#ifdef MODE_64_BIT
    fprintf(stream, "%66s, Hexadecimal: %#18lx, Signed Decimal: % 20ld, Unsigned Decimal: % 20ld", to_binary_str(v, size), v, un_sigval, sigval);
#else
    fprintf(stream, "%34s, Hexadecimal: %#10lx, Signed Decimal: % 20ld, Unsigned Decimal: % 20ld", to_binary_str(v, size), v, un_sigval, sigval);
#endif
}

value_t get_input(const char *var_name, bus_size_t bus_size)
{
    char buffer[256] = {0};
    for (;;)
    {
        printf("Value of '%s' (bus size: %d): ", var_name, bus_size);
        char *read = fgets(buffer, sizeof(buffer), stdin);

        if (read == NULL)
        {
            printf("\n");
            exit(0);
        }

        size_t char_read = strcspn(buffer, "\r\n");
        buffer[char_read] = 0;

        bool is_neg = false;

        if (char_read > 0 && buffer[0] == '-')
        {
            is_neg = true;
        }

        bool is_ok = false;
        value_t v;

        if (char_read > 1 + is_neg && buffer[is_neg] == '0' && buffer[1 + is_neg] == 'b')
        {
            // Binary Constant
            char *rest = NULL;
            v = strtoull(buffer + 2 + is_neg, &rest, 2);
            is_ok = errno != ERANGE && rest == buffer + char_read;
        }
        else if (char_read > 1 + is_neg && buffer[is_neg] == '0' && buffer[1 + is_neg] == 'd')
        {
            // Decimal Constant
            char *rest = NULL;
            v = strtoull(buffer + 2 + is_neg, &rest, 10);
            is_ok = errno != ERANGE && rest == buffer + char_read;
        }
        else if (char_read > 1 + is_neg && buffer[is_neg] == '0' && buffer[1 + is_neg] == 'x')
        {
            // Hexadecimal Constant
            char *rest = NULL;
            v = strtoull(buffer + 2 + is_neg, &rest, 16);
            is_ok = errno != ERANGE && rest == buffer + char_read;
        }
        else
        {
            // Binary Digits
            char *rest = NULL;
            v = strtoull(buffer + is_neg, &rest, 2);
            is_ok = errno != ERANGE && rest == buffer + char_read;
        }

        if (!is_ok)
        {
            fprintf(stdout, "Cannot interpret this constant : '%s'\n", buffer);
            errno = 0;
        }
        else if (is_neg && v <= (1 << (bus_size - 1)))
        {
            return (-v) & ((1 << bus_size) - 1);
        }
        else if (!is_neg && v < (1 << bus_size))
        {
            return v;
        }
        else
        {
            fprintf(stdout, "The constant '%s' does not fit in %i bits.\n", buffer, bus_size);
            errno = 0;
        }
    }
}

/** Returns true if @a s ends with @a t (equivalently if @a t is a suffix of @a s). */
static int strendswith(const char *s, const char *t)
{
    size_t slen = strlen(s);
    size_t tlen = strlen(t);
    if (tlen > slen)
        return 0;
    return strncmp(s + slen - tlen, t, tlen) == 0;
}

int main(int argc, char **argv)
{
    cycle_t nb_cycle = (cycle_t)(-1L);

    const char *ram_file = NULL;
    const char *rom_file = NULL;

    opterr = 0;
    int c;

    while ((c = getopt(argc, argv, "hs:p:d:")) != -1)
        switch (c)
        {
        case 's':
        {
            char *remainder;
            nb_cycle = strtoull(optarg, &remainder, 0);

            if (nb_cycle == 0 || remainder != optarg + strlen(optarg))
            {
                fprintf(stderr, "Invalid number of cycle '%s'.\n", optarg);
                return 1;
            }

            break;
        }
        case 'p':
            if (strendswith(optarg, ".po") || strendswith(optarg, ".rom") || strendswith(optarg, ".code"))
            {
                rom_file = optarg;
            }
            else
            {
                fprintf(stderr, "Invalid RAM data: %s.\n", optarg);
                return 1;
            }

            break;
        case 'd':
        {
            if (strendswith(optarg, ".do") || strendswith(optarg, ".ram") || strendswith(optarg, ".data"))
            {
                ram_file = optarg;
            }
            else
            {
                fprintf(stderr, "Invalid RAM data: %s.\n", optarg);
                return 1;
            }

            break;
        }
        case 'h':
            printf("Usage: %s [-p program] [-d data] [-s step]\n", argv[0]);
            return 0;
        case '?':
            if (optopt == 's' || optopt == 'p' || optopt == 'd')
            {
                fprintf(stderr, "Option -%c requires an argument.\n", optopt);
            }
            else if (isprint(optopt))
            {
                fprintf(stderr, "Unknown option `-%c'.\n", optopt);
            }
            else
            {
                fprintf(stderr, "Unknown option character `\\x%x'.\n", optopt);
            }
            return 1;
        default:
            abort();
        }

    if (optind != argc)
    {
        fprintf(stderr, "Unknown argument: %s\n", argv[optind]);
        return 1;
    }

    init_rom(rom_file);
    init_ram(ram_file);

    cycle_t curr_cycle = 0;
    do
    {
    } while (!do_cycle(&curr_cycle) && curr_cycle < nb_cycle);

    end_simulation();
    return 0;
}