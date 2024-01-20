// Copyright (c) 2024 Hubert Gruniaux
// This file is part of SparseMemory which is released under the MIT license.
// See file LICENSE.txt for full license details.

#include "screen.h"

#include <assert.h>
#include <stdio.h>

#define SCI "\x1b["

#ifndef DISABLE_SCREEN_STYLING
enum style_flag_t {
  STYLE_BOLD = 1 << 18,
  STYLE_FAINT = 1 << 19,
  STYLE_ITALIC = 1 << 20,
  STYLE_UNDERLINE = 1 << 21,
  STYLE_BLINKING = 1 << 22,
  STYLE_HIDE = 1 << 23,
  STYLE_CROSSED = 1 << 24,
  STYLE_OVERLINE = 1 << 25,
};
#endif // !DISABLE_SCREEN_STYLING

void screen_init() {
  printf(SCI "?25l"); // hide cursor
  printf(SCI "2J");   // clear screen
}

void screen_init_with_ram_mapping(ram_t *ram) {
  assert(ram != NULL);
  screen_init();
  ram_install_write_listener(ram, SCREEN_BASE_ADDR,
                             SCREEN_BASE_ADDR + SCREEN_SIZE, &screen_ram_write);
}

void screen_terminate() {
  printf(SCI "?25h"); // show cursor
}

void screen_put_character(addr_t x, addr_t y, word_t styled_char) {
  assert(x < SCREEN_WIDTH);
  assert(y < SCREEN_HEIGHT);
  // Cursor indices are 1-based, thus the +1.
  // The first argument is the row number, the second is the column number.
  printf(SCI "%u;%uH", y + 1, x + 1);

  const char ch = (char)(styled_char & 0x7f); // ASCII character, 7-bits
                                              // 1-bit which is always set to 0

#ifndef DISABLE_SCREEN_STYLING
  // Check if there is any styling, and if yes, then handle it.
  // Because styling handling is heavy, we try to skip it if there is no style
  // specified.
  if (styled_char & (~0x7f)) {
    word_t fg_color = ((styled_char >> 8) & 0xf);  // 4-bits
    word_t bg_color = ((styled_char >> 13) & 0xf); // 4-bits
    // FIXME: because they are encoded in 4 bits, they can not be equal 16 (the
    //        code for the default color)

    if (fg_color <= 8) // normal colors
      fg_color += 30;
    else if (fg_color <= 15) // bright colors
      fg_color = 90 + (fg_color - 8);
    else if (fg_color == 16)
      fg_color = 39; // default foreground color

    if (bg_color <= 8) // normal colors
      bg_color += 40;
    else if (bg_color <= 15) // bright colors
      bg_color = 100 + (bg_color - 8);
    else if (bg_color == 16)
      bg_color = 49; // default background color

    char style_buffer[32]; // should be large enough
    char *it = style_buffer;
    *it++ = ';';

    // Handle all style attributes
    if (styled_char & STYLE_BOLD) {
      *it++ = '1';
      *it++ = ';';
    } else if (styled_char & STYLE_FAINT) {
      *it++ = '2';
      *it++ = ';';
    } else if (styled_char & STYLE_ITALIC) {
      *it++ = '3';
      *it++ = ';';
    } else if (styled_char & STYLE_UNDERLINE) {
      *it++ = '4';
      *it++ = ';';
    } else if (styled_char & STYLE_BLINKING) {
      *it++ = '5'; // slow blinking, fast is not widely supported
      *it++ = ';';
    } else if (styled_char & STYLE_HIDE) {
      // No widely supported
      *it++ = '8';
      *it++ = ';';
    } else if (styled_char & STYLE_CROSSED) {
      // No widely supported
      *it++ = '9';
      *it++ = ';';
    } else if (styled_char & STYLE_OVERLINE) {
      // No widely supported
      *it++ = '5';
      *it++ = '3';
      *it++ = ';';
    }

    *(--it) = '\0'; // printf expect a NUL-terminated string

    // Emit the Select Graphic Rendition command to style the character.
    printf(SCI "0;%u;%u%sm", fg_color, bg_color, style_buffer);
  }
#endif // !DISABLE_SCREEN_STYLING

  putchar(ch);
}

void screen_ram_write(addr_t addr, word_t new_word) {
  // Check if addr is in the bounds.
  assert(addr >= SCREEN_BASE_ADDR && addr < (SCREEN_BASE_ADDR + SCREEN_SIZE));

  const addr_t offset = addr - SCREEN_BASE_ADDR;
  const addr_t x = offset % SCREEN_WIDTH;
  const addr_t y = offset / SCREEN_WIDTH;
  screen_put_character(x, y, new_word);
}
