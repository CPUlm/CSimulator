// Copyright (c) 2024 Hubert Gruniaux
// This file is part of SparseMemory which is released under the MIT license.
// See file LICENSE.txt for full license details.

#include <gtest/gtest.h>

#include "memory.h"

TEST(RomTest, rom_create) {
  static word_t data[8] = {0xab, 0xbc, 0xcd, 0xde, 0x12, 0x23, 0x34, 0x45};

  rom_t rom = rom_create(data, 8);
  ASSERT_NE(rom.data, nullptr);

  EXPECT_EQ(rom_get(rom, 0), 0xab);
  EXPECT_EQ(rom_get(rom, 1), 0xbc);
  EXPECT_EQ(rom_get(rom, 2), 0xcd);
  EXPECT_EQ(rom_get(rom, 3), 0xde);
  EXPECT_EQ(rom_get(rom, 4), 0x12);
  EXPECT_EQ(rom_get(rom, 5), 0x23);
  EXPECT_EQ(rom_get(rom, 6), 0x34);
  EXPECT_EQ(rom_get(rom, 7), 0x45);

  rom_destroy(rom);
}
