// Copyright (c) 2024 Hubert Gruniaux
// This file is part of SparseMemory which is released under the MIT license.
// See file LICENSE.txt for full license details.

#include <gtest/gtest.h>

#include "memory.h"

TEST(RamTest, ram_create) {
  ram_t *ram = ram_create();
  ASSERT_NE(ram, nullptr);
  ram_destroy(ram);
}

TEST(RamTest, ram_init) {
  ram_t *ram = ram_create();
  ASSERT_NE(ram, nullptr);

  const word_t data[] = { 1, 2, 3, 4, 5, 6, 7, 8 };
  ram_init(ram, data, 8);

  EXPECT_EQ(ram_get(ram, 0), 1);
  EXPECT_EQ(ram_get(ram, 1), 2);
  EXPECT_EQ(ram_get(ram, 2), 3);
  EXPECT_EQ(ram_get(ram, 3), 4);
  EXPECT_EQ(ram_get(ram, 4), 5);
  EXPECT_EQ(ram_get(ram, 5), 6);
  EXPECT_EQ(ram_get(ram, 6), 7);
  EXPECT_EQ(ram_get(ram, 7), 8);

  ram_destroy(ram);
}

TEST(RamTest, low_address) {
  ram_t *ram = ram_create();
  ASSERT_NE(ram, nullptr);

  ram_set(ram, 512, 158);
  ram_set(ram, 8652, 326);

  EXPECT_EQ(ram_get(ram, 512), 158);
  EXPECT_EQ(ram_get(ram, 8652), 326);

  ram_destroy(ram);
}

TEST(RamTest, high_address) {
  ram_t *ram = ram_create();
  ASSERT_NE(ram, nullptr);

  ram_set(ram, 1147483647, 84852);

  EXPECT_EQ(ram_get(ram, 1147483647), 84852);

  ram_destroy(ram);
}

TEST(RamTest, many_access) {
  ram_t *ram = ram_create();
  ASSERT_NE(ram, nullptr);

  // Creates approximately 37 memory pages
  for (addr_t i = 52; i < 47483647; i += 1284852) {
    ram_set(ram, i, i);
  }

  // Read data back
  for (addr_t i = 52; i < 47483647; i += 1284852) {
    EXPECT_EQ(ram_get(ram, i), i);
  }

  ram_destroy(ram);
}

#ifndef RAM_NO_READ_LISTENER
bool read_listener_1_was_called = false;
bool read_listener_2_was_called = false;
bool read_listener_3_was_called = false;

TEST(RamTest, read_listener) {
  ram_t *ram = ram_create();
  ASSERT_NE(ram, nullptr);

  ram_install_read_listener(ram, 156, 89965,
                            [](auto) { read_listener_1_was_called = true; });

  ram_install_read_listener(ram, 9532, 89965,
                            [](auto) { read_listener_2_was_called = true; });

  ram_install_read_listener(ram, 50, 100, [](auto addr) {
    EXPECT_EQ(addr, 67);
    read_listener_3_was_called = true;
  });

  // Simple test
  read_listener_1_was_called = false;
  read_listener_2_was_called = false;
  read_listener_3_was_called = false;
  ram_get(ram, 8532);
  EXPECT_TRUE(read_listener_1_was_called);
  EXPECT_FALSE(read_listener_2_was_called);
  EXPECT_FALSE(read_listener_3_was_called);

  // Does it work for ram_get_set? Also, do multiple listeners with common
  // regions be called?
  read_listener_1_was_called = false;
  read_listener_2_was_called = false;
  read_listener_3_was_called = false;
  ram_get_set(ram, 9999, 0);
  EXPECT_TRUE(read_listener_1_was_called);
  EXPECT_TRUE(read_listener_2_was_called);
  EXPECT_FALSE(read_listener_3_was_called);

  // Check if it is the correct address that is given to the callback
  read_listener_1_was_called = false;
  read_listener_2_was_called = false;
  read_listener_3_was_called = false;
  ram_get(ram, 67);
  EXPECT_FALSE(read_listener_1_was_called);
  EXPECT_FALSE(read_listener_2_was_called);
  EXPECT_TRUE(read_listener_3_was_called);

  // Out of bounds accesses do not trigger the listener
  read_listener_1_was_called = false;
  read_listener_2_was_called = false;
  read_listener_3_was_called = false;
  ram_get(ram, 5);
  EXPECT_FALSE(read_listener_1_was_called);
  EXPECT_FALSE(read_listener_2_was_called);
  EXPECT_FALSE(read_listener_3_was_called);
  ram_get(ram, 1289965);
  EXPECT_FALSE(read_listener_1_was_called);
  EXPECT_FALSE(read_listener_2_was_called);
  EXPECT_FALSE(read_listener_3_was_called);

  ram_destroy(ram);
}
#endif // !RAM_NO_READ_LISTENER

#ifndef RAM_NO_WRITE_LISTENER
bool write_listener_1_was_called = false;
bool write_listener_2_was_called = false;
bool write_listener_3_was_called = false;

TEST(RamTest, write_listener) {
  ram_t *ram = ram_create();
  ASSERT_NE(ram, nullptr);

  ram_install_write_listener(
      ram, 156, 89965, [](auto, auto) { write_listener_1_was_called = true; });

  ram_install_write_listener(
      ram, 9532, 89965, [](auto, auto) { write_listener_2_was_called = true; });

  ram_install_write_listener(ram, 50, 100, [](auto addr, auto value) {
    EXPECT_EQ(addr, 67);
    EXPECT_EQ(value, 146);
    write_listener_3_was_called = true;
  });

  // Simple test
  write_listener_1_was_called = false;
  write_listener_2_was_called = false;
  write_listener_3_was_called = false;
  ram_set(ram, 8532, 0);
  EXPECT_TRUE(write_listener_1_was_called);
  EXPECT_FALSE(write_listener_2_was_called);
  EXPECT_FALSE(write_listener_3_was_called);

  // Does it work for ram_get_set? Also, do multiple listeners with common
  // regions be called?
  write_listener_1_was_called = false;
  write_listener_2_was_called = false;
  write_listener_3_was_called = false;
  ram_get_set(ram, 9999, 0);
  EXPECT_TRUE(write_listener_1_was_called);
  EXPECT_TRUE(write_listener_2_was_called);
  EXPECT_FALSE(write_listener_3_was_called);

  // Check if it is the correct address that is given to the callback
  write_listener_1_was_called = false;
  write_listener_2_was_called = false;
  write_listener_3_was_called = false;
  ram_set(ram, 67, 146);
  EXPECT_FALSE(write_listener_1_was_called);
  EXPECT_FALSE(write_listener_2_was_called);
  EXPECT_TRUE(write_listener_3_was_called);

  // Out of bounds accesses do not trigger the listener
  write_listener_1_was_called = false;
  write_listener_2_was_called = false;
  write_listener_3_was_called = false;
  ram_set(ram, 5, 0);
  EXPECT_FALSE(write_listener_1_was_called);
  EXPECT_FALSE(write_listener_2_was_called);
  EXPECT_FALSE(write_listener_3_was_called);
  ram_set(ram, 1289965, 0);
  EXPECT_FALSE(write_listener_1_was_called);
  EXPECT_FALSE(write_listener_2_was_called);
  EXPECT_FALSE(write_listener_3_was_called);

  ram_destroy(ram);
}
#endif // !RAM_NO_WRITE_LISTENER
