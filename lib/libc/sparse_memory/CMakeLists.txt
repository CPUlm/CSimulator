cmake_minimum_required(VERSION 3.16)

project(SparseMemory)

add_library(SparseMemory memory.c memory.h screen.h screen.c)
target_include_directories(SparseMemory PUBLIC .)

enable_testing()
add_subdirectory(test)
