# Sparse Memory and Screen

## API

The project provides two APIs: one for creating RAM and ROM blocks and the other to implement the screen mechanism of
CPUlm.

The first one is defined in `memory.h` and include the following functions:

- For the RAM:
    - `ram_create()`: allocate an "infinite" RAM
    - `ram_from_file()`: allocate an "infinite" RAM initially storing the content of a file
    - `ram_destroy()`: free the RAM
    - `ram_get()`: read a value from the RAM
    - `ram_set()`: write a value into the RAM

- For the ROM:
    - `rom_create()`: create a ROM block from the given data
    - `rom_from_file()`: same as `rom_create()` but read data from a file
    - `rom_destroy()`: free the ROM
    - `rom_get()`: read a value from the ROM

There are other functions more advanced that you can learn about in `memory.h`.

RAM example:

```c
ram_t* ram = ram_create(); // or ram_from_file("input_file.ram")
ram_set(ram, 4523, 563);
assert(ram_get(ram, 4523) == 563);
ram_destroy(ram);
```

ROM example:

```c
rom_t rom = rom_create(ROM_DATA, ROM_SIZE); // or rom_from_file("input_file.rom")
word_t read_value = rom_get(rom, 4523);
// Do something with read_value
rom_destroy(rom);
```

The second API, for the screen, is defined in `screen.h` and include the following functions:

- `screen_init()`: initialize the screen, must be called before any other screen function
- `screen_terminate90`: terminate the screen
- `screen_put_character()`: write a (styled) character to the screen at the given coordinates

Screen example:

```c
screen_init();
// The following code prints "Hello" to the screen with the
// first l being red and the second being green.
screen_put_character(0, 0, 'H');
screen_put_character(1, 0, 'e');
screen_put_character(2, 0, 'l' | (1 << 8));
screen_put_character(3, 0, 'l' | (2 << 8));
screen_put_character(4, 0, 'p');
// Do not forget to call screen_terminate()!
screen_terminate();
```

You can combine both the RAM and screen API to provide a RAM-mapped screen:

```c
ram_t* ram = ram_create();
screen_init_with_ram_mapping(ram); // no need to call screen_init()

ram_set(ram, SCREEN_BASE_ADDRESS, 'H');
// screen_put_character(0, 0, 'H') was called
ram_set(ram, SCREEN_BASE_ADDRESS + 1, 'e');
// screen_put_character(1, 0, 'e') was called
// And so on...

// Do not forget:
screen_terminate();
ram_destroy(ram);
```

However, this feature requires that RAM write listeners are supported. That is,
the macro `RAM_NO_WRITE_LISTENER` must not be defined.

## I want performance

If you want the best performance, you can define the following macros:

- `RAM_NO_READ_LISTENER`, improve RAM reading performance if you don't need RAM read listeners.
- `RAM_NO_WRITE_LISTENER`, improve RAM writing performance if you don't need RAM read listeners.
  **Warning**, the RAM-mapped screen feature requires RAM write listener.
- `DISABLE_SCREEN_STYLING`, disable styling in the screen. The input characters can still be styled,
  but internally no styling preprocessing is done. Improve the performance of the screen.

In the case of the CPUlm simulator, both `RAM_NO_READ_LISTENER` and `DISABLE_SCREEN_STYLING` can
be defined for the best performance (but fewer features).

## Build

The project uses CMake as a build tool. To use it, you can type the following commands in your terminal:

```shell
mkdir build
cd build
cmake ..
# Or cmake .. -DCMAKE_BUILD_TYPE=Debug
# Or cmake .. -DCMAKE_BUILD_TYPE=Release
make
```

You can also run the tests:

```
make test
```
