# simulator_c

## Build

To compile a Netlist `file.netlist`, just type the following commands:
```
./csimulator file.netlist > file.c
gcc file.c -I SparseMemory SparseMemory/memory.c SparseMemory/screen.c
```

You can add `-g` to the GCC command so debugging data is also emitted. You
can also add `-O2` (or `-O3`) to GCC to optimize the C code.

## Idée générale :

Afin d'interpréter le fichier `test.net`, le processus suivant est appliqué :

`./netlist_simulator.byte test.net` produit un code c++.

Ce code est compilé par gcc.

Puis le programme est exécuté : `./processor`

## Utilisation :

Il faut dans un premier temps produire `netlist_simulator.byte`, pour cela : `make all`.

Puis `./simulator.sh [args] file.net`.
