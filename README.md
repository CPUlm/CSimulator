# simulator_c

## Idée générale :

Afin d'interpréter le fichier `test.net`, le processus suivant est appliqué :

`./netlist_simulator.byte test.net` produit un code c++.

Ce code est compilé par gcc.

Puis le programme est exécuté : `./processor`

## Utilisation :

Il faut dans un premier temps produire `netlist_simulator.byte`, pour cela : `make all`.

Puis `./simulator.sh [args] file.net`.
