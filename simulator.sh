./netlist_simulator.byte $@ | g++ -o processor -xc++ -
./processor
