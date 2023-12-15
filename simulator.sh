if [ $# -eq 0 ]
then
	echo "No arguments supplied"
else
	./netlist_simulator.byte $@ | g++ -o processor -xc++ -
	./processor
fi
