#!/bin/bash

red='\033[0;31m'
noColor='\033[0m'
for netFile in test/*.net; do
	echo $netFile
	for inFile in test/*.in; do
		if [ ${inFile%%.*} = ${netFile%%.*} ]; then
			echo "-   $inFile"
			echo -en "${red}"
			n=$(head -n 1 $inFile)
			./netlist_simulator.byte -n $n $netFile | g++ -o processor -xc++ -
			diff -w -B ${inFile/.in/.out} <(tail -n +2 $inFile | ./processor)
			echo -en "${noColor}"
		fi
	done
done
