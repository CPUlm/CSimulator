# CSimulator

CSimulator is a Netlist to C compiler. It accepts NetList files as described in this document. However some restrictions and modifications have been made:
- It does not yet support inputs.
- It only supports a single ROM and/or RAM block.
- Words are represented and interpreted with the most significant bit first.

In addition, since this simulator is intended to simulate the CPUlm, it is capable of handling screen outputs within the terminal. In addition, as requested by the CPUlm Clock program, a word in memory is modified when the system clock advances by one second.

## Usage
To build the NetList to C compiler, use the `make` command. The `csimulator` file will then be generated in the project directory.

To automate NetList debugging, the `make build file=...` recipe can be used. In this case, an executable named `a.out` is built in the `build` directory. This folder contains all the files generated by `csimulator` too.

For more information, please refer to the `csimulator` help.