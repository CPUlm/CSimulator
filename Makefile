netlist_simulator.byte: src/netlist_ast.ml src/netlist_lexer.mll src/netlist.ml src/netlist_parser.mly src/netlist_printer.ml src/netlist_to_c.ml src/scheduler.ml src/graph.ml src/netlist_simulator.ml
	ocamlbuild src/netlist_simulator.byte -use-menhir

tests: netlist_simulator.byte
	@echo "\033[0;34mTests de simulation :\033[0m"
	@./run_tests.sh


miniTest: netlist_simulator.byte
	./simulator.sh -n 5 test.net

all: netlist_simulator.byte


	
clean:
	ocamlbuild -clean
	rm -f processor
