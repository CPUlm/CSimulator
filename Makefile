all: csimulator

csimulator:
	@dune build bin/csimulator.exe
	@cp -f _build/default/bin/csimulator.exe csimulator

test: csimulator
	@echo "\033[0;34mTests de simulation :\033[0m"
	@bash ./run_tests.sh

clean:
	@rm -rf _build/ asm

.PHONY: all csimulator test clean