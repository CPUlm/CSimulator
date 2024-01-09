all: main

main:
	@dune build bin/main.exe
	@cp -f _build/default/bin/main.exe csimulator

test: main
	@echo "\033[0;34mTests de simulation :\033[0m"
	@bash ./run_tests.sh

clean:
	@rm -rf _build/ csimulator

.PHONY: all main test clean