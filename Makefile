all: csimulator
sources  := $(shell find . -regextype sed -regex "\./\(lib\|bin\)/.*\|\./dune-project")

csimulator: $(sources)
	@dune build bin/main.exe
	@cp -f _build/default/bin/main.exe csimulator

clean:
	@rm -rf _build/ csimulator build/

build: csimulator
ifeq ($(file),)
	@echo "Missign file name. Please add 'file=(file name)'"
else
	@echo "Building netlist '$(file)'..."
	@mkdir -p build
	@cp clib/main.c build/
	@cp clib/commons.h build/
	@cp clib/sparse_memory/memory.h build/
	@cp clib/sparse_memory/memory.c build/
	@cp clib/sparse_memory/screen.h build/
	@cp clib/sparse_memory/screen.c build/
	./csimulator $(file) > build/logic.c
	cd build/ && clang -g *.c
endif
dev:
	dune build -w

.PHONY: all csimulator test clean