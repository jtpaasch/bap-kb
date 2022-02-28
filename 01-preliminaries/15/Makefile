EXE := main.exe


#####################################################
# DEFAULT
#####################################################

.DEFAULT_GOAL := all

all: clean run


#####################################################
# THE EXE
#####################################################

.PHONY: clean
clean:
	dune clean

build:
	dune build ./$(EXE)

run: build
	dune exec ./$(EXE) -- /bin/true
