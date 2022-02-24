LIB := toy-lib


#####################################################
# DEFAULT
#####################################################

.DEFAULT_GOAL := all

.PHONY: all
all: clean uninstall build install


#####################################################
# CLEAN
#####################################################

.PHONY: clean
clean:
	dune clean


#####################################################
# BUILD
#####################################################

build: $(LIB_SRC_FILES)
	dune build -p $(LIB) @install
	@echo "" # Force a newline in terminal output


#####################################################
# INSTALL
#####################################################

install: build
	dune install

uninstall: build
	dune uninstall
