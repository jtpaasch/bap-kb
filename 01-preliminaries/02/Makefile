NAME := plugin_01
SRC := $(NAME).ml
PLUGIN := $(NAME).plugin


#####################################################
# DEFAULT
#####################################################

.DEFAULT_GOAL := all

all: clean uninstall install


#####################################################
# THE PLUGIN
#####################################################

.PHONY: clean
clean:
	bapbuild -clean

uninstall:
	bapbundle remove $(PLUGIN)

build: $(SRC) 
	bapbuild  -use-ocamlfind -package findlib.dynload $(PLUGIN)

install: build
	bapbundle update -name "my-plugin-01" $(PLUGIN)
	bapbundle update -desc "My hello world plugin" $(PLUGIN)
	bapbundle install $(PLUGIN)
