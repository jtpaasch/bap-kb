NAME := plugin_03
PUBLIC_NAME := my-plugin-03
PUBLIC_DESC := My hello world plugin 03

SRC := $(NAME).ml
PLUGIN := $(NAME).plugin
PKGS := -pkgs toy-lib

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
	bapbuild -use-ocamlfind -package findlib.dynload $(PKGS) $(PLUGIN)

install: build
	bapbundle update -name $(PUBLIC_NAME) $(PLUGIN)
	bapbundle update -desc "$(PUBLIC_DESC)" $(PLUGIN)
	bapbundle install $(PLUGIN)
