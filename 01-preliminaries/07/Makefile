PUBLIC_NAME := my-pass-00
PUBLIC_DESC := My demo pass 00

NAME := pass_00
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
	bapbuild -use-ocamlfind -package findlib.dynload $(PLUGIN)

install: build
	bapbundle update -name $(PUBLIC_NAME) $(PLUGIN)
	bapbundle update -desc "$(PUBLIC_DESC)" $(PLUGIN)
	bapbundle install $(PLUGIN)
