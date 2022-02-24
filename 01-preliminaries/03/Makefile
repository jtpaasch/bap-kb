NAME := plugin_02
PUBLIC_NAME := my-plugin-02
PUBLIC_DESC := My hello world plugin 02

SRC := $(NAME).ml
PLUGIN := $(NAME).plugin
FLAGS := -I lib


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
	bapbuild -use-ocamlfind -package findlib.dynload $(FLAGS) $(PLUGIN)

install: build
	bapbundle update -name $(PUBLIC_NAME) $(PLUGIN)
	bapbundle update -desc "$(PUBLIC_DESC)" $(PLUGIN)
	bapbundle install $(PLUGIN)
