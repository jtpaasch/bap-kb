SRC := main.asm
OBJ := main.o
EXE := main.elf


#####################################################
# DEFAULT
#####################################################

.DEFAULT_GOAL := all
all: clean build


#####################################################
# BUILD
#####################################################

$(EXE): $(SRC)
	nasm -w+all -f elf64 -o $(OBJ) $(SRC)
	ld -e main -o $(EXE) $(OBJ)
	rm -rf $(OBJ)

build: $(EXE)


#####################################################
# CLEAN
#####################################################

.PHONY: clean
clean:
	rm -rf $(OBJ) $(EXE)
