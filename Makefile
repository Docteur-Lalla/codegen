MLC=ocamlc
PREPROCESSOR=-pp camlp4o
SRC=src
BIN=bin
OBJS=$(BIN)/ast.cmo $(BIN)/parser.cmo $(BIN)/main.cmo
EXEC=bin/codegen

all: $(OBJS) $(EXEC)

$(BIN)/%.cmo: $(SRC)/%.ml
	$(MLC) $(PREPROCESSOR) -o $@ -I $(BIN) -c $<

$(EXEC): $(OBJS)
	$(MLC) -o $@ $^
