MLC=ocamlc
PREPROCESSOR=-pp camlp4o
LIBS=dynlink.cma
LANGS=$(wildcard lib/*.ml)
SRC=src
BIN=bin
OBJS=$(BIN)/ast.cmo $(BIN)/parser.cmo $(BIN)/plugin.cmo $(BIN)/main.cmo
EXEC=bin/codegen

all: $(OBJS) $(EXEC) $(addsuffix .cmo, $(basename $(LANGS)))

$(BIN)/%.cmo: $(SRC)/%.ml
	$(MLC) $(PREPROCESSOR) -o $@ -I $(BIN) -c $<

lib/%.cmo: lib/%.ml
	$(MLC) -o $@ -I $(BIN) -c $<

$(EXEC): $(OBJS)
	$(MLC) -o $@ $(LIBS) $^
