CABAL 	= cabal
REPL 	= ghci
EXE 	= dist/build/yawn/yawn
MAIN 	= Main.hs
SRC_DIR = src

all: 	repl

configure:
	@$(CABAL) configure

build: 	configure
	@$(CABAL) build

doc: 	configure
	@$(CABAL) haddock --executables

run: 	build
	@./$(EXE)

dist: 	configure
	@$(CABAL) sdist

profile:
	@$(CABAL) configure -p
	@$(CABAL) build

repl:
	@$(REPL) -i$(SRC_DIR) $(SRC_DIR)/$(MAIN)

wc:
	find $(SRC_DIR) -iname "*.hs" | xargs wc -l

clean:
	@$(CABAL) clean
