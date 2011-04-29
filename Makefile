CABAL 		= cabal
REPL 		= ghci
EXE 		= dist/build/yawn/yawn
SRC_DIR 	= src
TEST_DIR 	= test
MAIN 		= Main.hs
TEST_MAIN 	= Main.hs

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
	@cd $(SRC_DIR) && $(REPL) $(MAIN)

tests:
	@cd $(TEST_DIR) && $(REPL) -i../$(SRC_DIR) $(TEST_MAIN)

wc:
	find $(SRC_DIR) -iname "*.hs" | xargs wc -l

clean:
	@$(CABAL) clean
