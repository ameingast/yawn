CABAL 	= cabal
REPL 	= ghci
EXE 	= dist/build/yawn/yawn

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
	@$(REPL) Main.hs

wc:
	find Yawn -iname "*.hs" | xargs wc -l

clean:
	@$(CABAL) clean
