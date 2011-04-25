CABAL 	= cabal
EXE 	= dist/build/yawn/yawn

all: 	configure build doc

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

clean:
	@$(CABAL) clean
