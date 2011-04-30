CABAL      = cabal
RUNHASKELL = runhaskell
GHCI       = ghci

all: 	$(DEFAULT)

configure:
	@$(CABAL) configure

build: 	configure
	@$(CABAL) build

doc: 	configure
	@$(CABAL) haddock --executables

dist: 	configure
	@$(CABAL) sdist

run: 	build
	@./dist/build/$(TARGET)/$(TARGET)

runhaskell:
	@$(RUNHASKELL) -i$(INCLUDE) $(MAIN) 

repl:
	@$(GHCI) -i$(INCLUDE) $(MAIN)

wc:
	@find src -iname "*.hs" | xargs wc -l

clean:
	@$(CABAL) clean
