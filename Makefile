## YAWN
all: 	repl

configure:
	@cabal configure

build: 	configure
	@cabal build

doc: 	configure
	@cabal haddock --executables

run: 	build
	@./dist/build/yawn/yawn

dist: 	configure
	@cabal sdist

repl:
	ghci -isrc src/Main.hs 

wc:
	@find src -iname "*.hs" | xargs wc -l

clean:
	@cabal clean
	@cd test && cabal clean

## YAWN-TEST 
test-repl:
	@cd test && ghci -i../src:src src/TestMain.hs

test-run:
	@cd test && runhaskell -i../src:src src/TestMain.hs

test-configure:
	@cd test && cabal configure

test-build: test-configure
	@cd test && cabal build
