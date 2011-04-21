all:
	cabal configure
	cabal build

run:
	./dist/build/yawn/yawn

clean:
	cabal clean
