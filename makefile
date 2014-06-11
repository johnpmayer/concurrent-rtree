
all: dist build

dist: concurrent-rtree.cabal
	cabal configure

build:
	cabal build

clean:
	cabal clean
