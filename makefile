all: FORCE
	cabal configure --user
	cabal build
	cabal install

FORCE:
