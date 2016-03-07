all: FORCE
	cabal configure --user
	cabal build
	cabal install

test: FORCE
	shelltest --color --execdir test/ -- --threads=16 --hide-successes

doc: FORCE
	cp -r doc/ gen/doc/

FORCE:
