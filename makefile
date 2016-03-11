TOOLDOC = $(wildcard doc/tool/*)
DOC = doc/index.md doc/formats.md

all: FORCE
	cabal configure --user
	cabal build
	cabal install

test: FORCE
	shelltest --color --execdir test/ -- --threads=16 --hide-successes

doc: FORCE
	for f in $(DOC) $(TOOLDOC); do \
	  cat $$f | sth-import --with "&splice" > gen/$$f; \
	done

FORCE:
