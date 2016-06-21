TOOLDOC = $(wildcard doc/tool/*)
DOC = doc/index.md doc/formats.md
SITEDIR = $(HOME)/documents/nbloomf.md/pages/sth/

all: FORCE
	/app/halcyon/halcyon install /home/nathan/code/st-haskell

test: FORCE
	shelltest --color --execdir test/ -- --threads=16 --hide-successes

doc: FORCE
	mkdir -p gen/doc/tool
	for f in $(DOC) $(TOOLDOC); do \
	  cat $$f | sth-import --with "&splice" > gen/$$f; \
	done
	cp -r gen/doc/. $(SITEDIR)
	rm -r gen/

FORCE:
