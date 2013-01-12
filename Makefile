GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.2.1

.PHONY: all clean doc install

all: report.html doc dist/build/mustache2hs/mustache2hs dist/mustache2hs-$(VERSION).tar.gz

install: dist/build/mustache2hs/mustache2hs
	cabal install

report.html: mustache2hs.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/mustache2hs/index.html README

README: mustache2hs.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/mustache2hs/index.html: dist/setup-config mustache2hs.hs
	-cabal haddock --hyperlink-source --executables

dist/setup-config: mustache2hs.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc

dist/build/mustache2hs/mustache2hs: mustache2hs.cabal dist/setup-config mustache2hs.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/mustache2hs-$(VERSION).tar.gz: mustache2hs.cabal dist/setup-config README mustache2hs.hs
	cabal check
	cabal sdist
