.PHONY: all

all: bench1 bench2

bench1: bench1.hs MustacheTemplates.hs Records.hs
	ghc -Wall -fno-warn-name-shadowing $^ -o $@

bench2: bench2.hs Records.hs
	ghc -Wall -fno-warn-name-shadowing $^ -o $@

MustacheTemplates.hs: toplevel.mustache sublevel.mustache
	../dist/build/mustache2hs/mustache2hs -m Records.hs toplevel.mustache TopLevel sublevel.mustache SubLevel > MustacheTemplates.hs
