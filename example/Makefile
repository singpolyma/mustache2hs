showHomePage: showHomePage.hs MustacheTemplates.hs Records.hs
	ghc -Wall -fno-warn-name-shadowing $^ -o $@

MustacheTemplates.hs: homePage.mustache post.mustache
	../dist/build/mustache2hs/mustache2hs -m Records.hs homePage.mustache Blog post.mustache Post > MustacheTemplates.hs
