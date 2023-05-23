# A collection of handy commands as a makefile.

haddock : doctest
	stack haddock generative-art --fast
	@echo "Documentation location: $$(stack path --local-doc-root)/index.html"

doctest :
	stack test generative-art:doctest --fast

testall :
	stack test generative-art:testsuite generative-art:doctest --fast

watchtest :
	ghcid --command='stack ghci generative-art:lib generative-art:testsuite --main-is generative-art:testsuite' --test='test "/$(TEST)/"' --warnings --no-title

artwork-watch :
	ghcid --command='stack ghci generative-art:lib voronoi-postcard:exe:voronoi-postcard --main-is=voronoi-postcard:exe:voronoi-postcard' --test=main --no-title --warnings
