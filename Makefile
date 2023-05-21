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
