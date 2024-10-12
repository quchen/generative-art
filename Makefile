# A collection of handy commands as a makefile.

haddock :
	stack haddock generative-art --test --no-run-tests --fast
	@echo "Documentation location: $$(stack path --local-doc-root)/index.html"

doctest :
	stack test generative-art:doctest --fast

testsuite :
	stack test generative-art:testsuite --fast

testall :
	stack test generative-art:testsuite generative-art:doctest --fast

watchtest :
	ghcid --command='stack ghci generative-art:lib generative-art:testsuite --main-is generative-art:testsuite' --test='test "/$(TEST)/"' --warnings --no-title
