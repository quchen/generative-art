# A collection of handy commands as a makefile.

haddock : doctest
	stack haddock generative-art --fast
	@echo "Documentation location: $$(stack path --local-doc-root)/index.html"

doctest :
	stack test generative-art:doctest --fast
