all :
	./scripts/clean_test_output.sh
	./scripts/stylish-haskell.sh
	stack build
	stack test
