all :
	./scripts/clean_test_output.sh
	./scripts/stylish-haskell.sh
	stack build
	stack exec haskell-logo-billard
	stack exec haskell-logo-triangles
	stack test || true
	./scripts/normalize_svg.sh
