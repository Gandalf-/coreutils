all:
	stack build

release:
	stack install --flag coreutils:release


.PHONY: ready test format lint profile
ready: format lint test

test:
	stack test

format:
	stylish-haskell -i */*.hs

lint:
	hlint -j */*.hs

profile:
	stack build --flag coreutils:release --profile
	@echo
	@echo run 'stack exec -- <program> +RTS -p'
