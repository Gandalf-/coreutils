all:
	stack build

release:
	stack install --flag coreutils:release


.PHONY: test format lint profile
test:
	stack test

format:
	stylish-haskell -i */*.hs

lint:
	hlint */*.hs

profile:
	stack build --flag coreutils:release --profile
	@echo
	@echo run 'stack exec -- <program> +RTS -p'
