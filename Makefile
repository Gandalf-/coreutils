all:
	stack build

build-release:
	stack build --flag coreutils:release 2>&1 | grep -v relocation

release:
	stack install --flag coreutils:release

profile:
	stack build --flag coreutils:release --profile
	@echo
	@echo run 'stack exec -- <program> +RTS -p'
