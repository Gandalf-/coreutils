all:
	stack build

release:
	stack install --flag coreutils:release

profile:
	stack build --flag coreutils:release --profile
	@echo
	@echo run 'stack exec -- <program> +RTS -p'
