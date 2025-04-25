# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands
- Build: `stack build`
- Release install: `stack install --flag coreutils:release`
- Format: `stylish-haskell -i */*.hs`
- Lint: `hlint -j */*.hs`

## Test Commands
- Run all tests: `stack test`
- Run integration tests: `bash test/integration/all.sh`
- Run a specific test: `stack test --test-arguments="--match 'pattern'"`

## Code Style
- Use StrictData and RecordWildCards extensions
- 4-space indentation
- Group imports: standard library, external libraries, project imports
- Use qualified imports with aliases (e.g., `qualified Data.ByteString.Char8 as C`)
- Place data types at the top of files with explicit deriving clauses
- Follow naming: CamelCase for types, camelCase for functions/variables
- Use Either String for errors; System.Exit.die for termination
- Follow existing module structure for new utilities
- Document modules with comments describing purpose/features
- Ensure all utilities support -h/--help flags