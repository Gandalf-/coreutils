# Haskell Coreutils

BSD coreutils reimplemented in Haskell. ~40 utilities in a single dispatch binary.

## Build & Dev Commands
- `make all` — build (no optimization)
- `make release` — build + install with `-O2 -threaded`
- `make test` — unit tests + integration tests
- `make unittest` — unit tests only (`stack test`)
- `make format` — `stylish-haskell -i */*.hs`
- `make lint` — `hlint -j */*.hs`
- `make ready` — format, lint, then test
- `make profile` — build with profiling; run with `stack exec -- <prog> +RTS -p`
- Run one test: `stack test --test-arguments="--match 'pattern'"`
- `sghci <file> '<ghci commands>'` — non-interactive ghci via stack (e.g. `sghci Coreutils/Nl.hs ':t process'`)
- `stack unpack <package> --to /tmp/haskell-src` — download package source (with haddock docs inline) for reference

## Toolchain
- Stack, resolver LTS 24.24 (GHC 9.8.3)
- Formatter: stylish-haskell
- Linter: hlint

## Architecture
- `Coreutils/Util.hs` — `Util` typeclass + existential `Utility` wrapper for dispatch
- `src/main.hs` — entry point; HashMap-based dispatch by program name or first arg
- Each utility lives in `Coreutils/<Name>.hs` with a data type implementing `Util`
- Library source is at the repo root (`Coreutils/`), not under `src/`

## Adding a New Utility
1. Create `Coreutils/<Name>.hs` — define a data type, implement `Util`, write main logic
2. Add import + entry to the dispatch table in `src/main.hs`
3. Add `test/<Name>Spec.hs` for unit tests
4. Optionally add `test/integration/<name>.sh`
5. See `Coreutils/Rev.hs` as a minimal template

## Common Patterns
- **Arg parsing**: `System.Console.GetOpt` with `foldM` over a default options record (see `Coreutils/Head.hs`)
- **Errors**: `Either String` for recoverable errors; `System.Exit.die` for fatal
- **Streaming I/O**: `streaming-bytestring` for constant-memory file processing (see `Coreutils/Cat.hs`, `Coreutils/Rev.hs`)
- **Qualified aliases**: `C` = `Data.ByteString.Char8`, `Q` = `Streaming.ByteString.Char8`, `S` = `Streaming.Prelude`

## Code Style
- Default extensions: `StrictData`, `RecordWildCards`
- Also common: `OverloadedStrings`, `BangPatterns`, `LambdaCase`
- 4-space indentation
- Import grouping: standard library, then external packages, then project (`Coreutils.*`)
- Use qualified imports with short aliases
- Run `make format` then `make lint` before committing
- All utilities must support `-h`/`--help`

## Testing
- **Unit**: hspec with `hspec-discover`; one file per utility at `test/<Name>Spec.hs`
- **Integration**: bash scripts in `test/integration/`; compare output against system utilities
- Test helpers in `test/integration/common.sh` — `compare()`, `expect()`, `expect-not()`
- Name test functions `ptest_*()` for parallel execution
