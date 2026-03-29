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

## TDD Workflow

When adding features or fixing bugs, use strict test-driven development:

### Red — write a failing test that shows the behavior gap
1. **High-level test first.** In `test/<Name>Spec.hs`, write an integration-
   style test exercising the missing/broken behavior. Use existing helpers
   (e.g. `run`, `test`) so the test reads as a spec. The test must **run and
   produce wrong output** — a compile error doesn't count.
2. **Stub to compile if needed.** Add types, constructors, or record fields
   with defaults. Use `func :: Type; func = undefined` for functions that
   don't exist yet. All *existing* tests must still pass; only the new test
   should be red.
3. **Low-level unit tests.** Write focused tests for the new pure logic
   (matchers, formatters, state transitions). These also fail.

### Green — write the minimal code to pass
4. Implement just enough to make every test green. Follow existing patterns.

### Refactor — clean up with confidence
5. With all tests green, simplify, extract helpers, improve names.
   `make ready` (format, lint, test) must pass before committing.

### Rules
- One behavioral change per red-green-refactor cycle.
- A "failing test" means wrong output, not a compile error. Compile errors
  block *all* tests and give no signal about the behavior under test.
- Prefer pure-function unit tests over IO tests. Use integration tests
  (`test/integration/`) only for IO-heavy features (file args, handles).
- Run the full test suite (`make test`) after each green step.
