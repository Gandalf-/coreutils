# coreutils

Unix core utilities implemented in Haskell.

Goals are:
* idiomatic implementations
* fast, lazy IO
* feature parity with GNU utils
* fully tested
* platform independent

| Utility | Idiomatic | Complete | Fast | Lazy | Tests |
|---------|-----------|----------|------|------|-------|
| awk     | ✓         | 30       |      | ✓    | ✓     |
| cat     | ✓         | ✓        | ✓    | ✓    |       |
| cmp     | ✓         | 50       |      |      | ✓     |
| cut     | ✓         | 90       | 75   | ✓    | 90    |
| dirname | ✓         | ✓        | -    | -    | ✓     |
| echo    | ✓         | ✓        | -    | -    | -     |
| env     | ✓         | ✓        | -    | -    | ✓     |
| head    | ✓         | ✓        | ✓    | ✓    | ✓     |
| nologin | ✓         | ✓        | -    | -    | -     |
| pwd     | ✓         | ✓        | -    | -    | -     |
| rev     | ✓         | ✓        | ✓    | ✓    | ✓     |
| seq     | ✓         | ✓        | ✓    | ✓    | ✓     |
| sleep   | ✓         | ✓        | -    | -    | ✓     |
| split   | ✓         | ✓        | ✓    | ✓    | ✓     |
| sponge  | ✓         | ✓        | ✓    | -    | ✓     |
| tac     | ✓         | 90       | ✓    | ✓    | ✓     |
| tee     | ✓         | ✓        | ✓    | ✓    | ✓     |
| test    | ✓         | ✓        | ✓    | ✓    | ✓     |
| tr      |           | 70       | 40   | 40   | 30    |
| uniq    |           | 80       | 90   | ✓    |       |
| wc      | ✓         | ✓        | ✓    | ✓    |       |
| which   | ✓         | ✓        | -    | -    | -     |
| yes     | ✓         | ✓        | ✓    | ✓    | -     |

| Symbol | Meaning            |
|--------|--------------------|
| ✓      | Fully complete     |
| -      | Not applicable     |
| n%     | % feature complete |

# Usage

This project compiles to a single executable which detects which utility to call
depending on the name of the binary.

```
make release
```
or
```
stack install --flag coreutils:release
```

## Platform Independent

With `utils` on your path, you can call any utility by name, using the name as
the first argument.
```
I leaf@elm ~> utils echo hello | utils rev
olleh
```
```
C:\Users\leaf>utils echo hello | utils rev
olleh
```

This is the best option for trying things out without committing your entire
shell to these implementations. The testing done is mostly thorough, but there's
some crazy usage of the coreutils out in the wild; like `cat - - -` in
build scripts. Who needs to read `stdin` three separate times? The standard
Haskell IO libraries make some sane assumptions that things like this don't
happen, so this project has balance ugly workarounds with idomatic behavior.
Where reasonable, idomatic is preferred. Further, this project relies on the
Haskell IO libraries for platform independence - where they decided to simplify
(permissions, file types), this project does also.

## Linux, FreeBSD, MacOS

If you do want to commit, symlink the utilities you'd like to expose to your `$PATH` variable. If
`~/.local/bin/` has precedence over `/bin/` and `/usr/bin/`, these are the
versions your shell will use.

```
cd ~/.local/bin/
ln -s utils cat
ln -s utils sleep
ln -s utils rev
...
I leaf@elm ~> which which
/home/leaf/.local/bin/which
```

## On Windows

Symlinking doesn't appear to change the name reported by
`System.Environment.getProgName`, so you'll need to create copies of the binary
with different names.

```
PS C:\> cd (Split-Path -Parent (Get-Command utils.exe).Path)
PS C:\Users\leaf\AppData\Roaming\local\bin> cp utils.exe which.exe
PS C:\Users\leaf\AppData\Roaming\local\bin> cd C:\
PS C:\> which calc.exe
C:\WINDOWS\system32\calc.exe
PS C:\> which which.exe
C:\Users\leaf\AppData\Roaming\local\bin\which.exe
```
