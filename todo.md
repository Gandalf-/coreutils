# TODO

# Future commands to implement

* jot
* column
* dc
* du
* shuf
* sort
* watch

* expr
* sh
* tail
* telnet

`ls /bin/ /usr/bin/ | grep -o '^....$'`

# Issues

## cut
- [ ] There's currently no efficient way to use `--complement` with multiple fields
  right now. Each line takes O(n^2) time, based on the number of elements in the line
  and the number of fields.

## tr
- [ ] Something unusual going on when `-c -d [:graph:]` on binary files

## which
- [ ] Need tests, not sure how to do this in a platform independent way

## head
- [ ] Doesn't suppport byte suffixes like MB, K

## cat
- [ ] some sort of encoding bug on windows with binary files (.jpg)

## tac
- [ ] eats initial newlines
- [ ] reorganize readBackwards to take a path, will need something like
  https://hackage.haskell.org/package/managed-1.0.8/docs/Control-Monad-Managed.html
  to get it out of IO ()

## seq
- [ ] should try https://hackage.haskell.org/package/text-format
- [ ] super slow, need a faster printf and batching

## nl
- [ ] slow, probably unnecessary bytestring copying

## addrinfo
- [ ] No support for `-s service[/protocol]` syntax
- [ ] Could use more tests for `display`
- [ ] Address:port formatting isn't right, probably difficult to fix though
- [ ] Canonical name prints on every output instead of just the first
- [ ] Socket type display is still different
- [ ] Consider parsing /etc/protocols for parsing and display
- [ ] Error messages are pretty ugly

# Notes

http://www.maizure.org/projects/decoded-gnu-coreutils/index.html

## Profiling
```
stack build --profile --flag coreutils:release
stack exec --profile -- utils cut -c 1,3,4,5- ~/working/pi/ping-data.csv +RTS -p | wc -c
```

## Testing
```
stack test --file-watch --test-arguments --match=/Ls
```

## Parallel Builds

- stack clean
- time make release

Default
```
Executed in   59.93 secs    fish           external
   usr time   57.02 secs   43.00 micros   57.02 secs
   sys time    2.47 secs  353.00 micros    2.47 secs
```

`-j4`
```
Executed in   21.25 secs    fish           external
   usr time   70.56 secs   57.00 micros   70.56 secs
   sys time    8.80 secs  454.00 micros    8.80 secs
```

`-j`
```
Executed in   28.86 secs    fish           external
   usr time   87.54 secs   44.00 micros   87.54 secs
   sys time   17.37 secs  364.00 micros   17.37 secs
```

`-j2`
```
Executed in   35.03 secs    fish           external
   usr time   63.73 secs   50.00 micros   63.73 secs
   sys time    3.63 secs  445.00 micros    3.63 secs
```
