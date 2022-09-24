# TODO

# Future commands to implement

* column
* dc
* du
* shuf
* sort
* strings
* watch
* sum
* cksum

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
- [ ] slow, could be streaming

## which
- [ ] Need tests, not sure how to do this in a platform independent way

## head
- [ ] Doesn't suppport byte suffixes like MB, K

## uniq
- [ ] Needs a rewrite to make it idiomatic

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

# Notes

## Profiling
```
stack build --profile --flag coreutils:release
stack exec --profile -- utils cut -c 1,3,4,5- ~/working/pi/ping-data.csv +RTS -p | wc -c
```

## Testing
```
stack test --file-watch --test-arguments --match=/Ls
```
