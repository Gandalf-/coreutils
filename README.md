# coreutils
Unix core utilities implemented in Haskell

- cat

- wc
```
usage: wc [option]

  -l  lines
  -w  words
  -c  characters
```

- uniq
```
uniq [option ...] [files ...]

  -h     show this help
  -c     prefix lines with the number of occurences
  -u     only show unique lines
  -d     only show repeated lines
  -i     do not consider case while making a match
  -s n   do not compare the first n characters
  -w n   only compare the first n characters
```

- seq
```
usage: seq [last]
           [first] [last]
           [first] [increment] [last]
```
