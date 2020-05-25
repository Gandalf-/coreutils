
# TODO

# Future commands to implement

* column
* dc
* du
* env
* shuf
* sort
* strings
* split
* tee

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

- [X] Argument parsing requires spaces between flags and operators, which is different
  than getopt. For example `-d' '` is invalid.


## tr

- [ ] Allows any range to be used as the second set when transposing characters. GNU tr
  only allows `lower` and `upper`.


## which

- [ ] Should use the library path searching functions for efficiency, idiomaticity
- [ ] Need tests, not sure how to do this in a platform independent way


## head

- [ ] No memory efficient implementation for 'tail' ranges like `-n -5`
    - this is possible, but requires some clever fseek'ing, and is impossible for stdin
- [X] Needs tests


## rev

- [X] Doesn't read files from the argument list
- [X] Should have a couple basic tests


## uniq

- [ ] Needs a rewrite to make it idiomatic


# Notes

## Profiling
```
stack build --profile --flag coreutils:release
stack exec --profile -- utils cut -c 1,3,4,5- ~/working/pi/ping-data.csv +RTS -p | wc -c
```
