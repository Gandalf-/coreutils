GHC = ghc --make -dynamic

all: cat uniq wc

cat: cat.hs
	${GHC} $^ -o $@

uniq: uniq.hs
	${GHC} $^ -o $@

wc: wc.hs
	${GHC} $^ -o $@

.PHONY: clean
clean:
	-rm -f *.hi *.o cat uniq wc
