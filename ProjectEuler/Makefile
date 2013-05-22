UTIL_FILES = Prime.hs
SOLUTIONS = $(filter-out $(UTIL_FILES), $(wildcard *.hs))
PROBLEMS_SOLVED = $(SOLUTIONS:%.hs=%)
EXECUTABLES = $(PROBLEMS_SOLVED:%=%.exe)
TESTS       = $(PROBLEMS_SOLVED:%=%.test)

tests: $(EXECUTABLES) $(TESTS)

clean:
	rm -f *.o *.hi *.exe

%.test: %.exe
	@printf "%s %s\n" $* $(shell ./$<)

%.exe: %.hs
	ghc -O3 -o $@ -i.. $<
