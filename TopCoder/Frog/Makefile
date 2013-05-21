INPUTS  = $(wildcard *.in)
OUTPUTS = $(INPUTS:%.in=%.out)

TESTS   = $(INPUTS:%.in=test.%)
TEST_ANSWERS = $(INPUTS:%.in=%.test)

.INTERMEDIATE: $(TEST_ANSWERS)
.PHONY: test all

test: frog.exe $(TESTS)

test.%: %.out %.test
	cmp $*.test $*.out

%.test: %.in
	./frog.exe < $< > $@

frog.exe: Frog.hs
	ghc $< -o $@
