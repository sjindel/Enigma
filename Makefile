.PHONY: all test lib sim clean

MLOPTS=-codegen native
MLPROF=-profile time

all: lib sim test

test:
	mlton $(MLOPTS) test/test.mlb
	test/test

sim:
	mlton $(MLOPTS) sim/sim.mlb

simprof:
	mlton $(MLOPTS) $(MLPROF) sim/sim.mlb

lib:
	mlton $(MLOPTS) -stop tc lib/eng.mlb

clean:
	rm -rf test/test
	rm -rf sim/sim
