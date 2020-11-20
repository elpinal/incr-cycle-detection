all: mlkit

test:
	mlkit --output test test.mlb
	./test

mlton:
	mlton -default-ann 'warnUnused true' cycle.mlb

mlkit:
	mlkit --output cycle cycle.mlb

smlnj:
	sml -Ccm.verbose=false cycle.cm

.PHONY: all test mlton mlkit smlnj
