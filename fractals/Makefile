.SUFFIXES: .lhs .hi .o
.PHONY: all clean

all:	TextFractals PPMFractals

TextFractals: Fractals.lhs Regions.lhs TextFractals.lhs
	ghc -o TextFractals --make TextFractals.lhs

PPMFractals: Fractals.lhs Regions.lhs PPMFractals.lhs
	ghc -o PPMFractals --make PPMFractals.lhs

clean:
	-rm a.out *.o *.hi PPMFractals TextFractals

