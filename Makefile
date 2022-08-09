TOP=TinyBlarneyTest

all: $(TOP)

%: %.hs
	ghc --make -j $<

.PHONY: clean mrproper
clean:
	rm -rf *.o *.hi

mrproper: clean
	rm -f $(TOP)
