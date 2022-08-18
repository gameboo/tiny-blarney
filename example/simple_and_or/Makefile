TOP=Main

all: $(TOP)

%: %.hs
	ghc --make -i../../src -hidir hidir -odir odir -j $<

.PHONY: clean mrproper
clean:
	rm -rf hidir odir

mrproper: clean
	rm -f $(TOP)
