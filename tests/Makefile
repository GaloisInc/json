ODIR=.ghc

$(ODIR):
	mkdir $(ODIR)

all: $(ODIR)
	ghc -cpp -O QC.hs --make -o QC -no-recomp -i.. -odir=$(ODIR) -hidir=$(ODIR)
	time ./QC
	runhaskell -i.. HUnit.hs

generic:	$(ODIR)
	ghc -i.. --make -fforce-recomp -odir=$(ODIR) -hidir=$(ODIR) GenericTest.hs -o GenericTest
	./GenericTest

clean:
	$(RM) -r $(ODIR)
	$(RM) *.html *.tix QC
