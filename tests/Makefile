ODIR=.ghc

$(ODIR):
	mkdir $(ODIR)

all: $(ODIR)
	ghc -cpp -O QC.hs --make -o QC -no-recomp -odir=$(ODIR) -hidir=$(ODIR)
	time ./QC
	runhaskell HUnit.hs

clean:
	$(RM) -r $(ODIR)
	$(RM) *.html *.tix QC
