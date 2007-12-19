ODIR=.ghc

$(ODIR):
	mkdir $(ODIR)

all: $(ODIR)
	ghc -cpp -O QC.hs --make -o QC -fhpc -odir=$(ODIR) -hidir=$(ODIR)
	time ./QC

clean:
	$(RM) -r $(ODIR)
	$(RM) *.html *.tix QC
