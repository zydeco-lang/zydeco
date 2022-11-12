LATEXMK ?= latexmk

.PHONY: all
all : formalism.pdf

%.pdf : %.tex
	$(LATEXMK) $*.tex
	mv build/$*.pdf $*.pdf

.PHONY : clean
clean :
	rm -rf build
	rm -f *.pdf
