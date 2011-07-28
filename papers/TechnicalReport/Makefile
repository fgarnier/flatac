all: *.tex *.eps *.pstex *.pstex_t
	@latex logic.tex && bibtex logic && latex logic.tex && latex logic.tex && dvipdfmx logic.dvi

clean:
	@rm *.aux *.log

mrproper: clean
	@rm logic.dvi logic.pdf
