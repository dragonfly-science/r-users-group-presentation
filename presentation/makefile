all: presentation.pdf

# presentation.tex: presentation.rnw
# 	Rscript -e "library(knitr); knit('presentation.rnw')"

# presentation.pdf: presentation.tex
# 	pdflatex presentation


presentation.pdf: presentation.rnw
	Rscript -e "library(knitr); knit2pdf('presentation.rnw', compiler='xelatex')"

clean:
	rm *.aux *.log *.nav *.out *.pdf *.*~ *.snm presentation.tex *.toc *~ *.vrb
