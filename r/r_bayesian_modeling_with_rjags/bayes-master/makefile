
all: report.pdf
	evince report.pdf &

report.pdf: report.tex
	pdflatex report.tex

report.tex: report.rnw model_1.rnw model_2.rnw model_3.rnw model_4.rnw priors.rnw modifying.rnw
	Rscript -e "library(knitr); knit('./report.rnw')"

clean:
	-rm *.tex *.aux *.log *.tdo *.toc *.out
	-rm -r figure
	-rm -r cache
