#########################
# Makefile
# Simon Goring
#########################

bacon: Baconizing_paper.Rmd
	Rscript -e 'rmarkdown::render(c("$<"))'
	
clean:
	rm -rf *.html *.md *.docx figure/ cache/
