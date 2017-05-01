#########################
# Makefile
# Simon Goring
#########################

bacon: Baconizing_paper.Rmd
	Rscript -e 'rmarkdown::render("$<")'
	
clean:
	rm -rf *.html *.md *.docx figure/ cache/
