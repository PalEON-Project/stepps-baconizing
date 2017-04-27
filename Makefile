#########################
# Makefile
# Simon Goring
#########################

clean:
	rm -rf *.html *.md *.docx figure/ cache/

bacon: Baconizing_paper.Rmd
	Rscript -e 'rmarkdown::render("$<")'
