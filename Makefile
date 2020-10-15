install:
	Rscript -e "devtools::document()"
	R CMD INSTALL .

