.PHONY: help testall testone document check install deploy deploydev html pdf news clean website

BOOK_DIR := book

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

runnersup: ## hack local files to run tests
	awk '!/tinytest/' .Rbuildignore > temp && mv temp .Rbuildignore
	awk '/^run <- FALSE/{print "run <- TRUE"; next} 1' tests/tinytest.R > temp && mv temp tests/tinytest.R

runnersdown: ## unhack local files to not run tests
	git restore .Rbuildignore
	git restore tests/tinytest.R


testall: ## tinytest::build_install_test()
	# Rscript -e "pkgload::load_all();cl <- parallel::makeCluster(5);tinytest::run_test_dir(cluster = cl)"
	awk '!/tinytest/' .Rbuildignore > temp && mv temp .Rbuildignore
	Rscript -e "pkgload::load_all();tinytest::run_test_dir()"
	git restore .Rbuildignore

testone: ## make testone testfile="inst/tinytest/test-aaa-warn_once.R"
	Rscript -e "pkgload::load_all();tinytest::run_test_file('$(testfile)')"

testplot: ## test the plots
	make testone testfile="inst/tinytest/test-plot_predictions.R"
	make testone testfile="inst/tinytest/test-plot_comparisons.R"
	make testone testfile="inst/tinytest/test-plot_slopes.R"

document: ## altdoc::render_docs()
	Rscript -e "devtools::document()"

check: document runnersup ## devtools::check()
	Rscript -e "devtools::check()"
	$(MAKE) runnersdown

install: document ## devtools::install(dependencies = FALSE)
	Rscript -e "devtools::install(dependencies = FALSE)"

deps: document ## devtools::install(dependencies = TRUE)
	Rscript -e "devtools::install(dependencies = TRUE)"

news: ## Download the latest changelog
	Rscript -e "source('book/utils/utils.R');get_news()"

pdf: news ## Render the book to PDF
	Rscript -e "source('book/utils/utils.R');get_quarto_yaml(pdf = TRUE)"
	cd $(BOOK_DIR) && quarto render --to pdf && cd ..
	rm -rf $(BOOK_DIR)/NEWS.qmd $(BOOK_DIR)/_quarto.qmd 
	make clean

html: news ## Render the book to HTML
	Rscript -e "source('book/utils/utils.R');get_quarto_yaml(pdf = FALSE, dev = FALSE)"
	cd $(BOOK_DIR) && quarto render --to html && cd ..
	rm -rf $(BOOK_DIR)/NEWS.qmd $(BOOK_DIR)/_quarto.qmd 
	make clean

htmldev: news ## Render the book to HTML
	Rscript -e "source('book/utils/utils.R');get_quarto_yaml(pdf = FALSE, dev = TRUE)"
	cd $(BOOK_DIR) && quarto render --to html && cd ..
	rm -rf $(BOOK_DIR)/NEWS.qmd $(BOOK_DIR)/_quarto.qmd 
	make clean

clean: ## Clean the book directory
	rm -rf $(BOOK_DIR)/NEWS.qmd $(BOOK_DIR)/_quarto.qmd 
	rm -rf ut 

setvar: ## Set the environment variable
	export R_BUILD_DOC=true

buildtest: install runnersup ## Build and test in parallel with 8 cores
	Rscript -e "tinytest::build_install_test(ncpu = 10)"
	$(MAKE) runnersdown

website: setvar ## altdoc::render_docs(verbose = TRUE)
	# Rscript -e "altdoc::render_docs(verbose = TRUE, freeze = TRUE)"
	Rscript -e "reticulate::use_virtualenv('~/.virtualenvs/r-reticulate');altdoc::render_docs(verbose = TRUE, freeze = TRUE)"
