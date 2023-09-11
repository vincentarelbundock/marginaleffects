.PHONY: help testall testone document check install deploy deploydev html pdf news clean

BOOK_DIR := book

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

testall: ## tinytest::build_install_test()
	Rscript -e "tinytest::build_install_test(ncpu = 8)"

testone: ## make testone testfile="inst/tinytest/test-aaa-warn_once.R"
	Rscript -e "pkgload::load_all();tinytest::run_test_file('$(testfile)')"

document: ## devtools::document()
	Rscript -e "devtools::document()"

check: ## devtools::check()
	Rscript -e "devtools::check()"

install: ## devtools::install()
	Rscript -e "devtools::install()"

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

deploy: ## Deploy book to Github website
	git fetch origin
	git checkout -B gh-pages origin/gh-pages
	git pull
	git checkout main
	git pull
	git stash
	git checkout gh-pages
	git checkout main -- book
	git checkout main -- Makefile
	Rscript -e "source('book/utils/utils.R');get_reference()"
	Rscript -e "source('book/utils/utils.R');link_function_docs()"
	Rscript -e "source('book/utils/utils.R');get_quarto_yaml(dev = FALSE)"
	make html
	rsync -a book/_book/* ./
	rsync -a book/data ./
	rm -rf book Makefile _quarto.yml utils.R
	git add .
	git commit -m "Update book"
	git push
	git checkout main

deploydev: ## Deploy dev book to Github website
	git stash
	git fetch origin
	git checkout -B gh-pages origin/gh-pages
	git pull
	git checkout main
	git pull
	git stash
	git checkout gh-pages
	git checkout main -- book
	git checkout main -- Makefile
	Rscript -e "source('book/utils/utils.R');get_reference()"
	Rscript -e "source('book/utils/utils.R');link_function_docs()"
	Rscript -e "source('book/utils/utils.R');get_quarto_yaml(dev = TRUE)"
	make htmldev
	rsync -a book/_book/* ./dev/
	rsync -a book/data ./
	rm -rf book Makefile _quarto.yml utils.R
	git add .
	git commit -m "Update book"
	git push
	git checkout main