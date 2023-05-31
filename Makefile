.PHONY: help testall testone document check install deploy

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

deploy: ## pkgdown::deploy_to_branch()
	Rscript -e "pkgdown::deploy_to_branch()"
