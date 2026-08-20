.PHONY: help

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-22s\033[0m %s\n", $$1, $$2}' | sort

# ==============================================================================
# Combined targets (R + Python in parallel)
# ==============================================================================

install: r-install py-install ## Both: install R and Python packages
test: r-test py-test ## Both: run R and Python test suites
document: r-document py-document ## Both: generate docs and populate website/man

# ==============================================================================
# R targets
# ==============================================================================

r-runnersup:
	cd r && awk '!/tinytest/' .Rbuildignore > temp && mv temp .Rbuildignore

r-runnersdown:
	cd r && git restore .Rbuildignore

r-install: r-document ## R: install package (dependencies=FALSE)
	cd r && Rscript -e "devtools::install(dependencies = FALSE)"

r-dependencies: r-document ## R: install package with all dependencies
	cd r && Rscript -e "devtools::install(dependencies = TRUE)"

r-document: ## R: generate roxygen docs and populate website/man/r
	cd r && Rscript -e "devtools::document()"
	@mkdir -p r/altdoc website/man/r && touch r/altdoc/quarto_website.yml
	@Rscript -e 'invisible(sapply(Sys.glob("r/man/*.Rd"), altdoc:::.rd2qmd, "website/man/r", "r"))'
	@rm -rf r/altdoc
	cp -f r/NEWS.md website/bonus/NEWS_r.qmd

r-check: r-document r-runnersup ## R: run R CMD check
	cd r && Rscript -e "devtools::check()"; status=$$?; \
	cd .. && $(MAKE) r-runnersdown; exit $$status

r-testone: ## R: run single test (testfile=path)
	cd r && Rscript -e "pkgload::load_all();r<-tinytest::run_test_file('$(testfile)');print(r);if(any(!sapply(r,isTRUE)))stop('test failures')"

r-testseq: r-runnersup ## R: run all tests sequentially
	cd r && Rscript -e "pkgload::load_all();r<-tinytest::run_test_dir();print(r);if(any(!sapply(r,isTRUE)))stop('test failures')"; status=$$?; \
	cd .. && $(MAKE) r-runnersdown; exit $$status

r-test: r-install r-runnersup ## R: build, install, and test in parallel
	cd r && Rscript -e "tinytest::build_install_test(ncpu = 10)"; status=$$?; \
	cd .. && $(MAKE) r-runnersdown; exit $$status

r-testplot: ## R: run plot tests
	$(MAKE) r-testone testfile="inst/tinytest/test-plot_predictions.R"
	$(MAKE) r-testone testfile="inst/tinytest/test-plot_comparisons.R"
	$(MAKE) r-testone testfile="inst/tinytest/test-plot_slopes.R"

# ==============================================================================
# Python targets
# ==============================================================================

py-install: ## Py: install package
	cd python && uv pip install -e .

py-test: py-install ## Py: run pytest suite
	cd python && uv run --all-extras pytest -n auto

py-lint: ## Py: run ruff linter and formatter
	cd python && uv run --all-extras ruff check marginaleffects
	cd python && uv run --all-extras ruff format marginaleffects
	cd python && uv run --all-extras ruff format tests

py-precommit: ## Py: run pre-commit on all files
	cd python && pre-commit run --all-files

py-snapshot: ## Py: regenerate R reference fixtures in python/tests/r
	cd python && Rscript tests/r/run.R

py-document: ## Py: populate website/man/python from docstrings
	@mkdir -p website/man/python
	cd python && uv run --all-extras python -m marginaleffects.docstrings.qmd ../website/man/python
	@for file in website/man/python/*.qmd; do \
		awk '/^#/ {print $$0 " {.unnumbered}"} !/^#/ {print}' "$$file" > "$$file.tmp" && mv "$$file.tmp" "$$file"; \
	done

py-coverage: ## Py: run tests with coverage
	cd python && pytest --cov=marginaleffects --cov-report=term-missing --cov-report=html tests/

py-build: ## Py: build package
	cd python && uv build

py-publish: py-build ## Py: build and publish package
	cd python && uv publish

py-ipy: ## Py: launch IPython
	cd python && uv run --all-extras ipython --no-autoindent
