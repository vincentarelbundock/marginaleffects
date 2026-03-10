.PHONY: help

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-22s\033[0m %s\n", $$1, $$2}' | sort

# ==============================================================================
# Combined targets (R + Python in parallel)
# ==============================================================================

install: r-install py-install ## Both: install R and Python packages
test: r-test py-test ## Both: run R and Python test suites
autodiff: r-autodiff py-test-autodiff ## Both: run autodiff tests
document: r-document py-document ## Both: generate docs and populate website/man

# ==============================================================================
# R targets
# ==============================================================================

r-runnersup:
	cd r && awk '!/tinytest/' .Rbuildignore > temp && mv temp .Rbuildignore
	cd r && awk '/^run <- FALSE/{print "run <- TRUE"; next} 1' tests/tinytest.R > temp && mv temp tests/tinytest.R

r-runnersdown:
	cd r && git restore .Rbuildignore
	cd r && git restore tests/tinytest.R

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
	cd r && Rscript -e "devtools::check()"
	$(MAKE) r-runnersdown

r-testone: ## R: run single test (testfile=path)
	cd r && uv run Rscript -e "pkgload::load_all();tinytest::run_test_file('$(testfile)')"

r-testseq: r-runnersup ## R: run all tests sequentially
	cd r && uv run Rscript -e "pkgload::load_all();tinytest::run_test_dir()"
	$(MAKE) r-runnersdown

r-test: r-install r-runnersup ## R: build, install, and test in parallel
	cd r && uv run Rscript -e "tinytest::build_install_test(ncpu = 10)"
	$(MAKE) r-runnersdown

r-testplot: ## R: run plot tests
	$(MAKE) r-testone testfile="inst/tinytest/test-plot_predictions.R"
	$(MAKE) r-testone testfile="inst/tinytest/test-plot_comparisons.R"
	$(MAKE) r-testone testfile="inst/tinytest/test-plot_slopes.R"

r-autodiff: r-uv ## R: run autodiff tests
	$(MAKE) r-testone testfile="inst/tinytest/test-autodiff.R"

r-uv: ## R: clean and rebuild uv environment
	cd r && uv clean && rm -rf .venv && uv sync

# ==============================================================================
# Python targets
# ==============================================================================

py-install: ## Py: install package
	cd python && uv pip install -e .

py-test: py-install ## Py: run pytest suite
	cd python && uv run --all-extras pytest -n auto

py-test-autodiff: py-install ## Py: run pytest with autodiff forced on
	cd python && MARGINALEFFECTS_AUTODIFF=1 uv run --all-extras pytest -n auto

py-lint: ## Py: run ruff linter and formatter
	cd python && uv run --all-extras ruff check marginaleffects
	cd python && uv run --all-extras ruff format marginaleffects
	cd python && uv run --all-extras ruff format tests

py-precommit: ## Py: run pre-commit on all files
	cd python && pre-commit run --all-files

py-benchmark: py-install ## Py: run autodiff benchmark
	cd python && uv run --all-extras python benchmarks/benchmark_autodiff.py

py-snapshot: ## Py: snapshot test
	cd python && R CMD BATCH tests/r/run.R

py-document: ## Py: inject docstrings and populate website/man/python
	@if [ -n "$$(git status --porcelain python/)" ]; then echo "Error: uncommitted changes in python/. Commit or stash first." >&2; exit 1; fi
	cd python && uv run marginaleffects/inject_docs.py
	@mkdir -p website/man/python
	cd python && uv run marginaleffects/docs.py ../website/man/python
	cd python && git checkout -- marginaleffects/
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
