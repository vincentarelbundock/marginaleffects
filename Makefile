.PHONY: readme test help install qmd benchmark

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

precommit: ## run pre-commit on all files 
	pre-commit run --all-files

test: install ## run pytest suite
	uv run --all-extras pytest -n auto

test-autodiff: install ## run pytest suite with autodiff forced on
	MARGINALEFFECTS_AUTODIFF=1 uv run --all-extras pytest -n auto

benchmark: install ## run autodiff benchmark
	uv run --all-extras python benchmarks/benchmark_autodiff.py

snapshot: ## snapshot test
	R CMD BATCH tests/r/run.R

readme: ## render Quarto readme
	poetry run quarto render docs/get_started.qmd --to gfm
	mv -f docs/get_started.md README.md

lint: ## run the lint checkers
	uv run --all-extras ruff check marginaleffects
	uv run --all-extras ruff format marginaleffects
	uv run --all-extras ruff format tests

install: ## install in poetry venv
	uv pip install -e .

qmd: ## extract docstrings into quarto files
	uv run marginaleffects/docs.py

coverage:
	pytest --cov=marginaleffects --cov-report=term-missing --cov-report=html tests/

qmd_local:
	uv pip install -e . && uv run marginaleffects/docs.py

inject_docs:
	uv run marginaleffects/inject_docs.py 
	make lint

build:
	uv build

ipy:
	uv run --all-extras ipython --no-autoindent

publish: build
	uv publish
