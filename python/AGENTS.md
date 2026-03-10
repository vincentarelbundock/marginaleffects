# Repository Guidelines

## Project Structure & Module Organization
The Python source lives in `marginaleffects/`, organized by task (e.g., `predictions.py`, `plot_*` helpers, `model_*` adapters). Shared utilities are in `utils.py` and `validation.py`. Tests reside in `tests/`, mirroring module names and including fixtures under `tests/data/` and R interoperability scripts in `tests/r/`. Generated build artifacts and wheels are written to `build/`.

## Build, Test, and Development Commands
- `make install`: install the package into the active `uv` environment for local development.
- `make lint`: run `ruff check` plus `ruff format` on `marginaleffects/` and `tests/`.
- `make test`: execute the full `pytest` suite with extras (`uv run --all-extras pytest`).
- `make snapshot`: refresh R snapshot baselines via `tests/r/run.R`.
- `make coverage`: produce terminal and HTML coverage reports.
- `make readme`: rebuild `README.md` from Quarto sources when docstrings change.

## Coding Style & Naming Conventions
Target Python 3.10+ with four-space indentation, `black`-style line wrapping, and explicit imports. `ruff` enforces lint rules; do not commit failing lint. Modules and functions use `snake_case`, classes use `PascalCase`, and tests mirror their subject (e.g., `test_predictions.py::test_average_marginal_effects`). Prefer type hints on public APIs and short, factual docstrings—`make qmd` extracts them into Quarto docs.

## Testing Guidelines
Use `pytest` with tests collocated under `tests/`. Name scenarios `test_<feature>_<case>` to align with existing files. Mark heavy plotting checks with `@pytest.mark.plot`; exclude them locally via `uv run --all-extras pytest -m "not plot"`. When introducing regression data, add fixtures to `tests/data/`. Regenerate R comparison artifacts with `make snapshot` and ensure new baselines are committed. Aim to keep coverage steady; verify complex additions with `make coverage`.

## Commit & Pull Request Guidelines
Write imperative, single-line commit subjects referencing issues when relevant (`issue #214 fix marginal wrapper`). Aggregate related changes per commit instead of large mixed diffs. Pull requests should outline motivation, summarize behavioral changes, list test evidence (`make test`, targeted pytest invocations), and link tracked issues. Include screenshots only if UI-facing plots change. Before requesting review, run `make lint` and `make test`; pre-commit hooks (`make precommit`) keep formatting consistent.
