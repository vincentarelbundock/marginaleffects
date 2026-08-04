# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Quick Reference

```bash
# From repo root (preferred — handles install before test):
make py-test           # Install + run full test suite (parallel)
make py-lint           # ruff check + format
make py-install        # Install package (editable)

# From python/ directory:
uv run --all-extras pytest tests/test_predictions.py              # Single test file
uv run --all-extras pytest tests/test_predictions.py::test_name -v  # Single test
uv run --all-extras ruff check marginaleffects                    # Lint only
uv run --all-extras ruff format marginaleffects tests             # Format only
```

## Setup

```bash
uv venv .venv
source .venv/bin/activate
uv pip install -e .          # Editable install
uv pip install -e ".[test]"  # With test dependencies
```

## Architecture Overview

Python package for statistical marginal effects analysis. Unified interface for predictions, comparisons, and slopes across modeling frameworks. Docs: https://marginaleffects.com/

### Core Pipeline

The three main functions (`predictions()`, `comparisons()`, `slopes()`) share a common flow:

1. **Input sanitization** (`utils.py:prepare_base_inputs`, `sanitize/`) — wraps raw model via `sanitize_model()`, validates `newdata`/`by`/`vcov`/`hypothesis`
2. **Counterfactual computation** — builds modified data grids and computes estimates, capturing a reusable plan (`planning/`)
3. **Uncertainty** (`inference/`) — delta method for standard errors, with the Jacobian obtained from the first path that supports the plan: analytic (`inference/analytic.py`), automatic differentiation (`autodiff/`, opt-in), then forward finite differences (`inference/delta.py`)
4. **Result wrapping** (`classes/result.py:MarginaleffectsResult`) — dataclass wrapping a Polars DataFrame with metadata (conf_level, jacobian, column mapping, print formatting)

`slopes()` delegates entirely to `comparisons()` with `slope`-specific parameters — it does not reimplement the logic.

### Model Adapters

Each supported modeling library lives in its own subdirectory with a `model.py`:
- `statsmodels/` — auto-detected by class name; supports OLS, GLM, MixedLM, MNLogit, OrderedModel, QuantReg, etc.
- `sklearn/` — requires explicit `fit_sklearn()` wrapper (needs formula + data stored in vault)
- `linearmodels/` — requires explicit `fit_linearmodels()` wrapper
- `pyfixest/` — auto-detected by class name

Auto-detection happens in `sanitize_model.py`. Sklearn and linearmodels can't be auto-detected because they don't store formula/data, so they use `fit_*()` functions that create the adapter with a "vault" dict holding `coef`, `vcov`, `modeldata`, `formula`, `variables_type`, etc.

All adapters inherit from `ModelAbstract` (`classes/model.py`), which provides the vault-based accessor interface (`get_coef()`, `get_vcov()`, `get_modeldata()`, `find_variables()`, etc.). The `ModelAdapter` Protocol in the same file documents the methods estimation code is allowed to call. Adapters do **not** proxy unknown attributes to the fitted object; use `get_fitted_model()` for engine-specific access.

### Key Infrastructure

- `estimands.py` — defines comparison functions (difference, ratio, etc.). `planning/core.py:_builtin_comparison` mirrors these in numpy for fast replay; the two must stay in sync (`tests/test_planning_estimands.py`)
- `planning/` — immutable prediction/comparison plans and coefficient-to-estimand replay
- `inference/` — Jacobians (analytic, finite-difference), delta-method standard errors, test statistics
- `datagrid.py` — creates reference grids for evaluation points
- `by.py` — grouping/stratification of results
- `hypothesis_compile.py` / `test/` — hypothesis testing, joint tests, equivalence
- `transform.py` — response transformations (log, logit, etc.)
- `classes/` — `MarginaleffectsResult` and model classes; `MarginaleffectsDataFrame` is a deprecated alias for `MarginaleffectsResult`
- `docstrings/` — docstring template system with `{param_*}` placeholders
- `plan.py` and `uncertainty.py` are compatibility façades over `planning/` and `inference/`; import from the packages in new code

### Data Flow

- Uses **Polars** DataFrames throughout internally
- Accepts any Arrow-compatible input via `utils.py:ingest()` (uses `__arrow_c_stream__` protocol)
- Input validation uses **pydantic** `@validate_call` decorators
- Formula parsing via **formulaic** (default) or patsy

### Testing

- Tests in `tests/` correspond to source modules and model types
- R reference data in `tests/r/` — Python results are compared against R `marginaleffects` output
- `tests/helpers.py` and `tests/utilities.py` provide test comparison helpers
- Plot tests use image regression: `@pytest.mark.plot`, images in `tests/images/`
- Generate R snapshots: `make py-snapshot` (runs `tests/r/run.R`)
- Autodiff tests: `make py-test-autodiff` (sets `MARGINALEFFECTS_AUTODIFF=1`)
