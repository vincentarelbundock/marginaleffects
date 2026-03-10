# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Quick Reference

```bash
make test              # Run full test suite (parallel)
make lint              # Run ruff check and format
make install           # Install package with uv
```

## Development Commands

### Setup
```bash
uv venv .venv
source .venv/bin/activate
uv pip install .
```

### Testing
```bash
# Full test suite (runs in parallel with -n auto)
make test

# Single test file
uv run --all-extras pytest tests/test_predictions.py

# Single test function
uv run --all-extras pytest tests/test_predictions.py::test_function_name -v

# With coverage
make coverage
```

### Code Quality
```bash
make lint              # ruff check + format
make precommit         # pre-commit on all files
```

### Documentation
```bash
make qmd               # Extract docstrings into Quarto files
make inject_docs       # Inject minimal docstrings into source files
```

## Architecture Overview

Python package for statistical marginal effects analysis. Provides unified interfaces for predictions, comparisons, and slopes across multiple modeling frameworks. Documentation: https://marginaleffects.com/

### Core API Functions

All exported from `marginaleffects/__init__.py`:
- `predictions()` / `avg_predictions()` - Fitted values
- `comparisons()` / `avg_comparisons()` - Contrasts and differences
- `slopes()` / `avg_slopes()` - Marginal effects (partial derivatives)
- `hypotheses()` - Hypothesis testing
- `datagrid()` - Create reference grids
- `plot_predictions()`, `plot_comparisons()`, `plot_slopes()` - Visualization

### Model Adapters

Files prefixed with `model_` implement the adapter pattern for different modeling libraries:
- `model_abstract.py` - Abstract base class defining the interface
- `model_statsmodels.py` - StatsModels via `fit_statsmodels()`
- `model_sklearn.py` - Scikit-learn via `fit_sklearn()`
- `model_linearmodels.py` - LinearModels via `fit_linearmodels()`
- `model_pyfixest.py` - PyFixest integration

To add support for a new modeling library, implement the abstract interface in `model_abstract.py`.

### Key Design Patterns

1. **Adapter Pattern**: Model adapters provide unified interface across statsmodels, sklearn, etc.

2. **Polars-Based**: Uses Polars DataFrames throughout. `MarginaleffectsDataFrame` (in `classes.py`) extends Polars DataFrame with metadata.

3. **Functional Composition**: `slopes()` composes `comparisons()` with different parameters rather than reimplementing.

### Core Infrastructure

- `uncertainty.py` - Standard errors, jacobians, confidence intervals
- `sanitize_model.py` - Model wrapper/adapter logic
- `by.py` - Grouping/stratification logic
- `transform.py` - Transformations (log, logit, etc.)

### Testing

- Test files in `tests/` mirror source structure
- Reference data from R implementation in `tests/r/`
- Plot regression images in `tests/images/`
- Plot tests are marked slow: `@pytest.mark.plot`
