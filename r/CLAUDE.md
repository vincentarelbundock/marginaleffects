# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

NEVER CHANGE TESTS UNLESS I ASK YOU TO EXPLICITLY.

## Overview

`marginaleffects` is an R package that provides a unified interface for computing and plotting predictions, slopes, marginal means, and comparisons (contrasts, risk ratios, odds, etc.) for over 100 classes of statistical and machine learning models. The package supports both unit-level (conditional) and average (marginal) estimates with comprehensive uncertainty quantification.

## Key Commands

All commands run from the **repo root** (`/Users/vincent/repos/marginaleffects/`), not from `r/`.

### Development
- `make r-install` - Install package (dependencies=FALSE) via devtools (runs `r-document` first)
- `make r-dependencies` - Install package with all dependencies
- `make r-document` - Generate roxygen docs and populate website/man/r
- `make r-check` - Run R CMD check (includes document step and test runner setup)

### Testing
- `make r-testone testfile="inst/tinytest/test-bugfix.R"` - Run a single test file
- `make r-testseq` - Run all tests sequentially using pkgload::load_all()
- `make r-test` - Build, install, and test in parallel (10 cores)
- `make r-testplot` - Run plot-specific tests (predictions, comparisons, slopes)
- `make r-autodiff` - Run automatic differentiation tests (requires uv environment)

### Documentation & Website
- `make document` - Generate docs for both R and Python, populate website
- `make r-document` - Generate R docs only

### UV Environment (Python/JAX for Autodiff)
- `make r-uv` - Clean and rebuild the uv virtual environment
- The package uses `uv` for Python dependency management in autodiff features
- Test commands (`r-testone`, `r-testseq`, `r-test`) run via `uv run Rscript`

## Architecture

### Core Function Structure
The package centers around four main user-facing functions, each with conditional and marginal variants:

1. **Predictions**: `predictions()` / `avg_predictions()` - Predicted outcomes on specified scales
2. **Comparisons**: `comparisons()` / `avg_comparisons()` - Contrasts between predictions at different regressor values
3. **Slopes**: `slopes()` / `avg_slopes()` - Partial derivatives (marginal effects)
4. **Hypotheses**: `hypotheses()` - Linear and non-linear hypothesis tests

### Model Support Architecture
The package achieves broad model compatibility through a modular approach:

- **`methods_*.R` files**: Each supported model class has its own methods file (e.g., `methods_brms.R`, `methods_lme4.R`) containing:
  - `get_predict()` method for extracting predictions
  - `get_vcov()` method for variance-covariance matrices
  - `set_coef()` method for setting coefficients
  - `get_model_matrix()` method when needed

- **Generic dispatching**: The main functions dispatch to model-specific methods via S3 method system

- **Sanitization layer**: `sanitize_*.R` files handle input validation and standardization across different model types
  - Functions like `sanitize_comparison()`, `sanitize_newdata()`, `sanitize_vcov()` ensure consistent input processing
  - Model-specific sanitization via `sanitize_model_specific.classname()` methods

### Data Flow
1. User calls main function (`predictions()`, `comparisons()`, etc.)
2. Input sanitization via `sanitize_*()` functions
3. Data grid construction via `datagrid()` or `newdata` processing
4. Model-specific method dispatch for predictions/vcov
5. Statistical computation (deltas, jacobians, etc.)
6. Uncertainty quantification via delta method, bootstrap, or simulation
7. Result formatting and attribution

### Key Internal Components
- **`get_*.R` files**: Core computational functions for extracting model information
- **`inferences_*.R` files**: Different approaches to uncertainty quantification (bootstrap, simulation, etc.)
- **`plot_*.R` files**: Visualization functions for each main analysis type
- **`hypothesis_*.R` files**: Hypothesis testing framework supporting matrices, strings, and formulas
- **Comparison functions**: Dictionary of functions for computing different types of contrasts:
  - `difference`, `ratio`, `odds`, `lift` for basic comparisons
  - `dydx`, `eyex`, `eydx`, `dyex` for slopes and elasticities
  - Averaging variants (`*avg`) and weighted variants (`*avgwts`)
- **Autodiff system**: Uses JAX (via reticulate) for automatic differentiation
  - `R/autodiff.R` contains the automatic differentiation interface
  - Falls back to finite differences when autodiff is not supported
  - Requires Python/JAX setup via `uv` for full functionality

## Testing Framework

Uses `tinytest` as the primary testing framework:
- Test files in `inst/tinytest/test-*.R`
- Extensive model archive in `inst/tinytest/modelarchive/` with pre-fitted models
- Visual regression testing via `tinysnapshot` for plots
- Stata comparison tests in `inst/tinytest/stata/` directory
- Custom tinytest extensions: `expect_slopes()`, `expect_predictions()`, `expect_margins()`
- Test helpers in `inst/tinytest/helpers.R` provide utilities like `requiet()` for quiet package loading

Tests are organized by:
- Functionality (`test-predictions.R`, `test-slopes.R`, etc.)
- Model packages (`test-pkg-brms.R`, `test-pkg-lme4.R`, etc.)
- Special cases (`test-interaction.R`, `test-missing.R`, etc.)

## Test-Driven Development

When implementing new features or bug fixes, follow this workflow:

**1. Write the test first**

Add a test to the appropriate file in `inst/tinytest/` before writing any implementation code. For a new feature, use the relevant functionality file (`test-predictions.R`, `test-slopes.R`, etc.) or create `test-pkg-[package].R` for new model support. For a bug fix, add the test to `test-bugfix.R`.

**2. Verify the test fails**

```bash
make testone testfile="inst/tinytest/test-bugfix.R"
```

The test must fail before you write any implementation. If it passes immediately, the test is wrong or the feature already exists.

**3. Implement the minimal fix**

Write the smallest change that makes the test pass. Do not refactor or extend beyond what the test requires.

**4. Verify the test passes**

```bash
make testone testfile="inst/tinytest/test-bugfix.R"
```

**5. Run the full test suite**

```bash
make testseq
```

Ensure no regressions before considering the work done.

**6. Version bump and NEWS**

- Increment the 4th digit of the version number in `DESCRIPTION` (e.g., `0.6.1.0001` → `0.6.1.0002`).
- Add a bullet to `NEWS.md` with the GitHub issue number and thanks to the reporter.

### Writing Good Tests

- Always source helpers and load the package at the top:
  ```r
  source("helpers.R")
  using("marginaleffects")
  ```
- Use `requiet()` for optional package dependencies, not `library()` or `require()`
- Fit models inline using built-in R datasets (`mtcars`, `iris`, `sleep`) whenever possible. Fall back to pre-fitted models from `inst/tinytest/modelarchive/` only when the model class is too expensive to fit inline or requires optional packages that make inline fitting impractical.
- Use the custom expectations when applicable: `expect_slopes()`, `expect_predictions()`, `expect_margins()`
- For new model support, the test file `test-pkg-[package].R` should cover at minimum: `predictions()`, `comparisons()`, and `slopes()` on a simple model

### What Not to Do

- Do not modify existing tests unless explicitly asked
- Do not fit complex models inline when an archived model will do
- Do not write tests that only pass in one execution environment — use the `ON_LOCAL` / `ON_CI` / `ON_CRAN` flags to gate environment-specific behavior

## Model Method Development

When adding support for new model types:

1. Create `methods_[package].R` file with required methods:
   ```r
   get_predict.newmodel <- function(model, newdata, ...) { }
   get_vcov.newmodel <- function(model, ...) { }
   set_coef.newmodel <- function(model, coefs, ...) { }
   ```

2. Add comprehensive tests in `inst/tinytest/test-pkg-[package].R`

3. Consider if special handling needed in sanitization functions

4. Update supported models documentation

The modular architecture means most new model support requires < 50 lines of code.

## Package Structure Notes

### Dependencies and Compilation
- Core dependencies: `data.table`, `insight`, `checkmate`, `rlang`
- Performance: No C++/Rcpp in current version (DESCRIPTION shows no compiled code)
- Extensive suggested packages (100+) for model compatibility testing
- Python integration via `reticulate` for JAX-based automatic differentiation

### Development Workflow
- Uses `devtools` workflow for development (`make install`, `make document`)
- Package check includes automatic test runner setup via `runnersup`/`runnersdown` targets
  - `runnersup`: Modifies .Rbuildignore and tests/tinytest.R to enable tests during check
  - `runnersdown`: Restores original files using git restore
- Uses `altdoc` for documentation website generation with Quarto integration
- Built-in support for both HTML and PDF book rendering
- Python environment managed via `uv` for autodiff features

### Code Organization Conventions
- Model-specific methods follow `get_predict.classname()` pattern
- Test files use `test-pkg-packagename.R` naming for model packages
- Sanitization functions use `sanitize_*()` naming pattern
- Internal utilities use descriptive names (`get_*`, `sanitize_*`, `plot_*`)
- Collate order in DESCRIPTION controls source file loading sequence
