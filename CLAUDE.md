# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`marginaleffects` is an R package that provides a unified interface for computing and plotting predictions, slopes, marginal means, and comparisons (contrasts, risk ratios, odds, etc.) for over 100 classes of statistical and machine learning models. The package supports both unit-level (conditional) and average (marginal) estimates with comprehensive uncertainty quantification.

## Key Commands

### Development
- `make install` - Install package with dependencies=FALSE using devtools
- `make deps` - Install package with all dependencies using devtools  
- `make document` - Generate documentation using devtools::document()
- `make check` - Run R CMD check (includes document step and test runner setup)

### Testing
- `make testall` - Run all tinytest tests (uses pkgload::load_all() then tinytest::run_test_dir())
- `make testone testfile="path/to/test.R"` - Run a single test file
- `make buildtest` - Build, install, and test with parallel processing (10 cores)

### Documentation & Website
- `make website` - Render documentation website using altdoc
- `make html` - Render book to HTML using Quarto
- `make pdf` - Render book to PDF using Quarto

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

## Testing Framework

Uses `tinytest` as the primary testing framework:
- Test files in `inst/tinytest/test-*.R`
- Extensive model archive in `inst/tinytest/modelarchive/` with pre-fitted models
- Visual regression testing via `tinysnapshot` for plots
- Stata comparison tests in `inst/tinytest/stata/` directory
- Custom tinytest extensions: `expect_slopes()`, `expect_predictions()`, `expect_margins()`

Tests are organized by:
- Functionality (`test-predictions.R`, `test-slopes.R`, etc.)
- Model packages (`test-pkg-brms.R`, `test-pkg-lme4.R`, etc.)  
- Special cases (`test-interaction.R`, `test-missing.R`, etc.)

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