# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Building & Testing
- `make testall` - Run full test suite using tinytest
- `make testone testfile="path/to/test.R"` - Run single test file
- `make buildtest` - Build and test with parallel execution (10 cores)
- `make check` - Full R CMD check with automatic test enabling/disabling
- `make install` - Install package locally
- `make document` - Generate roxygen2 documentation

### Special Testing Workflow
Tests are **disabled by default** (`run <- FALSE` in `tests/tinytest.R`) to prevent long test runs during development:
- `make runnersup` - Enable tests by modifying `.Rbuildignore` and `tests/tinytest.R`
- `make runnersdown` - Restore files to disable tests
- CI automatically enables tests during builds

### Documentation & Website
- `make website` - Build documentation website using altdoc (not pkgdown)
- `make html` - Render book to HTML using quarto
- `make pdf` - Render book to PDF using quarto

## Architecture Overview

### Core API Functions
The package provides four main functions with consistent interfaces:
1. **`predictions()`** - Outcome predictions at specified predictor values
2. **`comparisons()`** - Contrasts/differences between predictions  
3. **`slopes()`** - Partial derivatives/marginal effects
4. **`hypotheses()`** - Linear/non-linear hypothesis tests

Each has an `avg_*()` variant for averaged effects.

### Model Support System
The package supports 100+ model classes through an extensible S3 method system:
- **Method files**: `R/methods_*.R` (45+ files for different packages)
- **Required methods**: Each model needs `get_predict()`, `get_coef()`, `get_vcov()` implementations
- **Adding support**: Create new `methods_<package>.R` with required S3 methods (often <10 lines)

### Testing Framework
- **Framework**: Uses `tinytest` (not testthat)
- **Location**: `/inst/tinytest/` directory
- **Snapshots**: Uses `tinysnapshot` for visual/output testing
- **Validation**: Numerical results checked against Stata and other R packages when possible

### Performance Components
- **Data processing**: Extensive use of `data.table` for speed
- **C++ code**: `/src/eigen.cpp` for matrix operations using RcppEigen
- **Memory efficiency**: Up to 30x less memory usage vs alternatives

### Core Dependencies
- **Essential**: data.table, insight, checkmate, Rcpp/RcppEigen
- **Model support**: 100+ suggested packages for different model classes
- **Documentation**: altdoc, quarto for website generation

## Development Patterns

### Adding Model Support
1. Create `R/methods_<package_name>.R`
2. Implement required S3 methods: `get_predict()`, `get_coef()`, `get_vcov()`
3. Add model class to appropriate type dictionary entries
4. Add tests in `inst/tinytest/test-pkg-<package_name>.R`

### File Organization
- **Core functions**: `R/predictions.R`, `R/comparisons.R`, `R/slopes.R`, `R/hypotheses.R`
- **Infrastructure**: `R/get_*.R` files for data extraction
- **Validation**: `R/sanitize_*.R` and `R/sanity*.R` files
- **Plotting**: `R/plot_*.R` files for visualizations
- **Utilities**: `R/utils.R`, `R/settings.R`

### Code Style
- Uses roxygen2 for documentation with `@template` system in `/man-roxygen/`
- Follows data.table conventions for performance-critical sections
- S3 method dispatch pattern for model-specific functionality
- Consistent error handling with `checkmate` validation

## Testing Notes

### Test Structure
- Tests are organized by functionality (`test-*.R`) and packages (`test-pkg-*.R`)
- Snapshot testing for plots and output formatting
- Cross-validation against Stata results in `/inst/tinytest/stata/`
- Model archive with pre-fitted models in `/inst/tinytest/modelarchive/`

### Before Submitting Changes
1. Run `make testall` to ensure all tests pass
2. Run `make check` for full R CMD check
3. Verify documentation builds with `make document`
4. Consider impact on supported model classes if changing core functions