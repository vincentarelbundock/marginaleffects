# 0.3.0

New:

* **Autodiff support for `predictions()` and `comparisons()`**: JAX-based automatic differentiation now extends to `predictions()` and `comparisons()`. When JAX is installed, standard errors are computed using analytical gradients instead of finite differences, improving both speed and numerical accuracy.

* **Global autodiff configuration**: New `marginaleffects.autodiff()` function to control JAX usage:
  ```python
  import marginaleffects as me
  me.autodiff(False)   # Disable JAX, use finite differences
  me.autodiff(True)    # Force JAX (error if not installed)
  me.autodiff(None)    # Auto-detect (default)
  me.autodiff()        # Inspect current state
  ```
  Can also be set via environment variable `MARGINALEFFECTS_AUTODIFF`.

Internal:

* Model adapters reorganized into package-specific submodules (e.g., `marginaleffects.statsmodels`, `marginaleffects.sklearn`).
* Plot functions moved to `marginaleffects.plot` submodule.
* Refactored `predictions.py` and `comparisons.py` for better code reuse.

# 0.2.3

Breaking changes:

* **String columns are no longer accepted in model formulas.** All categorical variables must be explicitly converted to `Categorical` or `Enum` dtype before fitting models. This ensures consistent categorical level ordering and prevents silent errors where category reference levels could change between model fitting and prediction.

  **What you need to do:** Convert string columns to categorical types before passing data to `fit_statsmodels()`, `fit_sklearn()`, `fit_linearmodels()`, or any other modeling function.

  For Polars DataFrames:
  ```python
  # Option 1: Cast to Categorical (simplest)
  df = df.with_columns(pl.col("region").cast(pl.Categorical))

  # Option 2: Cast to Enum with explicit category order (recommended for control)
  categories = ["<18", "18 to 35", ">35"]  # specify your desired order
  df = df.with_columns(pl.col("age_group").cast(pl.Enum(categories)))
  ```

  For pandas DataFrames:
  ```python
  df["region"] = df["region"].astype("category")
  ```

  **Why this change?** Previously, string columns could be silently converted with unpredictable category ordering (e.g., `"<18"` sorting after `"18 to 35"` lexically), leading to incorrect reference levels and inconsistent results between model fitting and prediction. Explicit conversion ensures you control the category order and get reproducible results.

  **Error message:** If you forget to convert, you'll see a clear error like:
  ```
  TypeError: Column 'region' has String type and is used in the model formula.
  String columns are not allowed in formulas. Please convert to Categorical or Enum before fitting the model.
  ```

Improvements:

* Categorical variable ordering is now preserved consistently across statsmodels and sklearn models. Category orders from pandas categorical columns (statsmodels) and formulaic encoders (sklearn) are respected throughout the prediction pipeline.
* `datagrid()` now correctly preserves Enum and Categorical dtypes when creating reference grids, preventing shape mismatches in design matrices.
* `datagrid()` now supports `None` values to create null columns (similar to R's `NA`). Thanks to @tontief for reporting via email.
* comparisons(model, variables={"x":"all"})` is now supported. Thanks to @tontier for report #230.

# 0.2.2

Improvements:

* `autodiff` module now uses JAX 64-bit precision by default for improved numerical accuracy.

# 0.2.1

Fixes:

* Fixed KeyError in `summary()` method when displaying results with `by` variables that are not in the column mapping. This affected cases where categorical variables like `branch` were used in `by=` argument.

# 0.2.0

New:

* `autodiff` module uses JAX for fast computation of predictions, comparisons, jacobians, and standard errors.
* Model adapters now forward attribute access to underlying fitted models via `__getattr__`, allowing direct access to model-specific attributes and methods.
* `plot_predictions()` gains a `points` argument to overlay raw-data points with controllable transparency.
* `hypothesis=` accepts a list of strings, evaluating each hypothesis in order and stacking the results.
* `comparison=` argument in `comparisons()` now accepts lambda functions to compute custom estimates based on `hi` and `lo` predictions.

Fixes:

* Fixed bug where variables were misidentified due to unordered set from `formulaic.Formula.required_variables`. This caused the outcome variable to be incorrectly included as a predictor and some predictors to be excluded. Issue #221.
* `datagrid(grid_type="counterfactual")` now retains all columns in `newdata`. Issue #1175.
* `hypothesis="ratio ~ ..."` now centers test statistics on 1 instead of 0.
* Allow `hypotheses=` alias in high-level APIs without breaking the signature, and restore hypothesis evaluation for duplicated `term` values while keeping `MarginaleffectsResult` metadata intact when calling DataFrame-like methods.

# 0.1.5

Bugs:

* `fit_sklearn()` sometimes failed due to bad formula parsing.

# 0.1.4

New:

* `datagrid()` gets new arguments: by, response, FUN_categorical, FUN_binary, FUN_numeric, FUN_other
* Improved parsing of categorical variables from formulas. Thanks to @danielkberry for Pull Request #225.

# 0.1.3

Breaking changes:

* PyFixest no longer supports standard errors for `predictions()` (all models) or `comparisons()` and `slopes()` when there is a non-linear link function. See this issue for a discussion of estimation challenges.

Bugs:

* `fit_statsmodels()` preserves parameter names. 
* `get_dataset()` downloads parquet files rather than CSV (faster)


# 0.1.2

* `datagrid(grid_type="counterfactual")` now accepts lambda functions.
* `get_dataset()` no longer requires the `package` argument.
* Group formula hypotheses are now supported `hypothesis="difference~reference|group"`
* `np.*()` is allowed in string/equation `hypothesis`.
* Better printing when `datagrid()` is used to specify column values explicitly.

# 0.1.1

Bugs:

* Allow predictors with missing values with `newdata=None`

New:

* `datagrid()` accepts functions: `datagrid(x = np.mean, y = lambda x: np.quantile(x, [0, .4]))`
* Major refactor and improvement of the `fit_*()` functions.

# 0.1.0

Breaking change:

* `hypothesis="reference"` and friends are deprecated. Use the formula syntax instead: `hypothesis=difference~reference`

New functions: 

* `get_dataset()`
* `fit_sklearn()`, `fit_statsmodels()`, `fit_linearmodels()`

New:

* `datagrid()` gets new arguments: `FUN_other`, `FUN_binary`, `FUN_numeric`, `FUN_character`
* `variables` is available in `avg_predictions()`
* `variables` accepts strings and list of strings in `predictions()`
* Regex supported in `joint_index` argument of `hypotheses()`. Issue #191.
* `comparisons()` allows reverse binary contrast by manually specifying `variables`. Issue #197.

Bugs:

* `datagrid(grid_type='balanced')` takes unique values of binary and categorical variables. Issue #156.
* `datagrid(grid_type='balanced')` does not return duplicates based on response. Issue 169.
* `comparisons(mod, variables="iqr")` and `minmax` now work. Issue #198.

# 0.0.14

* Thanks to Narwhals, marginaleffects can now ingest data frames in multiple formats and convert them to the Polars representation that we need internally. This no longer requires external dependencies like Pandas or DuckDB. Thanks to @artiom-matvei.

# 0.0.13

* Formulas should not include scale() or center(). Thanks to @alexjonesphd for reporting Issue #113.

Breaking change:

* `hypothesis` and `hypothesis` now index in a Python-like style by counting from 0 instead of counting from 1 as in R. Example code before the change  `predictions(mod, hypothesis = "b1 - b2 = 0")`; example correct code after change `predictions(mod, hypothesis = "b0 - b1 = 0")`
 
# 0.0.12

* Bug in datagrid() prevented "balanced" grid type. Thanks to @danielkberry for the fix (PR #104).
* Bug: Missing values leading to ValueError: "Something went wrong" in predictions() (Issue #83)
 
# 0.0.11

* Workaround for upstream regression in Polars.
* Bugfix for p value calculation in equivalence tests. Results could be incorrect.

# 0.0.10

* Polars 0.20.7 introduced a breaking change by error. Pinning version until thi is fixed. https://github.com/pola-rs/polars/issues/14401

# 0.0.9

* Issue #90: Informative error on reserved keyword like 'group'.
* Issue #91: find_variables() in class ModelStatsmodels does not return all variables which causes errors

# 0.0.8

* PyFixest: Fixed effects variables are recognized as categorical by `datagrid()`
* `MarginalEffectsDataFrame` class now has a `jacobian` attribute.

# 0.0.7

Breaking change:

* `datagridcf()` is deprecated. Use `datagrid(grid_type='counterfactual')` instead.

New:

* Support the `PyFixest` package. https://s3alfisc.github.io/pyfixest/
- `datagrid()` no longer requires specifying the `model` argument when called inside another marginaleffects function like `predictions()`.
* `eps_vcov` argument to control the step size in the computation of the Jacobian used for standard errors.
* plot_*() use `plotnine` instead of raw `matplotlib`
* plot_*() `condition` argument gets string shortcuts for numeric variables: "threenum", "fivenum", "minmax".
* `datagrid()` gets a `grid_type` argument: 'mean_or_mode', 'balance', 'counterfactual'
* Plot labels are sorted for clarity and consistency.
* `hypotheses()` function now supports raw models for conducting (non)-linear hypothesis tests on coefficients.

Misc:

* Refactor and several bug fixes in the `plot_*()` functions.
* Many bug fixes.
* Upgraded dependency on the `polars` package, with a shift from `.apply()` to `.map_*()` due to deprecation.
* Removed `pandas` dependency.


# 0.0.6

* `hypothesis` accepts a float or integer to specify a different null hypothesis.
* Better column order in printout when using `datagrid()` or `by`
* Version bump for dependencies.
* Equivalence test bug with duplicated column names.
* Minor bugs in plot_*() with unknown consequences.
* Linting.

# 0.0.5

* `predictions()` supports categorical predictors when `newdata` does not include all levels (internal padding).
* Better sorting of output, using the `by` argument.

# 0.0.4

* New function: `datagridcf()`
* `predictions()` supports categorical predictors when `newdata` does not include all levels (internal padding).

# 0.0.3

Breaking change:

* Rename argument to match `R` `marginaleffects`: `conf_int` -> `conf_level`

Misc:

* `MarginaleffectsDataFrame` class inherits from `pl.DataFrame` for better printing and to host useful attributes.

# 0.0.2

Misc:

* Better step size selection for the numerical derivatives used to compute delta method standard errors.

Bugs:

* When newdata was an unseen dataframe, out.columns would be referenced in sanity.py prior to assignment. Thanks to @Vinnie-Palazeti for PR #25.


# 0.0.1

Initial release
