# marginaleffects 0.8.1

* `deltamethod()` can run hypothesis tests on objects produced by the `comparisons()`, `marginaleffects()`, `predictions()`, and `marginalmeans()` functions. This feature relies on `match.call()`, which means it may not always work when used programmatically, inside functions and nested environments. It is generally safer and more efficient to use the `hypothesis` argument.
* `plot_cme()` and `plot_cco()` accept lists with user-specified values for the regressors, and can display nice labels for shortcut string-functions like "threenum" or "quartile".
* `posteriordraws`: new `shape` argument to return MCMC draws in various formats, including the new `rvar` structure from the `posterior` package.
* `transform_avg` function gets printed in `summary()` output.
* `transform_post` and `transform_avg` support string shortcuts: "exp" and "ln"
* Added support for `mlm` models from `lm()`. Thanks to Noah Greifer.

Bug fixes:

* `hypothesis` argument with bayesian models and `tidy()` used to raise an error.
* Missing values for some regressors in the `comparisons()` output for `brms` models.

# marginaleffects 0.8.0

Breaking change:

* The `interaction` argument is deprecated and replaced by the `cross` argument. This is to reduce ambiguity with respect to the `interaction` argument in `emmeans`, which does something completely different, akin to the difference-in-differences illustrated in the Interactions vignette.

71 classes of models supported, including the new:

* `rms::ols`
* `rms::lrm`
* `rms::orm`

New features:

* Plots: `plot_cme()`, `plot_cap()`, and `plot_cco()` are now much more flexible in specifying the comparisons to display. The `condition` argument accepts lists, functions, and shortcuts for common reference values, such as "minmax", "threenum", etc.
* `variables` argument of the `comparisons()` function is more flexible:
  - Accepts functions to specify custom differences in numeric variables (e.g., forward and backward differencing).
  - Can specify pairs of factors to compare in the `variables` argument of the `comparisons` function.
* `variables` argument of the `predictions()` function is more flexible:
  - Accepts shortcut strings, functions, and vectors of arbitrary length.
* Integrate out random effects in bayesian `brms` models (see Bayesian analysis vignette)

New vignettes:

* Experiments
* Extending marginal effects
* Integrating out random effects in bayesian models

Bug fixes and minor improvements:

* The default value of `conf_level` in `summary()` and `tidy()` is now `NULL`, which inherits the `conf_level` value in the original `comparisons`/`marginaleffects`/`predictions` calls.
* Fix typo in function names for missing "lnratioavgwts"
* Interactions with `fixest::i()` are parsed properly as categorical variables
* For `betareg` objects, inference can now be done on all coefficients using `deltamethod()`. previously only the location coefficients were available.
* For objects from `crch` package, a number of bugs have been fixed; standard errors should now be correct for `deltamethod()`, `marginaleffects()`, etc.
* Fixed a bug in the `tidy()` function for `glmmTMB` models without random effects, which caused all t statistics to be identical.

# marginaleffects 0.7.1

* New supported model class: `gamlss`. Thanks to Marcio Augusto Diniz.
* `marginalmeans()` accepts a `wts` argument with values: "equal", "proportional", "cells".
* `by` argument 
  - accepts data frames for complex groupings.
  - in `marginalmeans` only accepts data frames.
  - accepts "group" to group by response level.
  - works with bayesian models.
* `byfun` argument for the `predictions()` function to aggregate using different functions.
* `hypothesis` argument
  - The matrix column names are used as labels for hypothesis tests.
  - Better labels with "sequential", "reference", "pairwise".
  - new shortcuts "revpairwise", "revsequential", "revreference"
* `wts` argument is respected in `by` argument and with `*avg` shortcuts in the `transform_pre` argument.
* `tidy.predictions()` and `tidy.marginalmeans()` get a new `transform_avg` argument.
* New vignettes: 
  - Unit-level contrasts in logistic regressions. Thanks to @arthur-albuquerque.
  - Python Numpy models in `marginaleffects`. Thanks to @timpipeseek.
  - Bootstrap example in standard errors vignette.

# marginaleffects 0.7.0

Breaking changes:

* `by` is deprecated in `summary()` and `tidy()`. Use the same `by` argument in the main functions instead: `comparisons()`, `marginaleffects()`, `predictions()`
* Character vectors are no longer supported in the `variables` argument of the `predictions()` function. Use `newdata="fivenum"` or "grid", "mean", or "median" instead.

Critical bug fix:

* Contrasts with interactions were incorrect in version 0.6.0. The error should have been obvious to most analysts in most cases (weird-looking alignment). Thanks to @vmikk. 

New supported packages and models:

* `survival::clogit`
* `biglm`: The main quantities can be computed, but not the delta method standard errors. See https://github.com/vincentarelbundock/marginaleffects/issues/387

New vignette:

* Elasticity
* Frequently Asked Questions

New features:

* Elasticity and semi-elasticity using the new `slope` argument in `marginaleffects()`: eyex, dyex, eydx
* `datagrid()` accepts functions: `datagrid(newdata = mtcars, hp = range, mpg = fivenum, wt = sd)`
* New `datagridcf()` function to create counterfactual datasets. This is a shortcut to the `datagrid()` function with default to `grid_type = "counterfactual"`
* New `by` arguments in `predictions()`, `comparisons()`, `marginaleffects()`
* New `newdata` shortcuts: "tukey", "grid"
* New string shortcuts for `transform_pre` in `comparisons()`
* `marginalmeans()` now back transforms confidence intervals when possible.
* `vcov` argument string shortcuts are now case-insensitive
* The default contrast in `comparisons()` for binary predictors is now a difference between 1 and 0, rather than +1 relative to baseline.
* documentation improvements

# marginaleffects 0.6.0

New supported packages and models:

* `tidymodels` objects of class `tidy_model` are supported if the fit engine is supported by `marginaleffects`.

New function:

* `deltamethod()`: Hypothesis tests on functions of parameters
* `plot_cco()`: Plot conditional contrasts

New arguments:

* `hypothesis` for hypothesis tests and custom contrasts
* `transform_post` in `predictions()`
* `wts` argument in `predictions()` only affects average predictions in `tidy()` or `summary()`.

New or improved vignettes:

* Hypothesis Tests and Custom Contrasts using the Delta Method: https://vincentarelbundock.github.io/marginaleffects/articles/hypothesis.html
* Multiple Imputation: https://vincentarelbundock.github.io/marginaleffects/articles/multiple_imputation.html
* Causal Inference with the g-Formula: https://vincentarelbundock.github.io/marginaleffects/articles/gformula.html
 (Thanks to Rohan Kapre for the idea)

Deprecated or renamed arguments:

* `contrast_factor` and `contrast_numeric` arguments are deprecated in `comparisons()`. Use a named list in the `variables` argument instead. Backward compatibility is maintained.
* The `transform_post` argument in `tidy()` and `summary()` is renamed to `transform_avg` to disambiguate against the argument of the same name in `comparisons()`. Backward compatibility is preserved.

Misc:

* `tidy.predictions()` computes standard errors using the delta method for average predictions
* Support `gam` models with matrix columns.
* `eps` in `marginaleffects()` is now "adaptive" by default: it equals 0.0001 multiplied the range of the predictor variable
* `comparisons()` now supports "log of marginal odds ratio" in the `transform_pre` argument. Thanks to Noah Greifer.
* New `transform_pre` shortcuts: dydx, expdydx
* `tidy.predictions()` computes standard errors and confidence intervals for linear models or GLM on the link scale.

# marginaleffects 0.5.0

Breaking changes:

* `type` no longer accepts a character vector. Must be a single string.
* `conf.int` argument deprecated. Use `vcov = FALSE` instead.

New supported packages and models: 

* `mlogit`
* `mhurdle`
* `tobit1`
* `glmmTMB`

New features:

* `interaction` argument in `comparisons()` to compute interactions between contrasts (cross-contrasts).
* `by` argument in `tidy()` and `summary()` computes group-average marginal effects and comparisons.
* `transform_pre` argument can define custom contrasts between adjusted predictions (e.g., log adjusted risk ratios). Available in `comparisons()`.
* `transform_post` argument allows back transformation before returning the final results. Available in `comparisons()`, `marginalmeans()`, `summary()`, `tidy()`.
* The `variables` argument of the `comparisons()` function accepts a named list to specify variable-specific contrast types.
* Robust standard errors with the `vcov` argument. This requires version 0.17.1 of the `insight` package.
  - `sandwich` package shortcuts: `vcov = "HC3"`, `"HC2"`, `"NeweyWest"`, and more.
  - Mixed effects models: `vcov = "satterthwaite"` or `"kenward-roger"`
  - One-sided formula to clusters: `vcov = ~cluster_variable`
  - Variance-covariance matrix
  - Function which returns a named squared matrix
* `marginalmeans()` allows interactions
* Bayesian Model Averaging for `brms` models using `type = "average"`. See vignette on the `marginaleffects` website.
* `eps` argument for step size of numerical derivative
* `marginaleffects` and `comparisons` now report confidence intervals by default.
* New dependency on the `data.table` package yields substantial performance improvements.
* More informative error messages and warnings
* Bug fixes and performance improvements

New pages on the `marginaleffects` website: https://vincentarelbundock.github.io/marginaleffects/

* Alternative software packages
* Robust standard errors (and more)
* Performance tips
* Tables and plots
* Multinomial Logit and Discrete Choice Models
* Generalized Additive Models
* Mixed effects models (Bayesian and Frequentist)
* Transformations and Custom Contrasts: Adjusted Risk Ratio Example

Argument name changes (backward compatibility is preserved:

* Everywhere:
    - `conf.level` -> `conf_level` 
* `datagrid()`:
    - `FUN.factor` -> `FUN_factor` (same for related arguments)
    - `grid.type` -> `grid_type`

# marginaleffects 0.4.1

New supported packages and models: 

* `stats::loess`
* `sampleSelection::selection`
* `sampleSelection::heckit`

Misc:

* `mgcv::bam` models allow `exclude` argument. 
* Gam models allow `include_smooth` argument. 
* New tests
* Bug fixes

# marginaleffects 0.4.0

New function:

* `comparisons()` computes contrasts

Misc:

* Speed optimizations
* `predictions()` and `plot_cap()` include confidence intervals for linear models
* More robust handling of in-formula functions: factor(), strata(), mo()
* Do not overwrite user's `ggplot2::theme_set()` call

# marginaleffects 0.3.4

* Bug fixes

# marginaleffects 0.3.3

New supported models:

* `mclogit::mclogit`
* `robust::lmRob`
* `robustlmm::rlmer`
* `fixest` confidence intervals in `predictions`

Misc:

* Support `modelbased::visualisation_matrix` in `newdata` without having to specify `x` explicitly. 
* `tidy.predictions()` and `summary.predictions()` methods.
* Documentation improvements.
* CRAN test fixes

# marginaleffects 0.3.2

Support for new models and packages:

* `brglm2::bracl`
* `mclogit::mblogit`
* `scam::scam`
* `lmerTest::lmer`

Misc:

* Drop `numDeriv` dependency, but make it available via a global option:
  options("marginaleffects_numDeriv" = list(method = "Richardson", method.args = list(eps = 1e-5, d = 0.0001)))
* Bugfixes
* Documentation improvements
* CRAN tests

# marginaleffects 0.3.1

documentation bugfix

# marginaleffects 0.3.0

Breaking changes:

* `predictions` returns predictions for every observation in the original
  dataset instead of `newdata=datagrid()`.
* `marginalmeans` objects have new column names, as do the corresponding `tidy`
  and `summary` outputs.

New supported packages and models:

* `brms::brm`
* `rstanarm::stanglm`
* `brglm2::brmultinom`
* `MASS::glmmPQL`
* `aod::betabin`

Misc:

* `datagrid` function supersedes `typical` and `counterfactual` with the `grid.type`
  argument. The `typical` and `counterfactual` functions will remain available
  and exported, but their use is not encouraged.
* `posteriordraws` function can be applied to a `predictions` or a
  `marginaleffects` object to extract draws from the posterior distribution.
* `marginalmeans` standard errors are now computed using the delta method.
* `predictions` standard errors are now computed using the delta method when they are not available from `insight::get_predicted`.
* New vignette on Bayesian models with `brms`
* New vignette on Mixed effects models with `lme4`
* If the `data.table` package is installed, `marginaleffects` will automatically use it to speed things up.
* Contrast definition reported in a separate column of `marginaleffects` output.
* Safer handling of the `type` argument.
* Comprehensive list of supported and tests models on the website.
* Many bug fixes
* Many new tests, including several against `emmeans`

# marginaleffects 0.2.0

Breaking change:

* `data` argument becomes `newdata` in all functions.

New supported packages and models:

* `lme4:glmer.nb`
* `mgcv::gam`
* `ordinal::clm`
* `mgcv`

`marginalmeans`:

* New `variables_grid` argument

`predictions`:

* Support `mgcv`

`plot_cap`

* New `type` argument

Misc:

* New validity checks and tests

# marginaleffects 0.1.0

First release. Bravo!

Thanks to Marco Avina Mendoza, Resul Umit, and all those who offered comments
and suggestions.
