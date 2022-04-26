# marginaleffects 0.4.1.9000

Breaking change:

* `type` no longer accepts a character vector. Must be a single string.

New supported packages and models: 

* `mhurdle`
* `tobit1`
* `glmmTMB`

Misc:

* `by` argument in `tidy()` and `summary()` computes group-average marginal effects and comparisons.
* `FUN` argument in `tidy()` and `summary()` to compute mean, median, or arbitrary summaries of unit-level marginal effects.
* Robust standard errors with the `vcov` argument
  - `sandwich` package shortcuts: `vcov = "HC3"`, `"HC2"`, `"NeweyWest"`, and more.
  - Mixed effects models: `vcov = "satterthwaite"` or `"kenward-roger"`
  - One-sided formula to clusters: `vcov = ~cluster_variable`
  - Variance-covariance matrix
  - Function which returns a named squared matrix
* `marginaleffects` and `comparisons` now report confidence intervals by default.
* New dependency on the `data.table` package yields substantial performance improvements.
* New vignette on alternative software packages.
* New vignette on performance.
* Bug fixes and performance improvements

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
