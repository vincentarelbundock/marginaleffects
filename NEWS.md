# marginaleffects 0.2.0.9000

Breaking change:

* `predictions` returns predictions for every observation in the original dataset instead of `newdata=typical()`.

Support for new models and packages:

* `brms::brm`
* `rstanarm::stanglm`
* `brglm2::brmultinom`
* `aod::betabin`

New function:

* `datagrid` supersedes `typical` and `counterfactual` with the `grid.type`
  argument. The `typical` and `counterfactual` functions will remain available
  and exported, but their use is not encouraged.

Documentation:

* Vignette on Bayesian models with `brms`
* Vignette on Mixed effects models with `lme4`

Performance: 

* If the `data.table` package is installed, `marginaleffects` will automatically use it to speed things up.

Bugs:

* Inverted logical contrasts
* Erroneous label in `plot_cme`

# marginaleffects 0.2.0

Breaking change:

* `data` argument becomes `newdata` in all functions.

`marginaleffects`:

* Support `lme4:glmer.nb`
* Support `mgcv::gam`
* Support `ordinal::clm`
* Support `mgcv`

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
