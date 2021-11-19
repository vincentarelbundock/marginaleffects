# marginaleffects 0.2.0.9000

Breaking change:

* `predictions` returns predictions for every observation in the original dataset instead of `newdata=typical()`.

Support for new models and packages:

* `brms`
* `rstanarm`

Misc:

* New vignette on Bayesian models
* Bugfix: inverted logical contrasts
* Bugfix: erroneous label in `plot_cme`

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
