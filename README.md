
# The `marginaleffects` package for `R`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/vincentarelbundock/marginaleffects/branch/main/graph/badge.svg)](https://codecov.io/gh/vincentarelbundock/marginaleffects?branch=main)
[![R-CMD-check](https://github.com/vincentarelbundock/marginaleffects/workflows/R-CMD-check/badge.svg)](https://github.com/vincentarelbundock/marginaleffects/actions)
<!-- badges: end -->

This package is still experimental. *Use with caution\!*

## What?

The `marginaleffects` package allows `R` users to compute and plot three
principal quantities of interest for a *wide* variety of models:

  - [*Marginal Effect* (Vignette)](articles/mfx.html)
      - A partial derivative (slope) of the regression/prediction
        equation with respect to a regressor of interest.
  - [*Marginal Mean* (Vignette)](articles/marginalmeans.html)
      - Response predicted by a model for some combination of the
        regressors’ values (a.k.a. “reference grid”), typically their
        means or factor levels.
  - [*Contrast* (Vignette)](articles/contrasts.html)
      - The difference between two Marginal Means, calculated for
        meanginfully different regressor values (e.g., College graduates
        vs. Others).

In scientific practice, the “Marginal Effect” falls in the same toolbox
as the “Contrast.” Both try to answer a counterfactual question: What
would happen to \(y\) if \(x\) were different? They allow us to model
the “effect” of a change/difference in the regressor \(x\) on the
response \(y\).\[1\]

## Why?

To calculate marginal effects we need to take derivatives of the
regression equation. This can be challenging to do manually, especially
when our models are non-linear, or when regressors are transformed or
interacted. Computing the variance of a marginal effect is even more
difficult.

The `marginaleffects` package hopes to do most of this hard work for
you.

Many `R` packages advertise their ability to compute “marginal effects.”
However, most of them do *not* actually compute marginal effects *as
defined above* (the term is ambiguously defined in the statistical
literature and used differently across fields). Instead, they compute
related quantities such as “Estimated Marginal Means” or “Differences in
Predicted Probabilities.” The rare packages that actually compute
marginal effects are typically limited in the model types they support,
and in the range of transformations they allow (interactions,
polynomials, etc.).

The main package in the `R` ecosystem to compute marginal effects is
[the fantastic, trailblazing, and powerful
`margins`](https://cran.r-project.org/web/packages/margins/index.html)
by [Thomas J. Leeper.](https://thomasleeper.com/) The `marginaleffects`
package is (essentially) a clone of `margins`.

So why did I write a clone?

  - *Speed:* In one benchmark (see below), computing unit-level standard
    errors is over 400x faster with `marginaleffects` (1 minute vs. 150
    milliseconds).
  - *Efficiency:* Smaller memory footprint (1.8GB vs 52MB in the same
    example).
  - *Extensibility:* Adding support for new models is very easy, often
    requiring less than 10 lines of new code. In the medium run, the
    goal is to add support for *several* more model types.
  - `ggplot2` support for plotting (conditional) marginal effects.
  - *Tidy:* The results produced by `marginaleffects` follow “tidy”
    principles. They are easy to process and program with.
  - *User interface:* Slight changes to the user interface are intended
    to improve the experience.
  - *Active development*

Downsides of `marginaleffects` include:

  - Weights and simultation-based inference are not (yet) supported.
  - More dependencies.
  - Newer package with a smaller (read: nonexistent) user base.

## How?

By using [the `numDeriv`
package](https://cran.r-project.org/web/packages/numDeriv/index.html) to
compute gradients and jacobians, and [the `insight`
package](https://easystats.github.io/insight/) to extract information
from model objects. That’s it. That’s the secret sauce.

## Supported models

This table shows the list of models supported by `marginaleffect`, and
shows which numerical results have been checked against alternative
software packages: Stata’s `margins` command and R’s `margins` package.

I am *very* eager to add support for new models. Feel free to file a
request on Github or – even better – submit some code.

Warning: When using `marginaleffects` with different models, you will
probably have to adjust the `predict_type` argument. Refer to the
documentation of your modeling package to see what `type` argument is
allowed in the `predict` function.

| Model                | Support: Effect | Support: Std. Errors | Validity: Stata | Validity: Margins |
| :------------------- | :-------------- | :------------------- | :-------------- | :---------------- |
| stats::lm            | x               | x                    | x               | x                 |
| stats::glm           | x               | x                    | x               | x                 |
| aer::ivreg           | x               | x                    | x               | x                 |
| aer::tobit           | x               | x                    |                 |                   |
| betareg::betareg     | x               | x                    | x               | x                 |
| bife::bife           | x               | x                    |                 |                   |
| brglm2::brglmFit     | x               | x                    |                 |                   |
| brglm2::brnb         | x               | x                    |                 |                   |
| estimatr::lm\_robust | x               | x                    |                 |                   |
| fixest::feols        | x               | x                    |                 |                   |
| fixest::feglm        | x               | x                    |                 |                   |
| gam::gam             | x               | x                    |                 |                   |
| geepack::geeglm      | x               | x                    |                 |                   |
| glmx::glmx           | x               | x                    |                 |                   |
| ivreg::ivreg         | x               | x                    | x               | x                 |
| lme4::lmer           | x               | x                    |                 | dydx only         |
| lme4::glmer          | x               | x                    |                 | dydx only         |
| MASS::glm.nb         | x               | x                    |                 | x                 |
| MASS::polr           | x               |                      | x               |                   |
| MASS::rlm            | x               | x                    |                 |                   |
| nlme::gls            | x               | x                    |                 |                   |
| ordinal::clm         | x               |                      |                 | x                 |
| plm::plm             | x               | x                    |                 |                   |
| pscl::hurdle         | x               | x                    |                 |                   |
| pscl::zeroinfl       | x               | x                    |                 |                   |
| rms::lrm             | x               | x                    |                 |                   |
| robustbase::glmrob   | x               | x                    |                 |                   |
| robustbase::lmrob    | x               | x                    |                 |                   |
| speedglm::speedglm   | x               | x                    |                 |                   |
| speedglm::speedlm    | x               | x                    |                 |                   |
| survey::svyglm       | x               | x                    |                 | x                 |
| survival::coxph      | x               | x                    |                 |                   |
| truncreg::truncreg   | x               | x                    |                 |                   |

## Installation

You can install the latest version of `marginaleffects` from Github:

``` r
remotes::install_github("vincentarelbundock/marginaleffects")
```

## Getting started

First, we load the library, download the [Palmer
Penguins](https://allisonhorst.github.io/palmerpenguins/) data from the
[`Rdatasets`
archive](https://vincentarelbundock.github.io/Rdatasets/articles/data.html),
and estimate a GLM model:

``` r
library(marginaleffects)

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)

mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, 
           data = dat, family = binomial)
```

A “marginal effect” is a unit-specific measure of association between a
change in a regressor and a change in the regressand. In most cases, its
value will depend on the values of all the other variables in the model
for that specific unit.

The `marginaleffects` function thus computes a distinct estimate of the
marginal effect and of the standard error for each regressor (“term”),
for each unit of observation (“rowid”). You can view and manipulate the
full results with functions like `head`, as you would with any other
`data.frame`:

``` r
mfx <- marginaleffects(mod)

head(mfx)
#>   rowid     type           term       dydx   std.error large_penguin
#> 1     1 response bill_length_mm 0.01762275 0.007837288             0
#> 2     2 response bill_length_mm 0.03584665 0.011917159             0
#> 3     3 response bill_length_mm 0.08443344 0.021119186             0
#> 4     4 response bill_length_mm 0.03471401 0.006506804             0
#> 5     5 response bill_length_mm 0.05087500 0.013407802             0
#> 6     6 response bill_length_mm 0.01650827 0.007252823             0
#>   bill_length_mm flipper_length_mm species  predicted
#> 1           39.1               181  Adelie 0.05123266
#> 2           39.5               186  Adelie 0.11125087
#> 3           40.3               195  Adelie 0.36919834
#> 4           36.7               193  Adelie 0.10725326
#> 5           39.3               190  Adelie 0.16882994
#> 6           38.9               181  Adelie 0.04782069
```

Notice that the results are presented in “tidy” long format: each row of
the original dataset gets a unique `rowid` value, each unit-level
marginal effect appears on a distinct row, and metadata appears neatly
in separate columns. This makes it easy to operate on the results
programmatically.

1.  The term “effect” is itself tricky. To be clear, this vignette does
    *not* use the word “effect” to imply “causality”.
