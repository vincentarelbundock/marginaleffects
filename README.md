`meffects`: Marginal effects using `R`, automatic differentiation, and
the delta method
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

`meffects` stands for “Marginal Effects.”

`meffects` also stands for “Model Effects.”

This package is still experimental. *Use with caution!*

# What is this?

This package is (essentially) a clone of the `margins` package. It
allows users to easily and quickly compute “marginal effects” for a wide
variety of models. A “marginal effect” is a measure of the association
between a change in the regressors and a change in the response
variable. More formally, [the `margins`
vignette](https://cran.r-project.org/web/packages/margins/index.html)
defines “marginal effects” as follows:

> “Marginal effects are partial derivatives of the regression equation
> with respect to each variable in the model for each unit in the data;
> average marginal effects are simply the mean of these unit-specific
> partial derivatives over some sample. In ordinary least squares
> regression with no interactions or higher-order term, the estimated
> slope coefficients are marginal effects. In other cases and for
> generalized linear models, the coefficients are not marginal effects
> at least not on the scale of the response variable. margins therefore
> provides ways of calculating the marginal effects of variables to make
> these models more interpretable.”

# Supported models

| Model            | Support: Effect | Support: Std.Errors | Validity: Stata | Validity: margins |
|:-----------------|:----------------|:--------------------|:----------------|:------------------|
| stats::lm        | ✓               | ✓                   | ✓               | ✓                 |
| stats::glm       | ✓               | ✓                   | ✓               | ✓                 |
| stats::loess     | ✓               |                     |                 | ✓                 |
| AER::ivreg       | ✓               | ✓                   | ✓               | ✓                 |
| betareg::betareg | ✓               | ✓                   | ✓               | ✓                 |
| fixest::feols    | ✓               | ✓                   |                 |                   |
| fixest::feglm    | ✓               | ✓                   |                 |                   |
| ivreg::ivreg     | ✓               | ✓                   | ✓               | ✓                 |
| lme4::lmer       | ✓               | ✓                   |                 | dydx only         |
| lme4::glmer      | ✓               | ✓                   |                 | dydx only         |
| MASS::polr       | ✓               |                     | ✓               |                   |
| ordinal::clm     | ✓               |                     |                 | ✓                 |
| survey::svyglm   | ✓               | ✓                   |                 | ✓                 |
| survey::svyglm   | ✓               | ✓                   |                 | ✓                 |

# Installation

You can install the released version of meffects from Github:

``` r
remotes::install_github("vincentarelbundock/meffects")
```

# Examples

## Logit regression with a multiplicative interaction term

Load the library, simulate data, and estimate a logistic regression
model with interaction terms:

``` r
library(meffects)

N <- 1000
dat <- data.frame(
    x2 = rnorm(N),
    x1 = rnorm(N),
    x3 = rnorm(N),
    x4 = rnorm(N),
    e = rnorm(N))
dat$y <- rbinom(N, 1, plogis(
    dat$x1 + dat$x2 + dat$x3 + dat$x4 + dat$x3 * dat$x4))

mod <- glm(y ~ x1 + x2 + x3 * x4, data = dat, family = binomial)

coef(mod)
#> (Intercept)          x1          x2          x3          x4       x3:x4 
#>  -0.1349112   1.1047398   1.1225511   1.0550986   1.0239286   1.1726812
```

Compute unit-level marginal effects and standard errors:

``` r
mfx <- meffects(mod)
head(mfx)
#>   rowid term        dydx   std.error y         x1        x2         x3
#> 1     1   x1 0.132137375 0.008806117 0 -1.1452110 0.1008357 -0.4426676
#> 2     1   x3 0.106598407 0.013500656 0 -1.1452110 0.1008357 -0.4426676
#> 3     1   x4 0.060380032 0.013165695 0 -1.1452110 0.1008357 -0.4426676
#> 4     1   x2 0.134267859 0.016853016 0 -1.1452110 0.1008357 -0.4426676
#> 5     2   x4 0.004480184 0.028322125 1 -0.6416573 1.9640871 -0.8569794
#> 6     2   x1 0.260973948 0.030577292 1 -0.6416573 1.9640871 -0.8569794
#>           x4
#> 1 -0.1397409
#> 2 -0.1397409
#> 3 -0.1397409
#> 4 -0.1397409
#> 5  1.1287299
#> 6  1.1287299
```

Notice that the results are presented in “tidy” format: each row of the
original dataset gets a unique `rowid` value, each unit-level marginal
effect appears on a distinct row, and metadata appears neatly in
separate columns. This makes it easy to operate on the results
programmatically.

We can obtain similar (but arguably messier) results with the `margins`
package:

``` r
library(margins)

mar <- margins(mod, unit_ses = TRUE)

head(data.frame(mar), 2)
```

## Conditional marginal effects with `ggplot2`

We can use the `newdata` argument to do a “counterfactual” analysis.
Here, we create a new dataset with some factor and logical variables.
Then, we use the `at` argument to specify counterfactual values that we
want to inspect. When a variable is not specified in `at`, its median or
mode is automatically selected:

``` r
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
tmp$gear <- as.factor(tmp$gear)
mod <- lm(mpg ~ hp * wt + am + gear, data = tmp)

meffects(mod, 
          variables = "hp",
          at = list(hp = c(100, 110),
                    gear = c(3, 4),
                    am = TRUE))
```

The nice thing about the `tidy` output format is that we can pipe the
output of the `meffects` function directly to `ggplot2`:

``` r
library(tidyverse)

meffects(mod, 
          variables = "hp",
          at = list(hp = c(100, 110),
                    gear = c(3, 4),
                    am = TRUE))

    mutate(conf.low = dydx - 1.96 * std.error,
           conf.high = dydx + 1.96 * std.error) |>
    ggplot(aes(x = x4, 
               y = dydx, 
               ymin = conf.low, 
               ymax = conf.high)) +
    geom_ribbon(alpha = .1) +
    geom_line() + 
    facet_wrap(~term)
```

# Benchmarks

Here are a couple naive benchmarks to compare the speed of computation
with the `meffects` and `margins` packages. Since unit-level standard
errors can be expensive to compute, we run the benchmarks with and
without standard errors.

## Marginal effects and standard errors (unit-level)

In this naive benchmark, computing marginal effects with their
unit-level standard errors is over 300x faster.

``` r
b1 = bench::mark(
    margins(mod, unit_ses = TRUE),
    meffects(mod, variance = vcov(mod)),
    check = FALSE,
    max_iterations = 3)
b1
```

## Marginal effects only

In this naive benchmark, computing marginal effects *without* their
unit-level standard errors is over 40% faster.

``` r
b2 = bench::mark(
    margins(mod, unit_ses = FALSE),
    meffects(mod, variance = NULL),
    check = FALSE,
    max_iterations = 3)
b2
```
