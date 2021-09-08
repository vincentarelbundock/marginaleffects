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

# Getting started

First, we load the library, download data from the [`Rdatasets`
archive](https://vincentarelbundock.github.io/Rdatasets/articles/data.html),
and estimate a Poisson GLM model:

``` r
library(meffects)

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/COUNT/affairs.csv")
mod <- glm(naffairs ~ kids + vryunhap, data = dat, family = poisson)
```

The `meffects` function computes a distinct estimate of the marginal
effect and of the standard error for each regressor (“term”), for each
unit of observation (“rowid”). You can browse view and manipulate the
full results with functions like `head`, as you would any `data.frame`:

``` r
mfx <- meffects(mod)

head(mfx)
#>   rowid     term      dydx  std.error naffairs kids vryunhap
#> 1     1     kids 0.5192665 0.04476208        0    0        0
#> 2     1 vryunhap 0.8653523 0.13151255        0    0        0
#> 3     2     kids 0.5192665 0.04476208        0    0        0
#> 4     2 vryunhap 0.8653523 0.13151255        0    0        0
#> 5     3 vryunhap 0.8653523 0.13151255        3    0        0
#> 6     3     kids 0.5192665 0.04476208        3    0        0
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
mar <- data.frame(mar)

head(mar, 2)
#>   X naffairs kids vryunhap unhap avgmarr hapavg vryhap antirel notrel slghtrel
#> 1 1        0    0        0     0       0      1      0       0      0        1
#> 2 2        0    0        0     0       0      1      0       0      0        0
#>   smerel vryrel yrsmarr1 yrsmarr2 yrsmarr3 yrsmarr4 yrsmarr5 yrsmarr6    fitted
#> 1      0      0        0        0        0        0        1        0 0.8865564
#> 2      1      0        0        0        1        0        0        0 0.8865564
#>    se.fitted dydx_kids dydx_vryunhap Var_dydx_kids Var_dydx_vryunhap
#> 1 0.07117823 0.5192513     0.8653101    0.01740253        0.03918275
#> 2 0.07117823 0.5192513     0.8653101    0.01740253        0.03918275
#>   SE_dydx_kids SE_dydx_vryunhap X_weights X_at_number
#> 1   0.04476005        0.1314996        NA           1
#> 2   0.04476005        0.1314996        NA           1
```

A dataset with one marginal effect estimate per unit of observation is a
bit unwieldy and difficult to interpret. Many analysts like to report
the “Average Marginal Effect”, that is, the average of all the
unit-specific marginal effects. These are easy to compute based on the
full `data.frame` shown above, but the `summary` function is convenient:

``` r
summary(mfx)
#> Average marginal effects 
#>            Marg. Effect Std. Error z value Pr(>|z|)   2.5 %  97.5 %
#> 1     kids      0.85274    0.13505 6.31418        0 0.58804 1.11744
#> 2 vryunhap      1.42109    0.20381 6.97252        0 1.02162 1.82055
#> 
#> Model type:  glm 
#> Prediction type:  response
```

You can also calculate the “Median Marginal Effect” (or any other
aggregation function) by changing one argument:

``` r
summary(mfx, aggregation_function = median)
#> Median marginal effects 
#>            Marg. Effect Std. Error z value Pr(>|z|)   2.5 %  97.5 %
#> 1     kids      0.93273    0.15933 5.85401        0 0.62044 1.24501
#> 2 vryunhap      1.55438    0.20285 7.66255        0 1.15679 1.95197
#> 
#> Model type:  glm 
#> Prediction type:  response
```

You can also extract average marginal effects using `tidy` and `glance`
methods which conform to the [`broom` package
specification](https://broom.tidymodels.org/):

``` r
tidy(mfx)
#>       term  estimate std.error statistic      p.value  conf.low conf.high
#> 1     kids 0.8527418 0.1350519  6.314178 2.716005e-10 0.5880449  1.117439
#> 2 vryunhap 1.4210854 0.2038124  6.972517 3.113287e-12 1.0216204  1.820550

glance(mfx)
#>   null.deviance df.null    logLik      AIC      BIC deviance df.residual nobs
#> 1      2925.455     600 -1662.128 3330.257 3343.453 2830.267         598  601
```

# Regression tables

Average marginal effects are easy to display in a regression table using
packages like `modelsummary`:

``` r
library(modelsummary)

# fit models and store them in a named list
mod <- list(
    "Kids (Logit)" = glm(kids ~ vryunhap, data = dat, family = binomial),
    "Affairs (Poisson)" = glm(naffairs ~ kids + vryunhap, data = dat, family = poisson))

# apply the `meffects` function to all the models using `lapply`
mfx <- lapply(mod, meffects)

# build a table
modelsummary(mfx, output = "markdown")
```

|          | Kids (Logit) | Affairs (Poisson) |
|:---------|:------------:|:-----------------:|
| vryunhap |    0.113     |       1.421       |
|          |   (0.131)    |      (0.204)      |
| kids     |              |       0.853       |
|          |              |      (0.135)      |
| Num.Obs. |     601      |        601        |
| AIC      |    721.0     |      3330.3       |
| BIC      |    729.8     |      3343.5       |
| Log.Lik. |   -358.491   |     -1662.128     |

# Plots

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
```

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
