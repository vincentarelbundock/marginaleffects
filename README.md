`marginaleffects`: Marginal effects using `R`, automatic
differentiation, and the delta method
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

This package is still experimental. *Use with caution!*

# What?

The `marginaleffects` package allows `R` users to compute and plot
“marginal effects” for a *wide* variety of models.

A “marginal effect” is a measure of the association between a change in
the regressors, and a change in the response variable. More formally,
[the `margins`
vignette](https://cran.r-project.org/web/packages/margins/index.html)
defines “marginal effects” as follows:

> “Marginal effects are partial derivatives of the regression equation
> with respect to each variable in the model for each unit in the data.”

Marginal effects are extremely useful, because they are intuitive and
easy to interpret. They are often the main quantity of interest in an
empirical analysis. Unfortunately, they can be often be quite difficult
to compute:

> In ordinary least squares regression with no interactions or
> higher-order term, the estimated slope coefficients are marginal
> effects. In other cases and for generalized linear models, the
> coefficients are not marginal effects at least not on the scale of the
> response variable.

To calculate marginal effects, we take the derivative of the regression
equation. This can be annoying, especially when our models are
non-linear, or when regressors are transformed or interacted. Computing
the variance is even worse. The `marginaleffects` package hopes to do
most of this hard work for you.

# Why?

Many `R` packages advertise their ability to compute “marginal effects.”
However, most of them do *not* actually compute marginal effects *as
defined above* (the term is ambiguously defined in the statistical
literature and used differently across fields). Instead, they compute
related quantities such as “Estimated Marginal Means” or “Differences in
Predicted Probabilities.” The rare packages which actually compute
marginal effects are typically limited in the model types they support,
and in the range of transformations they allow (interactions,
polynomials, etc.).

The main package in the `R` ecosystem to compute marginal effects is
[the fantastic, trailblazing, and powerful
`margins`](https://cran.r-project.org/web/packages/margins/index.html)
by [Thomas J. Leeper.](https://thomasleeper.com/) The `marginaleffects`
package is (essentially) a clone of `margins`.

So why did I write a new package?

-   *Speed:* In a typical case shown below, `marginaleffects` is over
    400x faster (55 seconds vs. 130 *milli*seconds).
-   *Efficiency:* Between 5 and 60x smaller memory footprint.
-   *Extensibility:* Adding support for new models often requires less
    than 10 lines of new code.
-   `ggplot2` support: Plot your (conditional) marginal effects using
    `ggplot2`.
-   *Tidy:* The results produced by `marginaleffects` follow “tidy”
    principles and are easy to process and program with.

# How?

By using [the `numDeriv`
package](https://cran.r-project.org/web/packages/numDeriv/index.html) to
compute gradients and jacobians. That’s it. That’s the secret sauce.

# Supported models

This table shows the list of models supported by `marginaleffect`, and
shows which numerical results have been checked against alternative
software packages: Stata’s `margins` command and R’s `margins` package.

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

You can install the released version of marginaleffects from Github:

``` r
remotes::install_github("vincentarelbundock/marginaleffects")
```

# Getting started

First, we load the library, download data from the [`Rdatasets`
archive](https://vincentarelbundock.github.io/Rdatasets/articles/data.html),
and estimate a Poisson GLM model:

``` r
library(marginaleffects)

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/COUNT/affairs.csv")
mod <- glm(naffairs ~ kids + vryunhap, data = dat, family = poisson)
```

The `marginaleffects` function computes a distinct estimate of the
marginal effect and of the standard error for each regressor (“term”),
for each unit of observation (“rowid”). You can browse view and
manipulate the full results with functions like `head`, as you would any
`data.frame`:

``` r
mfx <- marginaleffects(mod)

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

# Average Marginal Effects

A dataset with one marginal effect estimate per unit of observation is a
bit unwieldy and difficult to interpret. Many analysts like to report
the “Average Marginal Effect”, that is, the average of all the
observation-specific marginal effects. These are easy to compute based
on the full `data.frame` shown above, but the `summary` function is
convenient:

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
summary(mfx, agg_fun = median)
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

# Typical Marginal Effects

Sometimes, we are not interested in *all* the unit-specific marginal
effects, but would rather look at the estimated marginal effects for
certain “typical” individuals. The `typical` function helps us build
datasets full of “typical” rows. For example, to generate very unhappy
individuals with or without kids:

``` r
typical(mod, at = list("vryunhap" = 1, "kids" = 0:1))
#>   vryunhap kids
#> 1        1    0
#> 2        1    1
```

This dataset can then be used in `marginaleffects` to compute marginal
effects for those (fictional) individuals:

``` r
nd <- typical(mod, at = list("vryunhap" = 1, "kids" = 0:1))
marginaleffects(mod, newdata = nd)
#>   rowid     term     dydx std.error vryunhap kids
#> 1     1     kids 1.378088 0.2016120        1    0
#> 2     1 vryunhap 2.296569 0.6271726        1    0
#> 3     2     kids 2.475374 0.5077121        1    1
#> 4     2 vryunhap 4.125186 1.0711911        1    1
```

When a variable is omitted from the `at` list, `typical` will
automatically select the median (or mode) of the missing variable.

# Counterfactual Marginal Effects

The `typical` function allowed us look at completely fictional
individual. The `counterfactual` lets us compute the marginal effects
for the actual observations in our dataset, but with a few manipulated
values. For example, this code will create a `data.frame` twice as long
as the original `dat`, where each observation is repeated with different
values of the `kids` variable:

``` r
nd <- counterfactual(mod, at = list("kids" = 0:1))
```

We see that the rows 1, 2, and 3 of the original dataset have been
replicated twice, with different values of the `kids` variable:

``` r
nd[nd$rowid %in% 1:3,]
#>     rowid vryunhap kids
#> 1       1        0    0
#> 2       2        0    0
#> 3       3        0    0
#> 602     1        0    1
#> 603     2        0    1
#> 604     3        0    1
```

Again, we can use this to compute average marginal effects over the
counterfactual individuals:

``` r
library(dplyr)

marginaleffects(mod, newdata = nd) |> 
    group_by(kids, term) %>%
    summarize(across(dydx:std.error, median))
#> # A tibble: 4 x 4
#> # Groups:   kids [2]
#>    kids term      dydx std.error
#>   <int> <chr>    <dbl>     <dbl>
#> 1     0 kids     0.519    0.0448
#> 2     0 vryunhap 0.865    0.132 
#> 3     1 kids     0.933    0.159 
#> 4     1 vryunhap 1.55     0.203
```

# Tables

Average marginal effects are easy to display in a regression table using
packages like `modelsummary`:

``` r
library(modelsummary)

# fit models and store them in a named list
mod <- list(
    "Kids (Logit)" = glm(kids ~ vryunhap, data = dat, family = binomial),
    "Affairs (Poisson)" = glm(naffairs ~ kids + vryunhap, data = dat, family = poisson))

# apply the `marginaleffects` function to all the models using `lapply`
mfx <- lapply(mod, marginaleffects)

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

# Plots (`ggplot2`)

Since the output of the `marginaleffects` function is “tidy”, it is very
easy to use the `data.frame` that this function produces directly to
draw plots with Base `R` functions, `lattice`, or `ggplot2`. In
addition, the `marginaleffects` package also offers two functions to
draw frequently used plots.

The first is a simple `plot` command to draw the average marginal
effects:

``` r
mod <- lm(mpg ~ hp + wt + drat, data = mtcars)
mfx <- marginaleffects(mod)

plot(mfx)
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" />

The second is a `plot_cme` function to draw “Conditional Marginal
Effects.” This is useful when a model includes interaction terms and we
want to plot how the marginal effect of a variable changes as the value
of a “condition” (or “moderator”) variable changes:

``` r
mod <- lm(mpg ~ hp * wt + drat, data = mtcars)

plot_cme(mod, effect = "hp", condition = "wt")
```

<img src="man/figures/README-unnamed-chunk-17-1.png" width="100%" />

# Benchmarks

Here are two *very* naive benchmarks to compare the speed of
`marginaleffects` and `margins`. Computing the unit-level marginal
effects and standard errors in a logistic regression model with 1500
observations is over 300 times faster with `marginaleffects`.
Calculating only the marginal effects is about twice as fast with
`marginaleffects`.

Simulate data and fit model:

``` r
N <- 1500
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

Marginal effects and standard errors:

``` r
b1 = bench::mark(
    margins::margins(mod, unit_ses = TRUE),
    marginaleffects(mod),
    check = FALSE,
    max_iterations = 3)

b1
#> # A tibble: 2 x 6
#>   expression                                  min   median `itr/sec` mem_alloc
#>   <bch:expr>                             <bch:tm> <bch:tm>     <dbl> <bch:byt>
#> 1 margins::margins(mod, unit_ses = TRUE)    54.6s    54.6s    0.0183    1.39GB
#> 2 marginaleffects(mod)                           123.4ms  129.5ms    7.60     21.83MB
#> # … with 1 more variable: gc/sec <dbl>
```

Marginal effects only:

``` r
b2 = bench::mark(
    margins::margins(mod, unit_ses = FALSE),
    marginaleffects(mod, variance = NULL),
    check = FALSE,
    max_iterations = 3)

b2
#> # A tibble: 2 x 6
#>   expression                                   min   median `itr/sec` mem_alloc
#>   <bch:expr>                              <bch:tm> <bch:tm>     <dbl> <bch:byt>
#> 1 margins::margins(mod, unit_ses = FALSE)   92.7ms  101.3ms      9.74   34.14MB
#> 2 marginaleffects(mod, variance = NULL)            66.1ms   67.7ms     14.3     5.22MB
#> # … with 1 more variable: gc/sec <dbl>
```
