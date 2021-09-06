fastmargins: Fast Marginal Effects
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Proof of concept

This is just a proof of concept. Please do not use in serious
applications.

# Installation

You can install the released version of fastmargins from Github:

``` r
remotes::install_github("vincentarelbundock/fastmargins")
```

# Examples

## Logit regression with a multiplicative interaction term

Load the library, simulate data, and estimate a logistic regression
model with interaction terms:

``` r
library(fastmargins)

N <- 1e3
dat <- data.frame(
    x2 = rnorm(N),
    x1 = rnorm(N),
    x3 = rnorm(N),
    x4 = rnorm(N),
    e = rnorm(N))
dat$y <- as.numeric(plogis(
    dat$x1 + dat$x2 + dat$x3 + dat$x4 + dat$x3 * dat$x4 + dat$e) > 0.5)

mod <- glm(y ~ x1 + x2 + x3 * x4, data = dat, family = binomial)
```

Compute unit-level marginal effects and variances:

``` r
res <- mfx(mod)
head(res)
#>   y         x1         x2          x3         x4    dydx_x1    dydx_x2
#> 1 0 -0.4314039  0.7843935 -0.46414782 -0.3344323 0.39577379 0.35078843
#> 2 1 -0.4727572 -0.1597447 -0.47979869  1.2051817 0.37059818 0.32847403
#> 3 1  0.8935453 -1.3832461  0.51524443  1.4268085 0.04016064 0.03559619
#> 4 1 -0.9882923  0.1713650  0.14651325  2.0666091 0.10482873 0.09291437
#> 5 0  1.0550042 -1.6421494  0.79159637 -0.5750850 0.31141112 0.27601403
#> 6 0 -1.4645218  0.1530650  0.01203903  0.5836429 0.29817746 0.26428451
#>      dydx_x3    dydx_x4 se_dydx_x1 se_dydx_x2 se_dydx_x3 se_dydx_x4
#> 1 0.20951826 0.19436352 0.03081659 0.03492218 0.02873504 0.02740292
#> 2 0.75213335 0.17634776 0.03130465 0.03120440 0.06137066 0.03388806
#> 3 0.09016752 0.05804355 0.01186115 0.01199401 0.02537074 0.01571310
#> 4 0.30068804 0.11385126 0.02949572 0.02395656 0.07153288 0.02334606
#> 5 0.09183942 0.53395731 0.05017214 0.02776799 0.03165472 0.06328442
#> 6 0.42457807 0.28477564 0.02075781 0.03373799 0.05354220 0.04059182
```

The results are obtained using a slightly different numerical
approximation strategy than the one used in `margins`, so they will
differ a bit. However, they are essentially indistinguishable:

``` r
library(margins)

mar <- margins(mod, unit_ses = TRUE) |> data.frame()

cor(mar$dydx_x1, res$dydx_x1)
#> [1] 1
cor(mar$SE_dydx_x1, res$se_dydx_x1)
#> [1] 1

cor(mar$dydx_x3, res$dydx_x3)
#> [1] 1
cor(mar$SE_dydx_x3, res$se_dydx_x3)
#> [1] 1
```

## Conditional marginal effects with `ggplot2`

``` r
library(tidyverse)

lm(mpg ~ hp * wt, data = mtcars) |>
    mfx() |> 
    mutate(conf.low = dydx_hp - se_dydx_hp * 1.96,
           conf.high = dydx_hp + se_dydx_hp * 1.96) |>
    ggplot(aes(x = wt, 
               y = dydx_hp, 
               ymin = conf.low, 
               ymax = conf.high)) +
    geom_ribbon(alpha = .1) +
    geom_line()
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

# Benchmarks

Here are a couple naive benchmarks to compare the speed of computation
with the `fastmargins` and `margins` packages. Since unit-level standard
errors can be expensive to compute, we run the benchmarks with and
without standard errors.

## Marginal effects and standard errors (unit-level)

In this naive benchmark, computing marginal effects with their
unit-level standard errors is over 300x faster.

``` r
b1 = bench::mark(
    margins(mod, unit_ses = TRUE),
    mfx(mod, variance = vcov(mod)),
    check = FALSE,
    max_iterations = 3)
#> Warning: Some expressions had a GC in every iteration; so filtering is disabled.
b1
#> # A tibble: 2 × 6
#>   expression                          min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                     <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 margins(mod, unit_ses = TRUE)     50.3s    50.3s    0.0199     947MB     1.29
#> 2 mfx(mod, variance = vcov(mod))  118.4ms  119.5ms    8.20      12.6MB     0
```

## Marginal effects only

In this naive benchmark, computing marginal effects *without* their
unit-level standard errors is over 40% faster.

``` r
b2 = bench::mark(
    margins(mod, unit_ses = FALSE),
    mfx(mod, variance = NULL),
    check = FALSE,
    max_iterations = 3)
b2
#> # A tibble: 2 × 6
#>   expression                          min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                     <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 margins(mod, unit_ses = FALSE)  101.3ms  101.5ms      9.33   22.96MB        0
#> 2 mfx(mod, variance = NULL)        65.2ms   67.7ms     14.7     1.74MB        0
```
