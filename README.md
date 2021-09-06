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

summary(mod)
#> 
#> Call:
#> glm(formula = y ~ x1 + x2 + x3 * x4, family = binomial, data = dat)
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -2.7271  -0.7051  -0.1885   0.7260   2.5893  
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  0.02079    0.08480   0.245    0.806    
#> x1           0.97229    0.09385  10.361  < 2e-16 ***
#> x2           1.04582    0.09808  10.663  < 2e-16 ***
#> x3           0.96172    0.10191   9.437  < 2e-16 ***
#> x4           1.10058    0.10271  10.715  < 2e-16 ***
#> x3:x4        0.93853    0.11970   7.841 4.49e-15 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 1382.93  on 999  degrees of freedom
#> Residual deviance:  885.63  on 994  degrees of freedom
#> AIC: 897.63
#> 
#> Number of Fisher Scoring iterations: 5
```

Compute unit-level marginal effects and variances:

``` r
res <- mfx(mod)
head(res)
#>   y         x1         x2          x3         x4     dydx_x1     dydx_x2
#> 1 1  0.1776867 -0.1935552  2.28200192  1.3943948 0.001185699 0.001275361
#> 2 1  1.1069474 -0.1839420  0.79333923  0.4618769 0.067015804 0.072083551
#> 3 0 -0.4001303 -0.2084529  0.14129243 -1.3696638 0.091622980 0.098552121
#> 4 1 -1.1236975  2.3575163  0.05344117 -0.1445274 0.165608911 0.178132475
#> 5 1 -0.3704111 -0.5638635 -1.44639133 -0.1851436 0.082353079 0.088581176
#> 6 1  1.4520958  0.4076593 -0.78185882 -1.7198915 0.229793254 0.247170823
#>        dydx_x3      dydx_x4   se_dydx_x1  se_dydx_x2  se_dydx_x3  se_dydx_x4
#> 1  0.002768543  0.003953501 0.0006849124 0.000746884 0.001405126 0.002024267
#> 2  0.096163398  0.127173302 0.0091808339 0.013343543 0.012567699 0.017452972
#> 3 -0.030506801  0.116209174 0.0117693410 0.013236280 0.014358391 0.009561125
#> 4  0.140704484  0.196001678 0.0292979360 0.013428674 0.022946135 0.029632146
#> 5  0.066739234 -0.021758446 0.0119928247 0.011765892 0.006236587 0.014677421
#> 6 -0.154204058  0.086686371 0.0196640647 0.025003754 0.041195442 0.030343455
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
#> Warning in system("timedatectl", intern = TRUE): running command 'timedatectl'
#> had status 1
#> ── Attaching packages ─────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.1 ──
#> ✔ tibble  3.1.3     ✔ dplyr   1.0.7
#> ✔ tidyr   1.1.3     ✔ stringr 1.4.0
#> ✔ readr   2.0.0     ✔ forcats 0.5.1
#> ✔ purrr   0.3.4
#> ── Conflicts ────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()

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
#> 1 margins(mod, unit_ses = TRUE)     50.8s    50.8s    0.0197   946.5MB     1.79
#> 2 mfx(mod, variance = vcov(mod))  118.3ms    119ms    8.40      12.6MB     0
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
#> 1 margins(mod, unit_ses = FALSE)  123.1ms    141ms      7.09   22.51MB     3.55
#> 2 mfx(mod, variance = NULL)        62.9ms   63.6ms     15.5     1.67MB     0
```
