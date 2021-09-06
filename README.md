fastmfx: Fast Marginal Effects
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Proof of concept

This is just a proof of concept. Please do not use in serious
applications.

# Installation

You can install the released version of fastmfx from Github:

`{r. eval=FALSE} remotes::install_github("vincentarelbundock/fastmfx")`

# Example

Load the library, simulate data, and estimate a logistic regression
model with interaction terms:

``` r
library(fastmfx)

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
#>   y         x1         x2          x3          x4     dydx_x1     dydx_x2
#> 1 0 -0.9398424 -0.3989979 -0.01261638 -0.84468299 0.038695650 0.039617224
#> 2 0 -0.9072317 -0.4557207 -1.25084417  0.24636617 0.019461190 0.019924677
#> 3 1  0.4159616  0.8759458 -0.60278837 -1.07809264 0.397247833 0.406707602
#> 4 0  0.9190068 -0.2029218  0.60564161 -0.99868128 0.403614582 0.413226469
#> 5 0 -1.0594245 -1.3643053 -1.74097520  0.07522011 0.001239623 0.001269146
#> 6 0  0.8480150 -0.9624568 -1.30264996  2.82120927 0.034040798 0.034851513
#>        dydx_x3      dydx_x4   se_dydx_x1   se_dydx_x2   se_dydx_x3   se_dydx_x4
#> 1  0.002757561  0.044028625 0.0079455603 0.0087614066 0.0032799297 0.0092705598
#> 2  0.025482383 -0.005201425 0.0052943790 0.0056257765 0.0066923235 0.0021747695
#> 3 -0.076908985  0.185952285 0.0346193644 0.0302943458 0.0363644300 0.0362391173
#> 4 -0.041766749  0.742418789 0.0432548766 0.0373693596 0.0350748223 0.0523380682
#> 5  0.001382378 -0.001020651 0.0005644503 0.0005746979 0.0006149435 0.0004772131
#> 6  0.144073272 -0.011099072 0.0213715351 0.0213305863 0.0823442710 0.0037613864
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

# Benchmarks

Unit-level standard errors can be expensive to compute, so we run the
benchmarks with and without standard errors.

# Benchmarks: Marginal effects + Variances

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
#> 1 margins(mod, unit_ses = TRUE)     38.7s    38.7s    0.0259     947MB     2.82
#> 2 mfx(mod, variance = vcov(mod))   81.4ms   90.3ms   11.1       11.4MB     3.70
```

# Benchmarks: Marginal effects only

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
#> 1 margins(mod, unit_ses = FALSE)   71.9ms   73.8ms      13.6   22.96MB        0
#> 2 mfx(mod, variance = NULL)        48.2ms   48.3ms      20.6    1.52MB        0
```
