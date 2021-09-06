
fastmargins: Fast Marginal Effects
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Proof of concept

This is just a proof of concept. Please do not use in serious
applications.

# Installation

You can install the released version of fastmargins from Github:

```r
remotes::install_github("vincentarelbundock/fastmargins")
```

# Example

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
#>   y          x1          x2         x3         x4    dydx_x1    dydx_x2
#> 1 1 -0.05710882 -0.03955901  0.2663629  1.5528969 0.02359321 0.02242696
#> 2 0 -1.46101992  0.12057078  1.9413666 -0.3170301 0.41221357 0.39183515
#> 3 0  0.19504407 -1.02289844  0.2686651 -1.4423351 0.02303646 0.02189755
#> 4 1  0.86767939  0.33877950  0.2565602  0.4375469 0.03988153 0.03791012
#> 5 1  0.74975103  1.57639836 -0.7730068  1.1803665 0.06016017 0.05718634
#> 6 0 -1.14716821  2.44451162 -1.4694027  0.7195470 0.34018131 0.32336359
#>       dydx_x3     dydx_x4  se_dydx_x1  se_dydx_x2  se_dydx_x3  se_dydx_x4
#> 1  0.06329837  0.03349969 0.007089214 0.006812727 0.017527645 0.008852562
#> 2  0.29151516  1.31503165 0.034288840 0.039609213 0.044316662 0.121388801
#> 3 -0.01110204  0.03276754 0.006706809 0.005831630 0.003346525 0.008520467
#> 4  0.06000385  0.05621422 0.008058450 0.008359958 0.012702297 0.011908868
#> 5  0.13772884  0.01934612 0.015613571 0.013926261 0.040199329 0.004757735
#> 6  0.61323923 -0.14095393 0.053558741 0.067749012 0.088165390 0.032926728
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
#> 1 margins(mod, unit_ses = TRUE)     49.5s    49.5s    0.0202     947MB     2.18
#> 2 mfx(mod, variance = vcov(mod))  107.2ms  110.2ms    8.47      11.4MB     0
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
#> 1 margins(mod, unit_ses = FALSE)   95.2ms  104.7ms      9.55   22.96MB     4.78
#> 2 mfx(mod, variance = NULL)        56.2ms   58.1ms     17.1     1.52MB     0
```
