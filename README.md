fastmargins: Fast Marginal Effects
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Proof of concept

This is just a proof of concept. Please do not use in serious
applications.

# Installation

You can install the released version of fastmargins from Github:

`{r. eval=FALSE}
remotes::install_github("vincentarelbundock/fastmargins")`

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
#>   y         x1          x2          x3         x4      dydx_x1      dydx_x2
#> 1 1 -0.1388475  2.34886295  0.95358357  0.7130788 0.0006811091 0.0006389892
#> 2 0 -0.1427565  0.08954914 -0.14785917  0.6237339 0.4279847801 0.4015164852
#> 3 0  0.5004366 -1.25412479 -1.44442735  1.0714918 0.0183349145 0.0172008869
#> 4 0 -1.2411815 -1.94561478 -0.02921365 -0.1402058 0.0045427831 0.0042618083
#> 5 1  0.4600975  0.75287178 -0.81304648  0.6142473 0.3736996712 0.3505889175
#> 6 1 -0.5413172 -1.06761719  1.27703422  0.9506337 0.0713218278 0.0669112479
#>       dydx_x3      dydx_x4   se_dydx_x1   se_dydx_x2   se_dydx_x3   se_dydx_x4
#> 1 0.001100082  0.001290116 0.0003574157 0.0003190751 0.0005582161 0.0006543667
#> 2 0.654432845  0.356440657 0.0349929169 0.0341713670 0.0523732416 0.0272213009
#> 3 0.035951104 -0.007638761 0.0072473505 0.0063478693 0.0126453308 0.0022526768
#> 4 0.003601955  0.004302770 0.0017477094 0.0015779604 0.0014841083 0.0017203508
#> 5 0.568004475  0.071673225 0.0330740397 0.0296934413 0.0697884300 0.0251091507
#> 6 0.131519299  0.157320374 0.0245448954 0.0240245997 0.0386438331 0.0468536441
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
#> 1 margins(mod, unit_ses = TRUE)     48.5s    48.5s    0.0206     947MB     2.41
#> 2 mfx(mod, variance = vcov(mod))  102.7ms  110.9ms    9.18      11.4MB     0
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
#> 1 margins(mod, unit_ses = FALSE)  101.8ms  118.1ms      8.34   22.96MB     0   
#> 2 mfx(mod, variance = NULL)        54.6ms   57.4ms     17.4     1.52MB     8.71
```
