
# Bootstrap & Simulation

`marginaleffects` offers an `inferences()` function to compute
uncertainty estimates using the bootstrap and simulation-based
inference.

WARNING: The `inferences()` function is experimental. It may be renamed,
the user interface may change, or the functionality may migrate to
arguments in other `marginaleffects` functions.

Consider a simple model:

``` r
library(marginaleffects)

mod <- lm(Sepal.Length ~ Petal.Width * Petal.Length + factor(Species), data = iris)
```

We will compute uncertainty estimates around the output of
`comparisons()`, but note that the same approach works with the
`predictions()` and `slopes()` functions as well.

## Delta method

The default strategy to compute standard errors and confidence intervals
is the delta method. This is what we obtain by calling:

``` r
avg_comparisons(mod, by = "Species", variables = "Petal.Width")
#> 
#>         Term Contrast    Species Estimate Std. Error      z Pr(>|z|)   S  2.5 % 97.5 %
#>  Petal.Width mean(+1) setosa      -0.1103      0.285 -0.387    0.699 0.5 -0.669  0.449
#>  Petal.Width mean(+1) versicolor  -0.0201      0.160 -0.125    0.900 0.2 -0.334  0.293
#>  Petal.Width mean(+1) virginica    0.0216      0.169  0.128    0.898 0.2 -0.309  0.353
#> 
#> Columns: term, contrast, Species, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted 
#> Type:  response
```

Since this is the default method, we obtain the same results if we add
the `inferences()` call in the chain:

``` r
avg_comparisons(mod, by = "Species", variables = "Petal.Width") |>
  inferences(method = "delta")
#> 
#>         Term Contrast    Species Estimate Std. Error      z Pr(>|z|)   S  2.5 % 97.5 %
#>  Petal.Width mean(+1) setosa      -0.1103      0.285 -0.387    0.699 0.5 -0.669  0.449
#>  Petal.Width mean(+1) versicolor  -0.0201      0.160 -0.125    0.900 0.2 -0.334  0.293
#>  Petal.Width mean(+1) virginica    0.0216      0.169  0.128    0.898 0.2 -0.309  0.353
#> 
#> Columns: term, contrast, Species, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted 
#> Type:  response
```

## Bootstrap

`marginaleffects` supports three bootstrap frameworks in `R`: the
well-established `boot` package, the newer `rsample` package, and the
so-called “bayesian bootstrap” in `fwb`.

### `boot`

``` r
avg_comparisons(mod, by = "Species", variables = "Petal.Width") |>
  inferences(method = "boot")
#> 
#>         Term Contrast    Species Estimate Std. Error  2.5 % 97.5 %
#>  Petal.Width mean(+1) setosa      -0.1103      0.267 -0.636  0.432
#>  Petal.Width mean(+1) versicolor  -0.0201      0.164 -0.343  0.328
#>  Petal.Width mean(+1) virginica    0.0216      0.185 -0.344  0.388
#> 
#> Columns: term, contrast, Species, estimate, predicted_lo, predicted_hi, predicted, std.error, conf.low, conf.high 
#> Type:  response
```

All unknown arguments that we feed to `inferences()` are pushed forward
to `boot::boot()`:

``` r
est <- avg_comparisons(mod, by = "Species", variables = "Petal.Width") |>
  inferences(method = "boot", sim = "balanced", R = 500, conf_type = "bca")
est
#> 
#>         Term Contrast    Species Estimate Std. Error  2.5 % 97.5 %
#>  Petal.Width mean(+1) setosa      -0.1103      0.272 -0.642  0.458
#>  Petal.Width mean(+1) versicolor  -0.0201      0.162 -0.321  0.301
#>  Petal.Width mean(+1) virginica    0.0216      0.190 -0.374  0.393
#> 
#> Columns: term, contrast, Species, estimate, predicted_lo, predicted_hi, predicted, std.error, conf.low, conf.high 
#> Type:  response
```

We can extract the original `boot` object from an attribute:

``` r
attr(est, "inferences")
#> 
#> BALANCED BOOTSTRAP
#> 
#> 
#> Call:
#> bootstrap_boot(model = model, INF_FUN = INF_FUN, newdata = ..1, 
#>     vcov = ..2, variables = ..3, type = ..4, by = ..5, conf_level = ..6, 
#>     cross = ..7, comparison = ..8, transform = ..9, wts = ..10, 
#>     hypothesis = ..11, eps = ..12)
#> 
#> 
#> Bootstrap Statistics :
#>        original       bias    std. error
#> t1* -0.11025325 0.0009435591   0.2723956
#> t2* -0.02006005 0.0009093862   0.1615825
#> t3*  0.02158742 0.0008936066   0.1902839
```

Or we can extract the individual draws with the `posterior_draws()`
function:

``` r
posterior_draws(est) |> head()
#>   drawid        draw        term contrast    Species    estimate predicted_lo predicted_hi predicted std.error   conf.low conf.high
#> 1      1 -0.10996114 Petal.Width mean(+1)     setosa -0.11025325     4.957514     4.845263  4.957514 0.2723956 -0.6419127 0.4583629
#> 2      1  0.26200784 Petal.Width mean(+1) versicolor -0.02006005     6.327949     6.322072  6.327949 0.1615825 -0.3209694 0.3008687
#> 3      1  0.43376764 Petal.Width mean(+1)  virginica  0.02158742     7.015513     7.051542  7.015513 0.1902839 -0.3735153 0.3934188
#> 4      2  0.18267101 Petal.Width mean(+1)     setosa -0.11025325     4.957514     4.845263  4.957514 0.2723956 -0.6419127 0.4583629
#> 5      2  0.08600361 Petal.Width mean(+1) versicolor -0.02006005     6.327949     6.322072  6.327949 0.1615825 -0.3209694 0.3008687
#> 6      2  0.04136663 Petal.Width mean(+1)  virginica  0.02158742     7.015513     7.051542  7.015513 0.1902839 -0.3735153 0.3934188

posterior_draws(est, shape = "DxP") |> dim()
#> [1] 500   3
```

### `rsample`

As before, we can pass arguments to `rsample::bootstraps()` through
`inferences()`. For example, for stratified resampling:

``` r
est <- avg_comparisons(mod, by = "Species", variables = "Petal.Width") |>
  inferences(method = "rsample", R = 100, strata = "Species")
est
#> 
#>         Term Contrast    Species Estimate  2.5 % 97.5 %
#>  Petal.Width mean(+1) setosa      -0.1103 -0.681  0.529
#>  Petal.Width mean(+1) versicolor  -0.0201 -0.299  0.353
#>  Petal.Width mean(+1) virginica    0.0216 -0.337  0.391
#> 
#> Columns: term, contrast, Species, estimate, predicted_lo, predicted_hi, predicted, conf.low, conf.high 
#> Type:  response

attr(est, "inferences")
#> # Bootstrap sampling using stratification with apparent sample 
#> # A tibble: 101 × 3
#>    splits           id           estimates       
#>    <list>           <chr>        <list>          
#>  1 <split [150/57]> Bootstrap001 <tibble [3 × 7]>
#>  2 <split [150/48]> Bootstrap002 <tibble [3 × 7]>
#>  3 <split [150/51]> Bootstrap003 <tibble [3 × 7]>
#>  4 <split [150/53]> Bootstrap004 <tibble [3 × 7]>
#>  5 <split [150/57]> Bootstrap005 <tibble [3 × 7]>
#>  6 <split [150/48]> Bootstrap006 <tibble [3 × 7]>
#>  7 <split [150/56]> Bootstrap007 <tibble [3 × 7]>
#>  8 <split [150/56]> Bootstrap008 <tibble [3 × 7]>
#>  9 <split [150/57]> Bootstrap009 <tibble [3 × 7]>
#> 10 <split [150/55]> Bootstrap010 <tibble [3 × 7]>
#> # ℹ 91 more rows
```

Or we can extract the individual draws with the `posterior_draws()`
function:

``` r
posterior_draws(est) |> head()
#>   drawid        draw        term contrast    Species    estimate predicted_lo predicted_hi predicted   conf.low conf.high
#> 1      1 -0.24029577 Petal.Width mean(+1)     setosa -0.11025325     4.957514     4.845263  4.957514 -0.6809434 0.5292387
#> 2      1  0.03316350 Petal.Width mean(+1) versicolor -0.02006005     6.327949     6.322072  6.327949 -0.2985162 0.3533660
#> 3      1  0.15943562 Petal.Width mean(+1)  virginica  0.02158742     7.015513     7.051542  7.015513 -0.3369258 0.3907752
#> 4      2 -0.31020800 Petal.Width mean(+1)     setosa -0.11025325     4.957514     4.845263  4.957514 -0.6809434 0.5292387
#> 5      2 -0.07690623 Petal.Width mean(+1) versicolor -0.02006005     6.327949     6.322072  6.327949 -0.2985162 0.3533660
#> 6      2  0.03082282 Petal.Width mean(+1)  virginica  0.02158742     7.015513     7.051542  7.015513 -0.3369258 0.3907752

posterior_draws(est, shape = "PxD") |> dim()
#> [1]   3 100
```

### Fractional Weighted Bootstrap (aka Bayesian Bootstrap)

[The `fwb` package](https://ngreifer.github.io/fwb/) implements
fractional weighted bootstrap (aka Bayesian bootstrap):

> “fwb implements the fractional weighted bootstrap (FWB), also known as
> the Bayesian bootstrap, following the treatment by Xu et al. (2020).
> The FWB involves generating sets of weights from a uniform Dirichlet
> distribution to be used in estimating statistics of interest, which
> yields a posterior distribution that can be interpreted in the same
> way the traditional (resampling-based) bootstrap distribution can be.”
> -Noah Greifer

The `inferences()` function makes it easy to apply this inference
strategy to `marginaleffects` objects:

``` r
avg_comparisons(mod) |> inferences(method = "fwb")
#> 
#>          Term            Contrast Estimate Std. Error  2.5 % 97.5 %
#>  Petal.Length +1                    0.8929     0.0809  0.721  1.045
#>  Petal.Width  +1                   -0.0362     0.1605 -0.355  0.283
#>  Species      versicolor - setosa  -1.4629     0.3387 -2.130 -0.832
#>  Species      virginica - setosa   -1.9842     0.4009 -2.755 -1.218
#> 
#> Columns: term, contrast, estimate, std.error, conf.low, conf.high 
#> Type:  response
```

## Simulation-based inference

This simulation-based strategy to compute confidence intervals was
described in Krinsky & Robb (1986) and popularized by King, Tomz,
Wittenberg (2000). We proceed in 3 steps:

1.  Draw `R` sets of simulated coefficients from a multivariate normal
    distribution with mean equal to the original model’s estimated
    coefficients and variance equal to the model’s variance-covariance
    matrix (classical, “HC3”, or other).
2.  Use the `R` sets of coefficients to compute `R` sets of estimands:
    predictions, comparisons, or slopes.
3.  Take quantiles of the resulting distribution of estimands to obtain
    a confidence interval and the standard deviation of simulated
    estimates to estimate the standard error.

Here are a few examples:

``` r
library(ggplot2)
library(ggdist)

avg_comparisons(mod, by = "Species", variables = "Petal.Width") |>
  inferences(method = "simulation")
#> 
#>         Term Contrast    Species Estimate Std. Error  2.5 % 97.5 %
#>  Petal.Width mean(+1) setosa      -0.1103      0.284 -0.698  0.445
#>  Petal.Width mean(+1) versicolor  -0.0201      0.161 -0.352  0.295
#>  Petal.Width mean(+1) virginica    0.0216      0.168 -0.322  0.333
#> 
#> Columns: term, contrast, Species, estimate, std.error, conf.low, conf.high, predicted_lo, predicted_hi, predicted, tmp_idx 
#> Type:  response
```

Since simulation based inference generates `R` estimates of the
quantities of interest, we can treat them similarly to draws from the
posterior distribution in bayesian models. For example, we can extract
draws using the `posterior_draws()` function, and plot their
distributions using packages like`ggplot2` and `ggdist`:

``` r
avg_comparisons(mod, by = "Species", variables = "Petal.Width") |>
  inferences(method = "simulation") |>
  posterior_draws("rvar") |>
  ggplot(aes(y = Species, xdist = rvar)) +
  stat_slabinterval()
```

<img
src="../bootstrap.markdown_strict_files/figure-markdown_strict/unnamed-chunk-13-1.png"
style="width:100.0%" />

## Multiple imputation and missing data

The same workflow and the same `inferences` function can be used to
estimate models with multiple imputation for missing data.
