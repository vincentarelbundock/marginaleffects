
# Performance

## What to do when `marginaleffects` is slow?

Some options:

1.  Compute [marginal effects and contrasts at the mean (or other
    representative
    value)](slopes.html#marginal-effect-at-user-specified-values)
    instead of all observed rows of the original dataset: Use the
    `newdata` argument and the `datagrid()` function.
2.  Compute marginal effects for a subset of variables, paying special
    attention to exclude factor variables which can be particularly
    costly to process: Use the `variables` argument.
3.  Do not compute standard errors: Use the `vcov = FALSE` argument.

This simulation illustrates how computation time varies for a model with
25 regressors and 100,000 observations:

``` r
library(marginaleffects)

## simulate data and fit a large model
N <- 1e5
dat <- data.frame(matrix(rnorm(N * 26), ncol = 26))
mod <- lm(X1 ~ ., dat)

results <- bench::mark(
    # marginal effects at the mean; no standard error
    slopes(mod, vcov = FALSE, newdata = "mean"),
    # marginal effects at the mean
    slopes(mod, newdata = "mean"),
    # 1 variable; no standard error
    slopes(mod, vcov = FALSE, variables = "X3"),
    # 1 variable
    slopes(mod, variables = "X3"),
    # 26 variables; no standard error
    slopes(mod, vcov = FALSE),
    # 26 variables
    slopes(mod),
    iterations = 1, check = FALSE)

results[, c(1, 3, 5)]
## <bch:expr>                                  <bch:tm> <bch:byt>
## slopes(mod, vcov = FALSE, newdata = "mean") 230.09ms    1.24GB
## slopes(mod, newdata = "mean")               329.14ms    1.25GB
## slopes(mod, vcov = FALSE, variables = "X3")  198.7ms  496.24MB
## slopes(mod, variables = "X3")                  1.27s    3.29GB
## slopes(mod, vcov = FALSE)                      5.73s   11.05GB
## slopes(mod)                                   21.68s   78.02GB
```

The benchmarks above were conducted using the development version of
`marginaleffects` on 2023-02-03.

## Speed comparison

The `slopes` function is relatively fast. This simulation was conducted
using the development version of the package on 2022-04-04:

``` r
library(margins)

N <- 1e3
dat <- data.frame(
    y = sample(0:1, N, replace = TRUE),
    x1 = rnorm(N),
    x2 = rnorm(N),
    x3 = rnorm(N),
    x4 = factor(sample(letters[1:5], N, replace = TRUE)))
mod <- glm(y ~ x1 + x2 + x3 + x4, data = dat, family = binomial)
```

`marginaleffects` is about the same speed as `margins` when unit-level
standard errors are *not* computed:

``` r
results <- bench::mark(
    slopes(mod, vcov = FALSE),
    margins(mod, unit_ses = FALSE),
    check = FALSE, relative = TRUE)
results[, c(1, 3, 5)]

##   expression                        median mem_alloc
##   <bch:expr>                          <dbl>     <dbl>
## 1 slopes(mod, vcov = FALSE)   1         1
## 2 margins(mod, unit_ses = FALSE)       6.15      4.17
```

`marginaleffects` can be 100x times faster than `margins` when
unit-level standard errors are computed:

``` r
results <- bench::mark(
    slopes(mod, vcov = TRUE),
    margins(mod, unit_ses = TRUE),
    check = FALSE, relative = TRUE, iterations = 1)
results[, c(1, 3, 5)]

## <bch:expr>                     <dbl>     <dbl>
## slopes(mod, vcov = TRUE)          1        1  
## margins(mod, unit_ses = TRUE)   128.      18.6
```

Models estimated on larger datasets (\> 1000 observations) can be
difficult to process using the `margins` package, because of memory and
time constraints. In contrast, `marginaleffects` can work well on much
larger datasets.

In some cases, `marginaleffects` will be considerably slower than
packages like `emmeans` or `modmarg`. This is because these packages
make extensive use of hard-coded analytical derivatives, or reimplement
their own fast prediction functions.
