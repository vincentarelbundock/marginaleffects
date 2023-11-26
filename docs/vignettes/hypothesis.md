
# Hypothesis Tests

This vignette introduces the `hypotheses()` function, and the
`hypothesis` argument of the `comparisons()`, `slopes()`, and
`predictions()` function. These features allow users to conduct linear
and non-linear hypothesis tests and to compute custom contrasts (linear
combinations) between parameters.

## Null hypothesis

The simplest way to modify a hypothesis test is to change the null
hypothesis. By default, all functions in the `marginaleffects` package
assume that the null is 0. This can be changed by changing the
`hypothesis` argument.

For example, consider a logistic regression model:

``` r
library(marginaleffects)
mod <- glm(am ~ hp + drat, data = mtcars, family = binomial)
```

We can compute the predicted outcome for a hypothetical unit where all
regressors are fixed to their sample means:

``` r
predictions(mod, newdata = "mean")
#> 
#>  Estimate Pr(>|z|)   S  2.5 % 97.5 %  hp drat
#>     0.231    0.135 2.9 0.0584  0.592 147  3.6
#> 
#> Columns: rowid, estimate, p.value, s.value, conf.low, conf.high, am, hp, drat 
#> Type:  invlink(link)
```

The Z statistic and p value reported above assume that the null
hypothesis equals zero. We can change the null with the `hypothesis`
argument:

``` r
predictions(mod, newdata = "mean", hypothesis = .5)
#> 
#>  Estimate Pr(>|z|)   S  2.5 % 97.5 %  hp drat
#>     0.231   0.0343 4.9 0.0584  0.592 147  3.6
#> 
#> Columns: rowid, estimate, p.value, s.value, conf.low, conf.high, am, hp, drat 
#> Type:  invlink(link)
```

This can obviously be useful in other contexts. For instance, if we
compute risk ratios (at the mean) associated with an increase of 1 unit
in `hp`, it makes more sense to test the null hypothesis that the ratio
of predictions is 1 rather than 0:

``` r
comparisons(
    mod,
    newdata = "mean",
    variables = "hp",
    comparison = "ratio",
    hypothesis = 1) |>
    print(digits = 3)
#> 
#>  Term Contrast Estimate Std. Error    z Pr(>|z|)   S 2.5 % 97.5 %  hp drat
#>    hp       +1     1.01    0.00791 1.05    0.293 1.8 0.993   1.02 147  3.6
#> 
#> Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted, am, hp, drat 
#> Type:  response
```

Warning: Z statistics and p values are computed *before* applying
functions in `transform`.

## Hypothesis tests with the delta method

The `marginaleffects` package includes a powerful function called
`hypotheses()`. This function emulates the behavior of the
well-established `car::deltaMethod` and `car::linearHypothesis`
functions, but it supports more models, requires fewer dependencies, and
offers some convenience features like shortcuts for robust standard
errors.

`hypotheses()` can be used to compute estimates and standard errors of
arbitrary functions of model parameters. For example, it can be used to
conduct tests of equality between coefficients, or to test the value of
some linear or non-linear combination of quantities of interest.
`hypotheses()` can also be used to conduct hypothesis tests on other
functions of a model’s parameter, such as adjusted predictions or
marginal effects.

Let’s start by estimating a simple model:

``` r
library(marginaleffects)
mod <- lm(mpg ~ hp + wt + factor(cyl), data = mtcars)
```

When the `FUN` and `hypothesis` arguments of `hypotheses()` equal `NULL`
(the default), the function returns a data.frame of raw estimates:

``` r
hypotheses(mod)
#> 
#>          Term Estimate Std. Error     z Pr(>|z|)     S   2.5 %    97.5 %
#>  (Intercept)   35.8460      2.041 17.56   <0.001 227.0 31.8457 39.846319
#>  hp            -0.0231      0.012 -1.93   0.0531   4.2 -0.0465  0.000306
#>  wt            -3.1814      0.720 -4.42   <0.001  16.6 -4.5918 -1.771012
#>  factor(cyl)6  -3.3590      1.402 -2.40   0.0166   5.9 -6.1062 -0.611803
#>  factor(cyl)8  -3.1859      2.170 -1.47   0.1422   2.8 -7.4399  1.068169
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

Test of equality between coefficients:

``` r
hypotheses(mod, "hp = wt")
#> 
#>     Term Estimate Std. Error    z Pr(>|z|)    S 2.5 % 97.5 %
#>  hp = wt     3.16       0.72 4.39   <0.001 16.4  1.75   4.57
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

Non-linear function of coefficients

``` r
hypotheses(mod, "exp(hp + wt) = 0.1")
#> 
#>                Term Estimate Std. Error     z Pr(>|z|)   S  2.5 %  97.5 %
#>  exp(hp + wt) = 0.1  -0.0594     0.0292 -2.04   0.0418 4.6 -0.117 -0.0022
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

The `vcov` argument behaves in the same was as in the `slopes()`
function. It allows us to easily compute robust standard errors:

``` r
hypotheses(mod, "hp = wt", vcov = "HC3")
#> 
#>     Term Estimate Std. Error    z Pr(>|z|)    S 2.5 % 97.5 %
#>  hp = wt     3.16      0.805 3.92   <0.001 13.5  1.58   4.74
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

We can use shortcuts like `b1`, `b2`, `...` to identify the position of
each parameter in the output of `FUN`. For example, `b2=b3` is
equivalent to `hp=wt` because those term names appear in the 2nd and 3rd
row when we call `hypotheses(mod)`.

``` r
hypotheses(mod, "b2 = b3")
#> 
#>     Term Estimate Std. Error    z Pr(>|z|)    S 2.5 % 97.5 %
#>  b2 = b3     3.16       0.72 4.39   <0.001 16.4  1.75   4.57
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

``` r
hypotheses(mod, hypothesis = "b* / b3 = 1")
#> 
#>         Term  Estimate Std. Error         z Pr(>|z|)    S  2.5 % 97.5 %
#>  b1 / b3 = 1 -12.26735    2.07340   -5.9165   <0.001 28.2 -16.33 -8.204
#>  b2 / b3 = 1  -0.99273    0.00413 -240.5539   <0.001  Inf  -1.00 -0.985
#>  b3 / b3 = 1   0.00000         NA        NA       NA   NA     NA     NA
#>  b4 / b3 = 1   0.05583    0.58287    0.0958    0.924  0.1  -1.09  1.198
#>  b5 / b3 = 1   0.00141    0.82981    0.0017    0.999  0.0  -1.62  1.628
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

Term names with special characters must be enclosed in backticks:

``` r
hypotheses(mod, "`factor(cyl)6` = `factor(cyl)8`")
#> 
#>                             Term Estimate Std. Error      z Pr(>|z|)   S 2.5 % 97.5 %
#>  `factor(cyl)6` = `factor(cyl)8`   -0.173       1.65 -0.105    0.917 0.1 -3.41   3.07
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

### Arbitrary functions: `FUN`

The `FUN` argument can be used to compute standard errors for arbitrary
functions of model parameters. This user-supplied function must accept a
single model object, and return a numeric vector or a data.frame with
two columns named `term` and `estimate`.

``` r
mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)

f <- function(x) {
    out <- x$coefficients["hp"] + x$coefficients["mpg"]
    return(out)
}
hypotheses(mod, FUN = f)
#> 
#>  Term Estimate Std. Error    z Pr(>|z|)   S 2.5 % 97.5 %
#>     1     1.31      0.593 2.22   0.0266 5.2 0.153   2.48
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

With labels:

``` r
f <- function(x) {
    out <- data.frame(
        term = "Horsepower + Miles per Gallon",
        estimate = x$coefficients["hp"] + x$coefficients["mpg"]
    )
    return(out)
}
hypotheses(mod, FUN = f)
#> 
#>                           Term Estimate Std. Error    z Pr(>|z|)   S 2.5 % 97.5 %
#>  Horsepower + Miles per Gallon     1.31      0.593 2.22   0.0266 5.2 0.153   2.48
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

Test of equality between two predictions (row 2 vs row 3):

``` r
f <- function(x) predict(x, newdata = mtcars)
hypotheses(mod, FUN = f, hypothesis = "b2 = b3")
#> 
#>     Term Estimate Std. Error     z Pr(>|z|)   S 2.5 % 97.5 %
#>  b2 = b3    -1.33      0.616 -2.16   0.0305 5.0 -2.54 -0.125
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

Note that we specified the `newdata` argument in the `f` function. This
is because the `predict()` method associated with `lm` objects will
automatically the original fitted values when `newdata` is `NULL`,
instead of returning the slightly altered fitted values which we need to
compute numerical derivatives in the delta method.

We can also use numeric vectors to specify linear combinations of
parameters. For example, there are 3 coefficients in the last model we
estimated. To test the null hypothesis that the sum of the 2nd and 3rd
coefficients is equal to 0, we can do:

``` r
hypotheses(mod, hypothesis = c(0, 1, 1))
#> 
#>    Term Estimate Std. Error    z Pr(>|z|)   S 2.5 % 97.5 %
#>  custom     1.31      0.593 2.22   0.0266 5.2 0.153   2.48
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

See below for more example of how to use string formulas, numeric
vectors, or matrices to calculate custom contrasts, linear combinations,
and linear or non-linear hypothesis tests.

### Arbitrary quantities with data frames

`marginaleffects` can also compute uncertainty estimates for arbitrary
quantities hosted in a data frame, as long as the user can supply a
variance-covariance matrix. (Thanks to Kyle F Butts for this cool
feature and example!)

Say you run a monte-carlo simulation and you want to perform hypothesis
of various quantities returned from each simulation. The quantities are
correlated within each draw:

``` r
# simulated means and medians
draw <- function(i) { 
  x <- rnorm(n = 10000, mean = 0, sd = 1)
  out <- data.frame(median = median(x), mean =  mean(x))
  return(out)
}
sims <- do.call("rbind", lapply(1:25, draw))

# average mean and average median 
coeftable <- data.frame(
  term = c("median", "mean"),
  estimate = c(mean(sims$median), mean(sims$mean))
)

# variance-covariance
vcov <- cov(sims)

# is the median equal to the mean?
hypotheses(
  coeftable,
  vcov = vcov,
  hypothesis = "median = mean"
)
#> 
#>           Term  Estimate Std. Error       z Pr(>|z|)   S   2.5 % 97.5 %
#>  median = mean -0.000673    0.00837 -0.0804    0.936 0.1 -0.0171 0.0157
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

## `hypotheses` Formulas

Each of the 4 core functions of the package support a `hypothesis`
argument which behaves similarly to the `hypotheses()` function. This
argument allows users to specify custom hypothesis tests and contrasts,
in order to test null hypotheses such as:

-   The coefficients *β*<sub>1</sub> and *β*<sub>2</sub> are equal.
-   The marginal effects of *X*<sub>1</sub> and *X*<sub>2</sub> equal.
-   The marginal effect of *X* when *W* = 0 is equal to the marginal
    effect of *X* when *W* = 1.
-   A non-linear function of adjusted predictions is equal to 100.
-   The marginal mean in the control group is equal to the average of
    marginal means in the other 3 treatment arms.
-   Cross-level contrasts: In a multinomial model, the effect of *X* on
    the 1st outcome level is equal to the effect of *X* on the 2nd
    outcome level.

### Marginal effects

For example, let’s fit a model and compute some [marginal effects at the
mean:](slopes.html#marginal-effect-at-the-mean-mem)

``` r
library(marginaleffects)

mod <- lm(mpg ~ am + vs, data = mtcars)

mfx <- slopes(mod, newdata = "mean")
mfx
#> 
#>  Term Contrast Estimate Std. Error    z Pr(>|z|)    S 2.5 % 97.5 %
#>    am    1 - 0     6.07       1.27 4.76   <0.001 19.0  3.57   8.57
#>    vs    1 - 0     6.93       1.26 5.49   <0.001 24.6  4.46   9.40
#> 
#> Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted, mpg, am, vs 
#> Type:  response
```

Is the marginal effect of `am` different from the marginal effect of
`vs`? To answer this question we can run a linear hypothesis test using
the `hypotheses` function:

``` r
hypotheses(mfx, hypothesis = "am = vs")
#> 
#>   Term Estimate Std. Error      z Pr(>|z|)   S 2.5 % 97.5 %
#>  am=vs   -0.863       1.94 -0.445    0.656 0.6 -4.66   2.94
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

Alternatively, we can specify the hypothesis directly in the original
call:

``` r
library(marginaleffects)

mod <- lm(mpg ~ am + vs, data = mtcars)

slopes(
    mod,
    newdata = "mean",
    hypothesis = "am = vs")
#> 
#>   Term Estimate Std. Error      z Pr(>|z|)   S 2.5 % 97.5 %
#>  am=vs   -0.863       1.94 -0.445    0.656 0.6 -4.66   2.94
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

The `hypotheses` string can include any valid `R` expression, so we can
run some silly non-linear tests:

``` r
slopes(
    mod,
    newdata = "mean",
    hypothesis = "exp(am) - 2 * vs = -400")
#> 
#>               Term Estimate Std. Error    z Pr(>|z|)   S 2.5 % 97.5 %
#>  exp(am)-2*vs=-400      817        550 1.49    0.137 2.9  -261   1896
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

But note that the p values and confidence intervals are calculated using
the delta method and are thus based on the assumption that the
`hypotheses` expression is approximately normally distributed. For
(very) non-linear functions of the parameters, this is not realistic,
and we get p values with incorrect error rates and confidence intervals
with incorrect coverage probabilities. For such hypotheses, it’s better
to calculate the confidence intervals using the bootstrap (see
[`inferences`](reference/inferences.html) for details):

``` r
set.seed(1234)
slopes(
    mod,
    newdata = "mean",
    hypothesis = "exp(am) - 2 * vs = -400") |>
  inferences(method = "boot")
#> 
#>               Term Estimate Std. Error 2.5 % 97.5 %
#>  exp(am)-2*vs=-400      817       1854   414   6990
#> 
#> Columns: term, estimate, std.error, conf.low, conf.high 
#> Type:  response
```

While the confidence interval from the delta method is symmetric (equal
to the estimate ± 1.96 times the standard error), the more reliable
confidence interval from the bootstrap is (here) highly skewed.

### Adjusted Predictions

Now consider the case of adjusted predictions:

``` r
p <- predictions(
    mod,
    newdata = datagrid(am = 0:1, vs = 0:1))
p
#> 
#>  am vs Estimate Std. Error    z Pr(>|z|)     S 2.5 % 97.5 %
#>   0  0     14.6      0.926 15.8   <0.001 183.4  12.8   16.4
#>   0  1     21.5      1.130 19.0   <0.001 266.3  19.3   23.7
#>   1  0     20.7      1.183 17.5   <0.001 224.5  18.3   23.0
#>   1  1     27.6      1.130 24.4   <0.001 435.0  25.4   29.8
#> 
#> Columns: rowid, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, mpg, am, vs 
#> Type:  response
```

Since there is no `term` column in the output of the `predictions`
function, we must use parameter identifiers like `b1`, `b2`, etc. to
determine which estimates we want to compare:

``` r
hypotheses(p, hypothesis = "b1 = b2")
#> 
#>   Term Estimate Std. Error     z Pr(>|z|)    S 2.5 % 97.5 %
#>  b1=b2    -6.93       1.26 -5.49   <0.001 24.6  -9.4  -4.46
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

Or directly:

``` r
predictions(
    mod,
    hypothesis = "b1 = b2",
    newdata = datagrid(am = 0:1, vs = 0:1))
#> 
#>   Term Estimate Std. Error     z Pr(>|z|)    S 2.5 % 97.5 %
#>  b1=b2    -6.93       1.26 -5.49   <0.001 24.6  -9.4  -4.46
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response

p$estimate[1] - p$estimate[2]
#> [1] -6.929365
```

In the next section, we will see that we can get equivalent results by
using a vector of contrast weights, which will be used to compute a
linear combination of estimates:

``` r
predictions(
    mod,
    hypothesis = c(1, -1, 0, 0),
    newdata = datagrid(am = 0:1, vs = 0:1))
#> 
#>    Term Estimate Std. Error     z Pr(>|z|)    S 2.5 % 97.5 %
#>  custom    -6.93       1.26 -5.49   <0.001 24.6  -9.4  -4.46
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

There are *many* more possibilities:

``` r
predictions(
    mod,
    hypothesis = "b1 + b2 = 30",
    newdata = datagrid(am = 0:1, vs = 0:1))
#> 
#>      Term Estimate Std. Error    z Pr(>|z|)    S 2.5 % 97.5 %
#>  b1+b2=30     6.12       1.64 3.74   <0.001 12.4  2.91   9.32
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response

p$estimate[1] + p$estimate[2] - 30
#> [1] 6.118254

predictions(
    mod,
    hypothesis = "(b2 - b1) / (b3 - b2) = 0",
    newdata = datagrid(am = 0:1, vs = 0:1))
#> 
#>               Term Estimate Std. Error      z Pr(>|z|)   S 2.5 % 97.5 %
#>  (b2-b1)/(b3-b2)=0    -8.03         17 -0.473    0.636 0.7 -41.3   25.2
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

### Average contrasts or marginal effects

The standard workflow with the `marginaleffects` package is to call a
function like `predictions()`, `slopes()` or `comparisons()` to compute
unit-level quantities; or one of their cousins `avg_predictions()`,
`avg_comparisons()`, or `avg_slopes()` to aggregate the unit-level
quantities into “Average Marginal Effects” or “Average Contrasts.” We
can also use the `comparison` argument to emulate the behavior of the
`avg_*()` functions.

First, note that these three commands produce the same results:

``` r
comparisons(mod, variables = "vs")$estimate |> mean()
#> [1] 6.929365

avg_comparisons(mod, variables = "vs")
#> 
#>  Term Contrast Estimate Std. Error    z Pr(>|z|)    S 2.5 % 97.5 %
#>    vs    1 - 0     6.93       1.26 5.49   <0.001 24.6  4.46    9.4
#> 
#> Columns: term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response

comparisons(
    mod,
    variables = "vs",
    comparison = "differenceavg")
#> 
#>  Term          Contrast Estimate Std. Error    z Pr(>|z|)    S 2.5 % 97.5 %
#>    vs mean(1) - mean(0)     6.93       1.26 5.49   <0.001 24.6  4.46    9.4
#> 
#> Columns: term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted 
#> Type:  response
```

[See the transformations section of the Contrasts vignette for more
details.](comparisons.html)

With these results in hand, we can now conduct a linear hypothesis test
between average marginal effects:

``` r
comparisons(
    mod,
    hypothesis = "am = vs",
    comparison = "differenceavg")
#> 
#>   Term Estimate Std. Error      z Pr(>|z|)   S 2.5 % 97.5 %
#>  am=vs   -0.863       1.94 -0.445    0.656 0.6 -4.66   2.94
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

Computing contrasts between average marginal effects requires a little
care to obtain the right scale. In particular, we need to specify both
the `variables` and the `comparison`:

``` r
comparisons(
    mod,
    hypothesis = "am = vs",
    variables = c("am", "vs"),
    comparison = "dydxavg")
#> 
#>   Term Estimate Std. Error      z Pr(>|z|)   S 2.5 % 97.5 %
#>  am=vs   -0.863       1.94 -0.445    0.656 0.6 -4.66   2.94
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

### Generic Hypothesis for Unsupported S3 Objects

`marginaleffects` provides a generic interface for hypothesis tests for
linear models by providing (1) a data.frame containing point estimates
(consiting of columns `term` containing the names and `estimate`
containing the point estiamtes) and (2) a variance-covariance matrix of
estimates.

``` r
coeftable <- data.frame(term = names(mod$coefficients), estimate = as.numeric(mod$coefficients))
vcov <- vcov(mod)

hypotheses(
  coeftable, vcov = vcov, 
  hypothesis = "am = vs"
)
#> 
#>     Term Estimate Std. Error      z Pr(>|z|)   S 2.5 % 97.5 %
#>  am = vs   -0.863       1.94 -0.445    0.656 0.6 -4.66   2.94
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

## `hypotheses` Vectors and Matrices

The `marginal_means()` function computes [estimated marginal
means.](marginalmeans.html) The `hypothesis` argument of that function
offers a powerful mechanism to estimate custom contrasts between
marginal means, by way of linear combination.

Consider a simple example:

``` r
library(marginaleffects)
library(emmeans)
library(nnet)

dat <- mtcars
dat$carb <- factor(dat$carb)
dat$cyl <- factor(dat$cyl)
dat$am <- as.logical(dat$am)

mod <- lm(mpg ~ carb + cyl, dat)
mm <- marginal_means(mod, variables = "carb")
mm
#> 
#>  Term Value Mean Std. Error     z Pr(>|z|)     S 2.5 % 97.5 %
#>  carb     1 21.7       1.44 15.06   <0.001 167.8  18.8   24.5
#>  carb     2 21.3       1.23 17.29   <0.001 220.0  18.9   23.8
#>  carb     3 21.4       2.19  9.77   <0.001  72.5  17.1   25.7
#>  carb     4 18.9       1.21 15.59   <0.001 179.7  16.5   21.3
#>  carb     6 19.8       3.55  5.56   <0.001  25.2  12.8   26.7
#>  carb     8 20.1       3.51  5.73   <0.001  26.6  13.2   27.0
#> 
#> Results averaged over levels of: cyl, carb 
#> Columns: term, value, carb, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

The contrast between marginal means for `carb==1` and `carb==2` is:

``` r
21.66232 - 21.34058 
#> [1] 0.32174
```

or

``` r
21.66232 + -(21.34058)
#> [1] 0.32174
```

or

``` r
sum(c(21.66232, 21.34058) * c(1, -1))
#> [1] 0.32174
```

or

``` r
c(21.66232, 21.34058) %*% c(1, -1)
#>         [,1]
#> [1,] 0.32174
```

The last two commands express the contrast of interest as [a linear
combination](https://en.wikipedia.org/wiki/Linear_combination) of
marginal means.

### Simple contrast

In the `marginal_means()` function, we can supply a `hypothesis`
argument to compute linear combinations of marginal means. This argument
must be a numeric vector of the same length as the number of rows in the
output of `marginal_means()`. For example, in the previous there were
six rows, and the two marginal means we want to compare are at in the
first two positions:

``` r
lc <- c(1, -1, 0, 0, 0, 0)
marginal_means(mod, variables = "carb", hypothesis = lc)
#> 
#>    Term  Mean Std. Error     z Pr(>|z|)   S 2.5 % 97.5 %
#>  custom 0.322       1.77 0.181    0.856 0.2 -3.15    3.8
#> 
#> Results averaged over levels of: cyl, carb 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

### Complex contrast

Of course, we can also estimate more complex contrasts:

``` r
lc <- c(0, -2, 1, 1, -1, 1)
marginal_means(mod, variables = "carb", hypothesis = lc)
#> 
#>    Term  Mean Std. Error     z Pr(>|z|)   S 2.5 % 97.5 %
#>  custom -2.02       6.32 -0.32    0.749 0.4 -14.4   10.4
#> 
#> Results averaged over levels of: cyl, carb 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

`emmeans` produces similar results:

``` r
library(emmeans)
em <- emmeans(mod, "carb")
lc <- data.frame(custom_contrast = c(-2, 1, 1, 0, -1, 1))
contrast(em, method = lc)
#>  contrast        estimate   SE df t.ratio p.value
#>  custom_contrast   -0.211 6.93 24  -0.030  0.9760
#> 
#> Results are averaged over the levels of: cyl
```

### Multiple contrasts

Users can also compute multiple linear combinations simultaneously by
supplying a numeric matrix to `hypotheses`. This matrix must have the
same number of rows as the output of `slopes()`, and each column
represents a distinct set of weights for different linear combinations.
The column names of the matrix become labels in the output. For example:

``` r
lc <- matrix(c(
    -2, 1, 1, 0, -1, 1,
    1, -1, 0, 0, 0, 0
    ), ncol = 2)
colnames(lc) <- c("Contrast A", "Contrast B")
lc
#>      Contrast A Contrast B
#> [1,]         -2          1
#> [2,]          1         -1
#> [3,]          1          0
#> [4,]          0          0
#> [5,]         -1          0
#> [6,]          1          0

marginal_means(mod, variables = "carb", hypothesis = lc)
#> 
#>        Term   Mean Std. Error       z Pr(>|z|)   S  2.5 % 97.5 %
#>  Contrast A -0.211       6.93 -0.0304    0.976 0.0 -13.79   13.4
#>  Contrast B  0.322       1.77  0.1814    0.856 0.2  -3.15    3.8
#> 
#> Results averaged over levels of: cyl, carb 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

### Contrasts across response levels

In models with multinomial outcomes, one may be interested in comparing
outcomes or contrasts across response levels. For example, in this model
there are 18 estimated marginal means, across 6 outcome levels (the
`group` column):

``` r
library(nnet)
mod <- multinom(carb ~ mpg + cyl, data = dat, trace = FALSE)
mm <- marginal_means(mod, type = "probs")
mm
#> 
#>  Group Term Value     Mean Std. Error       z Pr(>|z|)   S     2.5 %   97.5 %
#>      1  cyl     4 3.68e-01   2.60e-01 1.41647   0.1566 2.7 -1.41e-01 8.77e-01
#>      1  cyl     6 2.83e-01   1.96e-01 1.44488   0.1485 2.8 -1.01e-01 6.67e-01
#>      1  cyl     8 4.63e-04   9.59e-03 0.04824   0.9615 0.1 -1.83e-02 1.93e-02
#>      2  cyl     4 6.31e-01   2.60e-01 2.42772   0.0152 6.0  1.22e-01 1.14e+00
#>      2  cyl     6 1.85e-06   2.40e-06 0.77081   0.4408 1.2 -2.85e-06 6.55e-06
#>      2  cyl     8 6.65e-01   3.74e-01 1.77977   0.0751 3.7 -6.74e-02 1.40e+00
#>      3  cyl     4 6.85e-04   1.19e-02 0.05748   0.9542 0.1 -2.27e-02 2.40e-02
#>      3  cyl     6 1.12e-05   1.32e-03 0.00848   0.9932 0.0 -2.58e-03 2.60e-03
#>      3  cyl     8 3.10e-01   3.71e-01 0.83684   0.4027 1.3 -4.17e-01 1.04e+00
#>      4  cyl     4 2.12e-04   1.75e-02 0.01211   0.9903 0.0 -3.41e-02 3.45e-02
#>      4  cyl     6 5.56e-01   2.18e-01 2.55023   0.0108 6.5  1.29e-01 9.84e-01
#>      4  cyl     8 9.58e-03   2.28e-02 0.42007   0.6744 0.6 -3.51e-02 5.43e-02
#>      6  cyl     4 8.82e-06   8.39e-05 0.10506   0.9163 0.1 -1.56e-04 1.73e-04
#>      6  cyl     6 1.61e-01   1.54e-01 1.04698   0.2951 1.8 -1.40e-01 4.62e-01
#>      6  cyl     8 4.35e-09   9.89e-08 0.04393   0.9650 0.1 -1.90e-07 1.98e-07
#>      8  cyl     4 1.50e-04   7.97e-03 0.01878   0.9850 0.0 -1.55e-02 1.58e-02
#>      8  cyl     6 9.29e-06   7.98e-04 0.01164   0.9907 0.0 -1.56e-03 1.57e-03
#>      8  cyl     8 1.41e-02   4.66e-02 0.30323   0.7617 0.4 -7.72e-02 1.05e-01
#> 
#> Results averaged over levels of: mpg, cyl 
#> Columns: group, term, value, cyl, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  probs
```

Let’s contrast the marginal means in the first outcome level when `cyl`
equals 4 and 6. These marginal means are located in rows 1 and 7
respectively:

``` r
lc <- rep(0, nrow(mm))
lc[1] <- -1
lc[7] <- 1
marginal_means(
    mod,
    type = "probs",
    hypothesis = lc)
#> 
#>    Term   Mean Std. Error     z Pr(>|z|)   S  2.5 % 97.5 %
#>  custom -0.367       0.26 -1.41    0.158 2.7 -0.877  0.143
#> 
#> Results averaged over levels of: mpg, cyl 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  probs
```

This is indeed equal to the results we would have obtained manually:

``` r
2.828726e-01 - 3.678521e-01
#> [1] -0.0849795
```

Now let’s say we want to calculate a “contrast in contrasts”, that is,
the outcome of a 3-step process:

1.  Contrast between `cyl=6` and `cyl=4` in the 1st outcome level
2.  Contrast between `cyl=6` and `cyl=4` in the 2nd outcome level
3.  Contrast between the contrasts defined in steps 1 and 2.

We create the linear combination weights as follows:

``` r
lc <- rep(0, nrow(mm))
lc[c(1, 8)] <- -1
lc[c(7, 2)] <- 1
```

To make sure that the weights are correct, we can display them side by
side with the original `marginal_means()` output:

``` r
transform(mm[, 1:3], lc = lc)
#>    group term value lc
#> 1      1  cyl     4 -1
#> 2      1  cyl     6  1
#> 3      1  cyl     8  0
#> 4      2  cyl     4  0
#> 5      2  cyl     6  0
#> 6      2  cyl     8  0
#> 7      3  cyl     4  1
#> 8      3  cyl     6 -1
#> 9      3  cyl     8  0
#> 10     4  cyl     4  0
#> 11     4  cyl     6  0
#> 12     4  cyl     8  0
#> 13     6  cyl     4  0
#> 14     6  cyl     6  0
#> 15     6  cyl     8  0
#> 16     8  cyl     4  0
#> 17     8  cyl     6  0
#> 18     8  cyl     8  0
```

Compute the results:

``` r
marginal_means(mod, type = "probs", hypothesis = lc)
#> 
#>    Term    Mean Std. Error      z Pr(>|z|)   S  2.5 % 97.5 %
#>  custom -0.0843      0.321 -0.263    0.793 0.3 -0.714  0.545
#> 
#> Results averaged over levels of: mpg, cyl 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  probs
```

## Pairwise contrasts: Difference-in-Differences

Now we illustrate how to use the machinery described above to do
pairwise comparisons between contrasts, a type of analysis often
associated with a “Difference-in-Differences” research design.

First, we simulate data with two treatment groups and pre/post periods:

``` r
library(data.table)

N <- 1000
did <- data.table(
    id = 1:N,
    pre = rnorm(N),
    trt = sample(0:1, N, replace = TRUE))
did$post <- did$pre + did$trt * 0.3 + rnorm(N)
did <- melt(
    did,
    value.name = "y",
    variable.name = "time",
    id.vars = c("id", "trt"))
head(did)
#>       id   trt   time           y
#>    <int> <int> <fctr>       <num>
#> 1:     1     1    pre -1.04356113
#> 2:     2     0    pre -0.99460367
#> 3:     3     0    pre -0.16962798
#> 4:     4     1    pre -0.01854487
#> 5:     5     0    pre -1.37156492
#> 6:     6     0    pre  0.33690893
```

Then, we estimate a linear model with a multiple interaction between the
time and the treatment indicators. We also compute contrasts at the mean
for each treatment level:

``` r
did_model <- lm(y ~ time * trt, data = did)

comparisons(
    did_model,
    newdata = datagrid(trt = 0:1),
    variables = "time")
#> 
#>  Term   Contrast trt Estimate Std. Error      z Pr(>|z|)    S  2.5 % 97.5 % time
#>  time post - pre   0   -0.035     0.0821 -0.426     0.67  0.6 -0.196  0.126  pre
#>  time post - pre   1    0.298     0.0792  3.757   <0.001 12.5  0.142  0.453  pre
#> 
#> Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, trt, predicted_lo, predicted_hi, predicted, y, time 
#> Type:  response
```

Finally, we compute pairwise differences between contrasts. This is the
Diff-in-Diff estimate:

``` r
comparisons(
    did_model,
    variables = "time",
    newdata = datagrid(trt = 0:1),
    hypothesis = "pairwise")
#> 
#>           Term Estimate Std. Error     z Pr(>|z|)   S  2.5 % 97.5 %
#>  Row 1 - Row 2   -0.333      0.114 -2.92  0.00356 8.1 -0.556 -0.109
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

## Joint hypotheses tests

The `hypotheses()` function can also test multiple hypotheses jointly.
For example, consider this model:

``` r
model <- lm(mpg ~ as.factor(cyl) * hp, data = mtcars)
coef(model)
#>        (Intercept)    as.factor(cyl)6    as.factor(cyl)8                 hp as.factor(cyl)6:hp as.factor(cyl)8:hp 
#>        35.98302564       -15.30917451       -17.90295193        -0.11277589         0.10516262         0.09853177
```

We may want to test the null hypothesis that two of the coefficients are
jointly (both) equal to zero.

``` r

hypotheses(model, joint = c("as.factor(cyl)6:hp", "as.factor(cyl)8:hp"))
#> 
#> 
#> Joint hypothesis test:
#> as.factor(cyl)6:hp = 0
#> as.factor(cyl)8:hp = 0
#>  
#>     F Pr(>|F|) Df 1 Df 2
#>  2.11    0.142    2   26
#> 
#> Columns: statistic, p.value, df1, df2
```

The `joint` argument allows users to flexibly specify the parameters to
be tested, using character vectors, integer indices, or Perl-compatible
regular expressions. We can also specify the null hypothesis for each
parameter individually using the `hypothesis` argument.

Naturally, the `hypotheses` function also works with `marginaleffects`
objects.

``` r
# ## joint hypotheses: regular expression
hypotheses(model, joint = "cyl")
#> 
#> 
#> Joint hypothesis test:
#>  as.factor(cyl)6 = 0
#>  as.factor(cyl)8 = 0
#>  as.factor(cyl)6:hp = 0
#>  as.factor(cyl)8:hp = 0
#>  
#>    F Pr(>|F|) Df 1 Df 2
#>  5.7  0.00197    4   26
#> 
#> Columns: statistic, p.value, df1, df2

## joint hypotheses: integer indices
hypotheses(model, joint = 2:3)
#> 
#> 
#> Joint hypothesis test:
#>  as.factor(cyl)6 = 0
#>  as.factor(cyl)8 = 0
#>  
#>     F Pr(>|F|) Df 1 Df 2
#>  6.12  0.00665    2   26
#> 
#> Columns: statistic, p.value, df1, df2

## joint hypotheses: different null hypotheses
hypotheses(model, joint = 2:3, hypothesis = 1)
#> 
#> 
#> Joint hypothesis test:
#>  as.factor(cyl)6 = 1
#>  as.factor(cyl)8 = 1
#>  
#>     F Pr(>|F|) Df 1 Df 2
#>  6.84  0.00411    2   26
#> 
#> Columns: statistic, p.value, df1, df2
hypotheses(model, joint = 2:3, hypothesis = 1:2)
#> 
#> 
#> Joint hypothesis test:
#>  as.factor(cyl)6 = 1
#>  as.factor(cyl)8 = 2
#>  
#>     F Pr(>|F|) Df 1 Df 2
#>  7.47  0.00273    2   26
#> 
#> Columns: statistic, p.value, df1, df2

## joint hypotheses: marginaleffects object
cmp <- avg_comparisons(model)
hypotheses(cmp, joint = "cyl")
#> 
#> 
#> Joint hypothesis test:
#>  cyl 6 - 4 = 0
#>  cyl 8 - 4 = 0
#>  
#>    F Pr(>|F|) Df 1 Df 2
#>  1.6    0.221    2   26
#> 
#> Columns: statistic, p.value, df1, df2
```

We can also combine multiple calls to `hypotheses` to execute a joint
test on linear combinations of coefficients:

``` r
## fit model
mod <- lm(mpg ~ factor(carb), mtcars)

## hypothesis matrix for linear combinations
H <- matrix(0, nrow = length(coef(mod)), ncol = 2)
H[2:3, 1] <- H[4:6, 2] <- 1

## test individual linear combinations
hyp <- hypotheses(mod, hypothesis = H)
hyp
#> 
#>    Term Estimate Std. Error     z Pr(>|z|)   S 2.5 % 97.5 %
#>  custom    -12.0       4.92 -2.44  0.01477 6.1 -21.6  -2.35
#>  custom    -25.5       9.03 -2.83  0.00466 7.7 -43.2  -7.85
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high

## test joint hypotheses
#hypotheses(hyp, joint = TRUE, hypothesis = c(-10, -20))
```

## Complex aggregations

The `FUN` argument of the `hypotheses()` function can be used to conduct
complex aggregations of other estimates. For example, consider this
ordered logit model fitted on a dataset of cars:

``` r
library(MASS)
library(dplyr)

dat <- transform(mtcars, gear = factor(gear))
mod <- polr(gear ~ factor(cyl) + hp, dat)
summary(mod)
#> Call:
#> polr(formula = gear ~ factor(cyl) + hp, data = dat)
#> 
#> Coefficients:
#>                  Value Std. Error t value
#> factor(cyl)6  -3.87912    1.52625  -2.542
#> factor(cyl)8 -14.64228    4.46072  -3.282
#> hp             0.07269    0.02422   3.001
#> 
#> Intercepts:
#>     Value    Std. Error t value 
#> 3|4   3.6824   1.7945     2.0521
#> 4|5   7.3814   2.3473     3.1445
#> 
#> Residual Deviance: 34.40969 
#> AIC: 44.40969
```

If we compute fitted values with the `predictions()` function, we obtain
one predicted probability for each individual car and for each level of
the response variable:

``` r
predictions(mod)
#> 
#>  Group Estimate Std. Error      z Pr(>|z|)   S   2.5 % 97.5 %
#>      3   0.3931    0.19125   2.06  0.03982 4.7  0.0183  0.768
#>      3   0.3931    0.19125   2.06  0.03982 4.7  0.0183  0.768
#>      3   0.0440    0.04256   1.03  0.30081 1.7 -0.0394  0.127
#>      3   0.3931    0.19125   2.06  0.03982 4.7  0.0183  0.768
#>      3   0.9963    0.00721 138.17  < 0.001 Inf  0.9822  1.010
#> --- 86 rows omitted. See ?avg_predictions and ?print.marginaleffects --- 
#>      5   0.6969    0.18931   3.68  < 0.001 12.1  0.3258  1.068
#>      5   0.0555    0.06851   0.81  0.41775  1.3 -0.0788  0.190
#>      5   0.8115    0.20626   3.93  < 0.001 13.5  0.4073  1.216
#>      5   0.9111    0.16818   5.42  < 0.001 24.0  0.5815  1.241
#>      5   0.6322    0.19648   3.22  0.00129  9.6  0.2471  1.017
#> Columns: rowid, group, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, gear, cyl, hp 
#> Type:  probs
```

There are three levels to the outcome: 3, 4, and 5. Imagine that, for
each car in the dataset, we want to collapse categories of the output
variable into two categories (“3 & 4” and “5”) by taking sums of
predicted probabilities. Then, we want to take the average of those
predicted probabilities for each level of `cyl`. To do so, we define a
custom function, and pass it to the `FUN` argument of the `hypotheses()`
function:

``` r
aggregation_fun <- function(model) {
    predictions(model, vcov = FALSE) |>
        # label the new categories of outcome levels
        mutate(group = ifelse(group %in% c("3", "4"), "3 & 4", "5")) |>
        # sum of probabilities at the individual level
        summarize(estimate = sum(estimate), .by = c("rowid", "cyl", "group")) |>
        # average probabilities for each value of `cyl`
        summarize(estimate = mean(estimate), .by = c("cyl", "group")) |>
        # the `FUN` argument requires a `term` column
        rename(term = cyl)
}

hypotheses(mod, FUN = aggregation_fun)
#> 
#>  Group Term Estimate Std. Error     z Pr(>|z|)     S  2.5 % 97.5 %
#>  3 & 4    6   0.8390     0.0651 12.89   <0.001 123.9 0.7115  0.967
#>  3 & 4    4   0.7197     0.1099  6.55   <0.001  34.0 0.5044  0.935
#>  3 & 4    8   0.9283     0.0174 53.45   <0.001   Inf 0.8943  0.962
#>  5        6   0.1610     0.0651  2.47   0.0134   6.2 0.0334  0.289
#>  5        4   0.2803     0.1099  2.55   0.0108   6.5 0.0649  0.496
#>  5        8   0.0717     0.0174  4.13   <0.001  14.7 0.0377  0.106
#> 
#> Columns: term, group, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

Note that this workflow will not work for bayesian models or with
bootstrap. However, with those models it is trivial to do the same kind
of aggregation by calling `posterior_draws()` and operating directly on
draws from the posterior distribution. See the vignette on bayesian
analysis for examples with the `posterior_draws()` function.
