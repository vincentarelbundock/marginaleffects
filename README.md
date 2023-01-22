
# The `marginaleffects` package for `R` <img src="https://user-images.githubusercontent.com/987057/134899484-e3392510-2e94-4c39-9830-53356fa5feed.png" align="right" alt="" width="120" />

<!--
[![Codecov test coverage](https://codecov.io/gh/vincentarelbundock/marginaleffects/branch/main/graph/badge.svg)](https://app.codecov.io/gh/vincentarelbundock/marginaleffects?branch=main)
[![R-CMD-check](https://github/To cl.com/vincentarelbundock/marginaleffects/workflows/R-CMD-check/badge.svg)](https://github.com/vincentarelbundock/marginaleffects/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/marginaleffects)](https://CRAN.R-project.org/package=marginaleffects)
[![status](https://tinyverse.netlify.com/badge/marginaleffects)](https://CRAN.R-project.org/package=marginaleffects)
-->

Compute and plot predictions, slopes, marginal means, and comparisons
(contrasts, risk ratios, etc.) for over 70 classes of statistical models
in R. Conduct linear and non-linear hypothesis tests using the delta
method.

The code on this website was executed using `marginaleffects`
0.8.1.9112. See the [installation
section](https://vincentarelbundock.github.io/marginaleffects/#installation)
to install the latest CRAN or development version.

## Table of contents

Introduction:

  - [Motivation](https://vincentarelbundock.github.io/marginaleffects/#motivation)
  - [Installation](https://vincentarelbundock.github.io/marginaleffects/#installation)
  - Interpreting model estimates:
    1.  [Estimands: Predictions, Comparisons, and
        Slopes](https://vincentarelbundock.github.io/marginaleffects/#estimands-predictions-comparisons-and-slopes)
    2.  [Prediction
        Grid](https://vincentarelbundock.github.io/marginaleffects/#prediction-grid)
    3.  [Averaging](https://vincentarelbundock.github.io/marginaleffects/#averaging)
    4.  [Hypothesis Tests and Custom
        Contrasts](https://vincentarelbundock.github.io/marginaleffects/#hypothesis-tests)

Vignettes:

  - [Predictions](https://vincentarelbundock.github.io/marginaleffects/articles/predictions.html)
  - [Comparisons](https://vincentarelbundock.github.io/marginaleffects/articles/contrasts.html)
  - [Slopes](https://vincentarelbundock.github.io/marginaleffects/articles/slopes.html)
  - [Marginal
    Means](https://vincentarelbundock.github.io/marginaleffects/articles/marginalmeans.html)
  - [Hypothesis Tests and Custom
    Contrasts](https://vincentarelbundock.github.io/marginaleffects/articles/hypothesis.html)

Case studies:

  - [Bayesian Analyses with
    `brms`](https://vincentarelbundock.github.io/marginaleffects/articles/brms.html)
  - [Causal Inference with the
    g-Formula](https://vincentarelbundock.github.io/marginaleffects/articles/gformula.html)
  - [Elasticity](https://vincentarelbundock.github.io/marginaleffects/articles/elasticity.html)
  - [Experiments](https://vincentarelbundock.github.io/marginaleffects/articles/experiments.html)
  - [Generalized Additive
    Models](https://vincentarelbundock.github.io/marginaleffects/articles/gam.html)
  - [Mixed effects
    models](https://vincentarelbundock.github.io/marginaleffects/articles/lme4.html)
  - [Multinomial Logit and Discrete Choice
    Models](https://vincentarelbundock.github.io/marginaleffects/articles/mlogit.html)
  - [Multiple
    Imputation](https://vincentarelbundock.github.io/marginaleffects/articles/multiple_imputation.html)
  - [Plots: interactions, predictions, contrasts, and
    slopes](https://vincentarelbundock.github.io/marginaleffects/articles/plot.html)
  - [Python NumPyro models in
    `marginaleffects`](https://vincentarelbundock.github.io/marginaleffects/articles/python.html)
  - [Unit-level contrasts in logistic
    regressions](https://vincentarelbundock.github.io/marginaleffects/articles/logistic_contrasts.html)

Tips and technical notes:

  - [73 Supported Classes of
    Models](https://vincentarelbundock.github.io/marginaleffects/articles/supported_models.html)
  - [Index of Functions and
    Documentation](https://vincentarelbundock.github.io/marginaleffects/reference/index.html)
  - [Extending `marginaleffects`: add new models or modify existing
    ones](https://vincentarelbundock.github.io/marginaleffects/articles/extensions.html)
  - [Standard Errors and Confidence
    Intervals](https://vincentarelbundock.github.io/marginaleffects/articles/sandwich.html)
  - [Tables and
    Plots](https://vincentarelbundock.github.io/marginaleffects/articles/modelsummary.html)
  - [Performance](https://vincentarelbundock.github.io/marginaleffects/articles/performance.html)
  - [Alternative
    Software](https://vincentarelbundock.github.io/marginaleffects/articles/alternative_software.html)
  - [Frequently Asked
    Questions](https://vincentarelbundock.github.io/marginaleffects/articles/faq.html)

External links:

  - [Marginal and conditional effects for GLMMs with
    `marginaleffects`](https://www.andrewheiss.com/blog/2022/11/29/conditional-marginal-marginaleffects/)
    by Andrew Heiss
  - [Marginalia: A guide to figuring out what the heck marginal effects,
    marginal slopes, average marginal effects, marginal effects at the
    mean, and all these other marginal things
    are](https://www.andrewheiss.com/blog/2022/05/20/marginalia/) by
    Andrew Heiss
  - [Matching](https://cran.r-project.org/web/packages/MatchIt/vignettes/estimating-effects.html)
    by Noah Greifer
  - [Double propensity score adjustment using
    g-computation](https://stats.stackexchange.com/questions/580118/adjusting-the-model-by-propensity-scores-after-propensity-score-matching/580174#580174)
    by Noah Greifer
  - [Subgroup Analysis After Propensity Score Matching Using
    R](https://ngreifer.github.io/blog/subgroup-analysis-psm/) by Noah
    Greifer
  - [Bayesian model
    averaging](https://www.ajordannafa.com/blog/2022/05/24/bma-ames/) by
    A. Jordan Nafa

## Motivation

Parameter estimates are often hard to interpret substantively,
especially when they are generated by complex models with non-linear
components or transformations. Many applied researchers would rather
focus on simple quantities of interest, which have straightforward
scientific interpretations. Unfortunately, these estimands (and their
standard errors) are tedious to compute. Moreover, the different
modeling packages in `R` often produce inconsistent objects that require
special treatment.

`marginaleffects` offers a single point of entry to easily interpret the
results of over 73 classes of models, using a simple and consistent user
interface.

Benefits of `marginaleffects` include:

  - *Powerful:* It can compute predictions, comparisons (contrasts, risk
    ratios, etc.), slopes, and conduct hypothesis tests for 73 different
    classes of models in `R`.
  - *Simple:* All functions share a simple, unified, and well-documented
    interface.
  - *Efficient:* [Some
    operations](https://vincentarelbundock.github.io/marginaleffects/articles/performance.html)
    are orders of magnitude faster than with the `margins` package, and
    the memory footprint is much smaller.
  - *Valid:* When possible, [numerical results are
    checked](https://vincentarelbundock.github.io/marginaleffects/articles/supported_models.html)
    against alternative software like `Stata` or other `R` packages.
  - *Thin:* Few dependencies.
  - *Standards-compliant:* `marginaleffects` follows “tidy” principles
    and returns objects that work with standard functions like `plot`,
    `summary()`, `tidy()`, and `glance()`. These objects are easy to
    program with and feed to [other packages like
    `modelsummary`.](https://vincentarelbundock.github.io/marginaleffects/)
  - *Extensible:* Adding support for new models is very easy, often
    requiring less than 10 lines of new code. Please submit [feature
    requests on
    Github.](https://github.com/vincentarelbundock/marginaleffects/issues)
  - *Active development*: Bugs are fixed promptly.

Downsides of `marginaleffects` include:

  - No multiplicity adjustments. (Use `p.adjust()` instead.)
  - Marginal means are often slower to compute than with `emmeans`.
  - No omnibus test

If `marginaleffects` does not meet your needs, I recommend you try
[`emmeans`](https://github.com/rvlenth/emmeans) or one of the other
[alternative `R`
packages.](https://vincentarelbundock.github.io/marginaleffects/articles/alternative_software.html)

## Installation

You can install the released version of `marginaleffects` from CRAN:

``` r
install.packages("marginaleffects")
```

You can install the development version of `marginaleffects` (and its
dependency `insight`) from R-Universe:

``` r
install.packages(
    c("marginaleffects", "insight"),
    repos = c(
        "https://vincentarelbundock.r-universe.dev",
        "https://easystats.r-universe.dev"))
```

**Restart `R` completely before moving on.**

## Estimands: Predictions, Comparisons, and Slopes

The `marginaleffects` package allows `R` users to compute and plot three
principal quantities of interest: (1) predictions, (2) comparisons, and
(3) slopes. In addition, the package includes a convenience function to
compute a fourth estimand, “marginal means”, which is a special case of
averaged predictions.

[*Predictions*:](https://vincentarelbundock.github.io/marginaleffects/articles/predictions.html)

> The outcome predicted by a fitted model on a specified scale for a
> given combination of values of the predictor variables, such as their
> observed values, their means, or factor levels. a.k.a. Fitted values,
> adjusted predictions.

[*Comparisons*:](https://vincentarelbundock.github.io/marginaleffects/articles/contrasts.html)

> Compare the predictions made by a model for different regressor values
> (e.g., college graduates vs. others): contrasts, differences, risk
> ratios, odds, etc.

[*Slopes*:](https://vincentarelbundock.github.io/marginaleffects/articles/slopes.html)

> Partial derivative of the regression equation with respect to a
> regressor of interest. a.k.a. Marginal effects, trends.

[*Marginal
Means*:](https://vincentarelbundock.github.io/marginaleffects/articles/marginalmeans.html)

> Predictions of a model, averaged across a “reference grid” of
> categorical predictors.

Predictions, comparisons, and slopes are fundamentally unit-level (or
“conditional”) quantities. Except in the simplest linear case,
estimates will typically vary based on the values of all the regressors
in a model. Each of the observations in a dataset is thus associated
with its own prediction, comparison, and slope estimates. Below, we will
see that it can be useful to marginalize (or “average over”) unit-level
estimates to report an “average prediction”, “average comparison”, or
“average slope”.

One ambiguous aspect of the definitions above is that the word
“marginal” comes up in two different and *opposite* ways:

1.  In “marginal effects,” we refer to the effect of a tiny (marginal)
    change in the regressor on the outcome. This is a slope, or
    derivative.
2.  In “marginal means,” we refer to the process of marginalizing across
    rows of a prediction grid. This is an average, or integral.

On this website and in this package, we reserve the expression “marginal
effect” to mean a “slope” or “partial derivative”.

The `marginaleffects` package includes functions to estimate, average,
plot, and summarize all of the estimands described above. The objects
produced by `marginaleffects` are “tidy”: they produce simple data
frames in “long” format. They are also “standards-compliant” and work
seamlessly with standard functions like `summary()`, `plot()`, `tidy()`,
and `glance()`, as well with [external packages like
`modelsummary`.](https://vincentarelbundock.github.io/marginaleffects/)

| Estimand                                                                                           | Functions                                                                                              |
| -------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------ |
| [Predictions](https://vincentarelbundock.github.io/marginaleffects/articles/predictions.html)      | [`predictions()`](https://vincentarelbundock.github.io/marginaleffects/reference/predictions.html)     |
|                                                                                                    | [`avg_predictions()`](https://vincentarelbundock.github.io/marginaleffects/reference/predictions.html) |
|                                                                                                    | [`plot_cap()`](https://vincentarelbundock.github.io/marginaleffects/reference/plot_cap.html)           |
| [Comparisons](https://vincentarelbundock.github.io/marginaleffects/articles/contrasts.html)        | [`comparisons()`](https://vincentarelbundock.github.io/marginaleffects/reference/comparisons.html)     |
|                                                                                                    | [`avg_comparisons()`](https://vincentarelbundock.github.io/marginaleffects/reference/comparisons.html) |
|                                                                                                    | [`plot_cco()`](https://vincentarelbundock.github.io/marginaleffects/reference/plot_cap.html)           |
|                                                                                                    | [`plot_avg()`](https://vincentarelbundock.github.io/marginaleffects/reference/plot_avg.html)           |
| [Slopes](https://vincentarelbundock.github.io/marginaleffects/articles/slopes.html)                | [`slopes()`](https://vincentarelbundock.github.io/marginaleffects/reference/slopes.html)               |
|                                                                                                    | [`avg_slopes()`](https://vincentarelbundock.github.io/marginaleffects/reference/slopes.html)           |
|                                                                                                    | [`plot_cme()`](https://vincentarelbundock.github.io/marginaleffects/reference/plot_cme.html)           |
|                                                                                                    | [`plot_avg()`](https://vincentarelbundock.github.io/marginaleffects/reference/plot_avg.html)           |
| [Marginal means](https://vincentarelbundock.github.io/marginaleffects/articles/marginalmeans.html) | [`marginalmeans()`](https://vincentarelbundock.github.io/marginaleffects/reference/marginalmeans.html) |

### Examples

We now apply `marginaleffects` functions to compute each of the
estimands described above. First, we fit a linear regression model with
multiplicative interactions:

``` r
library(marginaleffects)

mod <- lm(mpg ~ hp * wt * am, data = mtcars)
```

Then, we call the `predictions()` function. As noted above, predictions
are unit-level estimates, so there is one specific prediction per
observation. By default, the `predictions()` function makes one
prediction per observation in the dataset that was used to fit the
original model. Since `mtcars` has 32 rows, the `predictions()` outcome
also has 32 rows:

``` r
pre <- predictions(mod)

nrow(mtcars)
#> [1] 32

nrow(pre)
#> [1] 32

head(pre)
#>   rowid     type estimate std.error statistic       p.value conf.low conf.high  mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> 1     1 response 22.48857 0.8841487  25.43528 1.027254e-142 20.75567  24.22147 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> 2     2 response 20.80186 1.1942050  17.41900  5.920119e-68 18.46126  23.14246 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> 3     3 response 25.26465 0.7085307  35.65781 1.783452e-278 23.87596  26.65335 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> 4     4 response 20.25549 0.7044641  28.75305 8.296026e-182 18.87477  21.63622 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> 5     5 response 16.99782 0.7118658  23.87784 5.205109e-126 15.60259  18.39305 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> 6     6 response 19.66353 0.8753226  22.46433 9.270636e-112 17.94793  21.37913 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

Now, we use the `comparisons()` function to compute the different in
predicted outcome when each of the predictors is incremented by 1 unit
(one predictor at a time, holding all others constant). Once again,
comparisons are unit-level quantities. And since there are 3 predictors
in the model and our data has 32 rows, we obtain 96 comparisons:

``` r
cmp <- comparisons(mod)

nrow(cmp)
#> [1] 96

head(cmp)
#>   rowid     type term contrast    estimate  std.error statistic     p.value    conf.low     conf.high predicted predicted_hi predicted_lo  mpg cyl disp  hp drat    wt  qsec vs am gear carb    eps
#> 1     1 response   hp       +1 -0.03690556 0.01850171 -1.994710 0.046074491 -0.07316824 -0.0006428654  22.48857     22.47012     22.50702 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4 0.0283
#> 2     2 response   hp       +1 -0.02868936 0.01562861 -1.835695 0.066402820 -0.05932088  0.0019421563  20.80186     20.78751     20.81620 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4 0.0283
#> 3     3 response   hp       +1 -0.04657166 0.02258715 -2.061866 0.039220505 -0.09084166 -0.0023016732  25.26465     25.24137     25.28794 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1 0.0283
#> 4     4 response   hp       +1 -0.04227128 0.01328278 -3.182412 0.001460541 -0.06830506 -0.0162375067  20.25549     20.23436     20.27663 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1 0.0283
#> 5     5 response   hp       +1 -0.03901845 0.01341076 -2.909487 0.003620221 -0.06530307 -0.0127338342  16.99782     16.97831     17.01733 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2 0.0283
#> 6     6 response   hp       +1 -0.03872931 0.01348887 -2.871204 0.004089118 -0.06516702 -0.0122916003  19.66353     19.64417     19.68290 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1 0.0283
```

The `comparisons()` function allows customized queries. For example,
what happens to the predicted outcome when the `hp` variable increases
from 100 to 120?

``` r
cmp <- comparisons(mod, variables = list(hp = c(120, 100)))
head(cmp)
#>   rowid     type term  contrast   estimate std.error statistic     p.value  conf.low   conf.high predicted predicted_hi predicted_lo  mpg cyl disp  hp drat    wt  qsec vs am gear carb    eps
#> 1     1 response   hp 120 - 100 -0.7381111 0.3700342 -1.994710 0.046074489 -1.463365 -0.01285731  22.48857     22.11951     22.85762 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4 0.0283
#> 2     2 response   hp 120 - 100 -0.5737872 0.3125723 -1.835695 0.066402820 -1.186418  0.03884313  20.80186     20.51496     21.08875 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4 0.0283
#> 3     3 response   hp 120 - 100 -0.9314333 0.4517429 -2.061866 0.039220507 -1.816833 -0.04603346  25.26465     24.00722     24.93865 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1 0.0283
#> 4     4 response   hp 120 - 100 -0.8454257 0.2656557 -3.182412 0.001460541 -1.366101 -0.32475013  20.25549     19.83278     20.67821 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1 0.0283
#> 5     5 response   hp 120 - 100 -0.7803690 0.2682153 -2.909487 0.003620221 -1.306061 -0.25467668  16.99782     19.14383     19.92420 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2 0.0283
#> 6     6 response   hp 120 - 100 -0.7745862 0.2697775 -2.871204 0.004089117 -1.303340 -0.24583201  19.66353     19.08259     19.85718 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1 0.0283
```

What happens to the predicted outcome when the `wt` variable increases
by 1 standard deviation about its mean?

``` r
cmp <- comparisons(mod, variables = list(hp = "sd"))
head(cmp)
#>   rowid     type term                contrast  estimate std.error statistic     p.value  conf.low   conf.high predicted predicted_hi predicted_lo  mpg cyl disp  hp drat    wt  qsec vs am gear carb    eps
#> 1     1 response   hp (x + sd/2) - (x - sd/2) -2.530351 1.2685305 -1.994710 0.046074489 -5.016625 -0.04407671  22.48857     19.86942     22.39977 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4 0.0283
#> 2     2 response   hp (x + sd/2) - (x - sd/2) -1.967025 1.0715425 -1.835695 0.066402820 -4.067210  0.13315980  20.80186     18.76581     20.73283 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4 0.0283
#> 3     3 response   hp (x + sd/2) - (x - sd/2) -3.193087 1.5486395 -2.061866 0.039220507 -6.228365 -0.15780929  25.26465     21.16779     24.36088 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1 0.0283
#> 4     4 response   hp (x + sd/2) - (x - sd/2) -2.898240 0.9107057 -3.182412 0.001460541 -4.683191 -1.11329004  20.25549     17.25554     20.15378 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1 0.0283
#> 5     5 response   hp (x + sd/2) - (x - sd/2) -2.675217 0.9194805 -2.909487 0.003620221 -4.477366 -0.87306820  16.99782     16.76492     19.44014 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2 0.0283
#> 6     6 response   hp (x + sd/2) - (x - sd/2) -2.655393 0.9248359 -2.871204 0.004089117 -4.468038 -0.84274738  19.66353     16.72131     19.37670 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1 0.0283
```

The `comparisons()` function also allows users to specify arbitrary
functions of predictions, with the `transform_pre` argument. For
example, what is the average ratio between predicted Miles per Gallon
after an increase of 50 units in Horsepower?

``` r
comparisons(
  mod,
  variables = list(hp = 50),
  transform_pre = "ratioavg")
#>       type term  contrast  estimate  std.error statistic       p.value  conf.low conf.high predicted predicted_hi predicted_lo
#> 1 response   hp mean(+50) 0.9095338 0.02895173  31.41553 1.241931e-216 0.8527894 0.9662781  22.48857     21.56593     23.41121
```

See the [Comparisons vignette for detailed explanations and more
options.](https://vincentarelbundock.github.io/marginaleffects/articles/contrasts.html)

The `slopes()` function allows us to compute the partial derivative of
the outcome equation with respect to each of the predictors. Once again,
we obtain a data frame with 96 rows:

``` r
mfx <- slopes(mod)

nrow(mfx)
#> [1] 96

head(mfx)
#>   rowid     type term    estimate  std.error statistic     p.value    conf.low     conf.high predicted predicted_hi predicted_lo  mpg cyl disp  hp drat    wt  qsec vs am gear carb    eps
#> 1     1 response   hp -0.03690556 0.01850172 -1.994710 0.046074551 -0.07316825 -0.0006428553  22.48857     22.48752     22.48857 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4 0.0283
#> 2     2 response   hp -0.02868936 0.01562861 -1.835695 0.066402771 -0.05932087  0.0019421508  20.80186     20.80105     20.80186 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4 0.0283
#> 3     3 response   hp -0.04657166 0.02258715 -2.061866 0.039220507 -0.09084166 -0.0023016728  25.26465     25.26333     25.26465 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1 0.0283
#> 4     4 response   hp -0.04227128 0.01328278 -3.182412 0.001460541 -0.06830506 -0.0162375066  20.25549     20.25430     20.25549 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1 0.0283
#> 5     5 response   hp -0.03901845 0.01341076 -2.909487 0.003620221 -0.06530307 -0.0127338342  16.99782     16.99671     16.99782 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2 0.0283
#> 6     6 response   hp -0.03872931 0.01348887 -2.871204 0.004089117 -0.06516702 -0.0122916004  19.66353     19.66244     19.66353 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1 0.0283
```

## Prediction grid

Predictions, comparisons, and slopes are typically “conditional”
quantities which depend on the values of all the predictors in the
model. By default, `marginaleffects` functions estimate quantities of
interest for empirical distribution of the data (i.e., for each row of
the original dataset). However, users can specify the exact values of
the predictors they want to investigate by using the `newdata` argument.

`newdata` accepts data frames, shortcut strings, or a call to the
`datagrid()` function. For example, to compute the predicted outcome for
a hypothetical car with all predictors equal to the sample mean or
median, we can do:

``` r
predictions(mod, newdata = "mean")
#>   rowid     type estimate std.error statistic       p.value conf.low conf.high      mpg       hp      wt      am
#> 1     1 response 18.37015 0.6798781  27.01978 8.656522e-161 17.03762  19.70269 20.09062 146.6875 3.21725 0.40625

predictions(mod, newdata = "median")
#>   rowid     type estimate std.error statistic       p.value conf.low conf.high  mpg  hp    wt am
#> 1     1 response 19.37392 0.6464425  29.97006 2.410906e-197 18.10691  20.64092 19.2 123 3.325  0
```

The [`datagrid` function gives us a powerful way to define a grid of
predictors.](https://vincentarelbundock.github.io/marginaleffects/reference/datagrid.html)
All the variables not mentioned explicitly in `datagrid()` are fixed to
their mean or mode:

``` r
predictions(
  mod,
  newdata = datagrid(
    am = c(0, 1),
    wt = range))
#>   rowid     type  estimate std.error statistic      p.value  conf.low conf.high      mpg       hp am    wt
#> 1     1 response 23.259500  2.705934  8.595737 8.273174e-18 17.955966  28.56303 20.09062 146.6875  0 1.513
#> 2     2 response 12.793013  2.978494  4.295128 1.745928e-05  6.955272  18.63075 20.09062 146.6875  0 5.424
#> 3     3 response 27.148334  2.851805  9.519702 1.736766e-21 21.558899  32.73777 20.09062 146.6875  1 1.513
#> 4     4 response  5.901966  5.814985  1.014958 3.101259e-01 -5.495196  17.29913 20.09062 146.6875  1 5.424
```

The same mechanism is available in `comparisons()` and `slopes()`. To
estimate the partial derivative of `mpg` with respect to `wt`, when `am`
is equal to 0 and 1, while other predictors are held at their means:

``` r
slopes(
  mod,
  variables = "wt",
  newdata = datagrid(am = 0:1))
#>   rowid     type term  estimate std.error statistic    p.value  conf.low  conf.high predicted predicted_hi predicted_lo      mpg       hp      wt am       eps
#> 1     1 response   wt -2.676166  1.419297 -1.885558 0.05935449 -5.457937  0.1056037  18.69864     18.69760     18.69864 20.09062 146.6875 3.21725  0 0.0003911
#> 2     2 response   wt -5.432464  2.152370 -2.523946 0.01160458 -9.651031 -1.2138976  17.89006     17.88793     17.89006 20.09062 146.6875 3.21725  1 0.0003911
```

We can also plot how predictions, comparisons, or slopes change across
different values of the predictors using [three powerful plotting
functions:](https://vincentarelbundock.github.io/marginaleffects/articles/plot.html)

  - `plot_cap`: Conditional Adjusted Predictions
  - `plot_cco`: Conditional Comparisons
  - `plot_cme`: Conditional Marginal Effects

For example, this plot shows the outcomes predicted by our model for
different values of the `wt` and `am` variables:

``` r
plot_cap(mod, condition = c("wt", "am"))
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" />

This plot shows how the derivative of `mpg` with respect to `am` varies
as a function of `wt` and `hp`:

``` r
plot_cme(mod, effect = "am", condition = list("wt", "hp" = "threenum"))
```

<img src="man/figures/README-unnamed-chunk-17-1.png" width="100%" />

See this vignette for more information: [Plots, interactions,
predictions, contrasts, and
slopes](https://vincentarelbundock.github.io/marginaleffects/articles/plot.html)

## Averaging

Since predictions, comparisons, and slopes are conditional quantities,
they can be a bit unwieldy. Often, it can be useful to report a
one-number summary instead of one estimate per observation. Instead of
presenting “conditional” estimates, some methodologists recommend
reporting “marginal” estimates, that is, an average of unit-level
estimates.

(This use of the word “marginal” as “averaging” should not be confused
with the term “marginal effect” which, in the econometrics tradition,
correspond to a partial derivative, or the effect of a “small/marginal”
change.)

To marginalize (average over) our unit-level estimates, we can use the
`by` argument or the one of the convenience functions:
`avg_predictions()`, `avg_comparisons()`, or `avg_slopes()`. For
example, both of these commands give us the same result: the average
predicted outcome in the `mtcars` dataset:

``` r
avg_predictions(mod)
#>   Estimate Std. Error     z   Pr(>|z|) 2.5 % 97.5 %
#> 1    20.09     0.3904 51.46 < 2.22e-16 19.33  20.86
#> 
#> Model type:  lm 
#> Prediction type:  response
```

This is equivalent to manual computation by:

``` r
mean(predict(mod))
#> [1] 20.09062
```

The main `marginaleffects` functions all include a `by` argument, which
allows us to marginalize within sub-groups of the data. For example,

``` r
avg_comparisons(mod, by = "am")
#>   Term          Contrast Estimate Std. Error       z  Pr(>|z|)    2.5 %    97.5 % am
#> 1   hp          mean(+1) -0.04364    0.02129 -2.0498 0.0403865 -0.08537 -0.001912  1
#> 2   hp          mean(+1) -0.03426    0.01586 -2.1598 0.0307863 -0.06536 -0.003171  0
#> 3   wt          mean(+1) -6.07176    1.97621 -3.0724 0.0021233 -9.94506 -2.198458  1
#> 4   wt          mean(+1) -2.47990    1.23163 -2.0135 0.0440605 -4.89385 -0.065954  0
#> 5   am mean(1) - mean(0)  1.90290    2.30863  0.8243 0.4097951 -2.62193  6.427728  1
#> 6   am mean(1) - mean(0) -1.38301    2.52499 -0.5477 0.5838789 -6.33191  3.565888  0
#> 
#> Model type:  lm 
#> Prediction type:  response

avg_slopes(mod, by = "cyl")
#>   Term    Contrast Estimate Std. Error       z   Pr(>|z|)    2.5 %    97.5 % cyl
#> 1   hp mean(dY/dX) -0.03667    0.01048 -3.4996 0.00046588 -0.05721 -0.016134   6
#> 2   hp mean(dY/dX) -0.05301    0.01989 -2.6657 0.00768339 -0.09199 -0.014034   4
#> 3   hp mean(dY/dX) -0.02704    0.01738 -1.5553 0.11987297 -0.06110  0.007034   8
#> 4   wt mean(dY/dX) -4.32457    1.39608 -3.0977 0.00195057 -7.06083 -1.588306   6
#> 5   wt mean(dY/dX) -6.44404    1.45832 -4.4188 9.9252e-06 -9.30230 -3.585773   4
#> 6   wt mean(dY/dX) -1.77819    1.00704 -1.7658 0.07743628 -3.75196  0.195572   8
#> 7   am mean(dY/dX) -1.12930    1.60146 -0.7052 0.48070735 -4.26810  2.009513   6
#> 8   am mean(dY/dX)  1.27543    2.63009  0.4849 0.62772027 -3.87945  6.430303   4
#> 9   am mean(dY/dX) -0.54744    2.77930 -0.1970 0.84385090 -5.99477  4.899891   8
#> 
#> Model type:  lm 
#> Prediction type:  response
```

Marginal Means are a special case of predictions, which are marginalized
(or averaged) across a balanced grid of categorical predictors. To
illustrate, we estimate a new model with categorical predictors:

``` r
dat <- mtcars
dat$am <- as.logical(dat$am)
dat$cyl <- as.factor(dat$cyl)
mod_cat <- lm(mpg ~ am + cyl + hp, data = dat)
```

We can compute marginal means manually using the functions already
described:

``` r
avg_predictions(
  mod_cat,
  newdata = datagrid(cyl = unique, am = unique),
  by = "am")
#>      am Estimate Std. Error     z   Pr(>|z|) 2.5 % 97.5 %
#> 1  TRUE    22.48     0.8343 26.94 < 2.22e-16 20.84  24.11
#> 2 FALSE    18.32     0.7854 23.33 < 2.22e-16 16.78  19.86
#> 
#> Model type:  lm 
#> Prediction type:  response
```

For convenience, the `marginaleffects` package also includes a
`marginalmeans()` function:

``` r
marginalmeans(mod_cat, variables = "am")
#>       type term value    am estimate std.error statistic       p.value conf.low conf.high
#> 1 response   am FALSE FALSE 18.31987 0.7853925  23.32575 2.429619e-120 16.78053  19.85921
#> 2 response   am  TRUE  TRUE 22.47772 0.8343346  26.94090 7.291801e-160 20.84246  24.11299
```

[The Marginal Means
vignette](https://vincentarelbundock.github.io/marginaleffects/articles/marginalmeans.html)
offers more detail.

## Hypothesis and equivalence tests

The `hypotheses()` function and the `hypothesis` argument can be used to
conduct linear and non-linear hypothesis tests on model coefficients, or
on any of the quantities computed by the functions introduced above.

Consider this model:

``` r
mod <- lm(mpg ~ qsec * drat, data = mtcars)
coef(mod)
#> (Intercept)        qsec        drat   qsec:drat 
#>  12.3371987  -1.0241183  -3.4371461   0.5973153
```

Can we reject the null hypothesis that the `drat` coefficient is 2 times
the size of the `qsec` coefficient?

``` r
hypotheses(mod, "drat = 2 * qsec")
#>              Term Estimate Std. Error       z Pr(>|z|)  2.5 % 97.5 %
#> 1 drat = 2 * qsec   -1.389      10.78 -0.1289  0.89744 -22.51  19.73
#> 
#> Model type:  lm 
#> Prediction type:
```

We can ask the same question but refer to parameters by position, with
indices `b1`, `b2`, `b3`, etc. and get a nicer printout by using
`summary()`:

``` r
hypotheses(mod, "b3 = 2 * b2") |> summary()
#>          Term Estimate Std. Error       z Pr(>|z|)  2.5 % 97.5 %
#> 1 b3 = 2 * b2   -1.389      10.78 -0.1289  0.89744 -22.51  19.73
#> 
#> Model type:  lm
```

The main functions in `marginaleffects` all have a `hypothesis`
argument, which means that we can do complex model testing. For example,
consider two slope estimates:

``` r
slopes(
  mod,
  variables = "drat",
  newdata = datagrid(qsec = range))
#>   rowid     type term  estimate std.error statistic    p.value   conf.low conf.high predicted predicted_hi predicted_lo      mpg     drat qsec      eps
#> 1     1 response drat  5.223926  3.791061  1.377959 0.16821604 -2.2064175  12.65427  16.27566     16.27679     16.27566 20.09062 3.596563 14.5 0.000217
#> 2     2 response drat 10.241374  5.161440  1.984209 0.04723256  0.1251384  20.35761  25.71863     25.72085     25.71863 20.09062 3.596563 22.9 0.000217
```

Are these two slopes significantly different from one another? To test
this, we can use the `hypothesis` argument:

``` r
slopes(
  mod,
  hypothesis = "b1 = b2",
  variables = "drat",
  newdata = datagrid(qsec = range))
#>       type  term  estimate std.error  statistic   p.value  conf.low conf.high
#> 1 response b1=b2 -5.017448  8.519298 -0.5889509 0.5558942 -21.71497  11.68007
```

Now, imagine that for theoretical (or substantive or clinical) reasons,
we only care about slopes larger than 2. We can use the `hypotheses()`
function to conduct an equivalence test:

``` r
avg_slopes(mod) |> hypotheses(equivalence = c(-2, 2))
#>   Term Estimate Std. Error     z   Pr(>|z|)  2.5 % 97.5 %    p (Inf)  p (Sup)   p (Eq)
#> 1 qsec    1.124     0.4331 2.595  0.0094487 0.2752  1.973 2.7403e-13 0.021585 0.021585
#> 2 drat    7.224     1.3652 5.292 1.2122e-07 4.5484  9.900 7.0624e-12 0.999935 0.999935
#> 
#> Model type:  lm 
#> Prediction type:  response
```

See the [Hypothesis Tests and Custom Contrasts
vignette](https://vincentarelbundock.github.io/marginaleffects/articles/hypothesis.html)
for background, details, and for instructions on how to conduct
hypothesis tests in more complex situations.

## More\!

There is *much* more you can do with `marginaleffects`. Return to the
[Table of
Contents](https://vincentarelbundock.github.io/marginaleffects/#table-of-contents)
to read the vignettes, learn how to report marginal effects and means in
[nice tables with the `modelsummary`
package](https://vincentarelbundock.github.io/modelsummary/), how to
define your own prediction “grid”, and much more. \*\*\*\*
