
# Matching

author: “Vincent Arel-Bundock”

This chapter introduces how to use `marginaleffects` to estimate
treatment effects after pre-processing a dataset to achieve better
covariate balance. The presentation is very short. Readers who seek a
more comprehensive understanding and application of these methods should
refer to [Noah Greifer’s excellent and detailed work on the
topic](https://ngreifer.github.io/) and to the [`MatchIt` package
vignettes and website](https://kosukeimai.github.io/MatchIt/)

The procedure we highlight can be broken down into three steps:

1.  Use `MatchIt` to pre-process the data and achieve better covariate
    balance
2.  Fit a regression model to the outcome of interest
3.  Use `marginaleffects` and
    [G-Computation](https://marginaleffects.com/vignettes/gcomputation.html)
    to estimate a quantity of interest, such as the Average treatment
    effect on the treated (ATT)

To begin, we load libraries and the data from the classic Lalonde
experiment:

``` r
library("MatchIt")
library("marginaleffects")
data("lalonde", package = "MatchIt")

head(lalonde)
```

         treat age educ   race married nodegree re74 re75       re78
    NSW1     1  37   11  black       1        1    0    0  9930.0460
    NSW2     1  22    9 hispan       0        1    0    0  3595.8940
    NSW3     1  30   12  black       0        0    0    0 24909.4500
    NSW4     1  27   11  black       0        1    0    0  7506.1460
    NSW5     1  33    8  black       0        1    0    0   289.7899
    NSW6     1  22    9  black       0        1    0    0  4056.4940

We are interested in the treatment effect of the `treat` variable on the
`re78` outcome. The `treat` variable is a binary variable indicating
whether the individual received job training. The `re78` variable is the
individual’s earnings in 1978.

## Matching

The first step is to pre-process the dataset to achieve better covariate
balance. To do this, we use the `MatchIt::matchit()` function and a
1-to-1 nearest neighbor matching with replacement on the Mahaloanobis
distance. This function supports many other matching methods, see
`?matchit`.

``` r
dat <- matchit(
    treat ~ age + educ + race + married + nodegree + re74 + re75, 
    data = lalonde, distance = "mahalanobis",
    replace = FALSE)
dat <- match.data(dat)
```

## Fitting

Now, we estimate a linear regression model with interactions between the
treatment and covariates. Note that we use the `weights` argument to use
the weights supplied by our matching method:

``` r
fit <- lm(
    re78 ~ treat * (age + educ + race + married + nodegree),
    data = dat,
    weights = weights)
```

## Quantity of interest

Finally, we use the `avg_comparisons()` function of the
`marginaleffects` package to estimate the ATT and its standard error. In
effect, this function applies
[G-Computation](https://marginaleffects.com/vignettes/gcomputation.html)
to estimate the quantity of interest. We use the following arguments:

-   `variables="treat"` indicates that we are interested in the effect
    of the `treat` variable.
-   `newdata=subset(dat, treat == 1)` indicates that we want to estimate
    the effect for the treated individuals only (i.e., the ATT).
-   `wts="weights"` indicates that we want to use the weights supplied
    by the matching method.

``` r
avg_comparisons(
    fit,
    variables = "treat",
    newdata = subset(dat, treat == 1),
    vcov = ~subclass,
    wts = "weights")
```


      Term Contrast Estimate Std. Error    z Pr(>|z|)   S 2.5 % 97.5 %
     treat    1 - 0     1221        850 1.44    0.151 2.7  -445   2888

    Columns: term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
    Type:  response 

## Learn more

[The `MatchIt` vignette titled “Estimating Effects After
Matching”](https://kosukeimai.github.io/MatchIt/vignettes/estimating-effects.html)
describes many more options, including different measures of uncertainty
(bootstrap, clustering, etc.), different estimands (ATE, etc.), and
different strategies for adjustment.
