
title: Inverse Probability Weighting

Inverse Probability Weighting (IPW) is a popular technique to remove
confounding in statistical modeling. It essentially involves
re-weighting your sample so that it represents the population you’re
interested in. Typically, we begin by estimating the predicted
probability that each unit is treated. Then, we use these probabilities
as weights in model fitting and in the computation of marginal effects,
contrasts, risk differences, ratios, etc.

This chapter introduces how to use `marginaleffects` for IPW. The
presentation is very short. Readers who seek a more comprehensive
understanding and application of these methods should refer to [Noah
Greifer’s excellent and detailed work on the
topic](https://ngreifer.github.io/) and to the [`WeightIt` package
vignettes and website.](https://ngreifer.github.io/WeightIt/)

To illustrate, we use the Lalonde data.

``` r
library(marginaleffects)
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

To begin, we use a logistic regression model to estimate the probability
that each unit will treated:

``` r
m <- glm(treat ~ age + educ + race + re74, data = lalonde, family = binomial)
```

Then, we call `predictions()` to extract predicted probabilities. Note
that we supply the original `lalonde` data explicity to the `newdata`
argument. This ensures that all the original columns are carried over to
the new dataset: `dat`. We also create a new column called `wts` that
contains the inverse of the predicted probabilities:

``` r
dat <- predictions(m, newdata = lalonde)
dat$wts <- ifelse(dat$treat == 1, 1 / dat$estimate, 1 / (1 - dat$estimate))
```

Now, we use linear regression to model the outcome of interest: personal
income in 1978 (`re78`). Note that we use the predictions as weights in
the model fitting process.

``` r
mod <- lm(re78 ~ treat * (age + educ + race + re74), data = dat, weights = wts)
```

Finally, we call `avg_comparisons()` to compute the average treatment
effect. Note that we use the `wts` argument to specify the weights to be
used in the computation.

``` r
avg_comparisons(mod,
    variables = "treat",
    wts = "wts",
    vcov = "HC3")
```


      Term Contrast Estimate Std. Error    z Pr(>|z|)   S 2.5 % 97.5 %
     treat    1 - 0      973       1173 0.83    0.407 1.3 -1326   3272

    Columns: term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
    Type:  response 

By default, `avg_comparisons()` uses the Hajek estimator, that is, the
weights are normalized to sum to 1 before computation. If a user wants
to use the Horvitz-Thompson estimator—where normalization accounts for
sample size—they can easily define a custom `comparison` function like
this one:

``` r
ht <- \(hi, lo, w, newdata) {
    (sum(hi * w) / nrow(newdata)) - (sum(lo * w) / nrow(newdata))
}

comparisons(mod,
    comparison = ht,
    variables = "treat",
    wts = "wts",
    vcov = "HC3")
```


      Term Contrast Estimate Std. Error    z Pr(>|z|)   S 2.5 % 97.5 %
     treat     1, 0     1851       2231 0.83    0.407 1.3 -2521   6222

    Columns: term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted 
    Type:  response 
