
# Alternative Software

If you do not like `marginaleffects`, you may want to consider one of
the alternatives described below:

-   `margins`:
    https://cran.r-project.org/web/packages/margins/index.html
-   `prediction`:
    https://cran.r-project.org/web/packages/prediction/index.html
-   `emmeans`:
    https://cran.r-project.org/web/packages/emmeans/index.html
-   `brmsmargins`: https://joshuawiley.com/brmsmargins/
-   `effects`: https://cran.r-project.org/package=effects
-   `modelbased`: https://easystats.github.io/modelbased/
-   `ggeffects`: https://strengejacke.github.io/ggeffects/
-   `Stata` by StataCorp LLC

## `emmeans`

The [`emmeans`
package](https://cran.r-project.org/web/packages/emmeans/index.html) is
developed by Russell V. Lenth and colleagues. `emmeans` is a truly
incredible piece of software, and a trailblazer in the `R` ecosystem. It
is an *extremely* powerful package whose functionality overlaps
`marginaleffects` to a significant degree: marginal means, contrasts,
and slopes. Even if the two packages can compute many of the same
quantities, `emmeans` and `marginaleffects` have pretty different
philosophies with respect to user interface and computation.

An `emmeans` analysis typically starts by computing [“marginal
means”](marginalmeans.html) by holding all numeric covariates at their
means, and by averaging across a balanced grid of categorical
predictors. Then, users can use the `contrast()` function to estimate
the difference between marginal means.

The `marginaleffects` package supplies a `marginal_means` function which
can replicate most `emmeans` analyses by computing marginal means.
However, the typical analysis is more squarely centered on
predicted/fitted values. This is a useful starting point because, in
many cases, analysts will find it easy and intuitive to express their
scientific queries in terms of changes in predicted values. For example,

-   How does the average predicted probability of survival differ
    between treatment and control group?
-   What is the difference between the predicted wage of college and
    high school graduates?

Let’s say we estimate a linear regression model with two continuous
regressors and a multiplicative interaction:

*y* = *β*<sub>0</sub> + *β*<sub>1</sub>*x* + *β*<sub>2</sub>*z* + *β*<sub>3</sub>*x* ⋅ *z* + *ε*

In this model, the effect of *x* on *y* will depend on the value of
covariate *z*. Let’s say the user wants to estimate what happens to the
predicted value of *y* when *x* increases by 1 unit, when
*z* ∈ { − 1, 0, 1}. To do this, we use the `comparisons()` function. The
`variables` argument determines the scientific query of interest, and
the `newdata` argument determines the grid of covariate values on which
we want to evaluate the query:

``` r
model <- lm(y ~ x * z, data)

comparisons(
  model,
  variables = list(x = 1), # what is the effect of 1-unit change in x?
  newdata = datagrid(z = -1:1) # when z is held at values -1, 0, or 1
)
```

As [the vignettes
show,](https://marginaleffects.com/index.html#table-of-contents)
`marginaleffects` can also compute contrasts on marginal means. It can
also compute various quantities of interest like raw fitted values,
slopes (partial derivatives), and contrasts between marginal means. It
also offers a flexible mechanism to run (non-)linear hypothesis tests
using the delta method, and it offers fully customizable strategy to
compute quantities like odds ratios (or completely arbitrary functions
of predicted outcome).

Thus, in my (Vincent’s) biased opinion, the main benefits of
`marginaleffects` over `emmeans` are:

-   Support more model types.
-   Simpler, more intuitive, and highly consistent user interface.
-   Easier to compute average slopes or unit-level contrasts for whole
    datasets.
-   Easier to compute slopes (aka marginal effects, trends, or partial
    derivatives) for custom grids and continuous regressors.
-   Easier to implement causal inference strategies like the parametric
    g-formula and regression adjustment in experiments (see vignettes).
-   Allows the computation of arbitrary quantities of interest via
    user-supplied functions and automatic delta method inference.
-   Common plots are easy with the `plot_predictions()`,
    `plot_comparisons()`, and `plot_slopes()` functions.

To be fair, many of the `marginaleffects` advantages listed above come
down to subjective preferences over user interface. Readers are thus
encouraged to try both packages to see which interface they prefer.

The main advantages of `emmeans` over `marginaleffects` arise when users
are specifically interested in marginal means, where `emmeans` tends to
be much faster and to have a lot of functionality to handle
backtransformations. `emmeans` also has better functionality for effect
sizes; notably, the `eff_size()` function can return effect size
estimates that account for uncertainty in both estimated effects and the
population SD.

Please let me know if you find other features in `emmeans` so I can add
them to this list.

The [Marginal Means Vignette](marginalmeans.html) includes side-by-side
comparisons of `emmeans` and `marginaleffects` to compute marginal
means. The rest of this section compares the syntax for contrasts and
marginaleffects.

### Contrasts

As far as I can tell, `emmeans` does not provide an easy way to compute
unit-level contrasts for every row of the dataset used to fit our model.
Therefore, the side-by-side syntax shown below will always include
`newdata=datagrid()` to specify that we want to compute only one
contrast: at the mean values of the regressors. In day-to-day practice
with `slopes()`, however, this extra argument would not be necessary.

Fit a model:

``` r
library(emmeans)
library(marginaleffects)

mod <- glm(vs ~ hp + factor(cyl), data = mtcars, family = binomial)
```

Link scale, pairwise contrasts:

``` r
emm <- emmeans(mod, specs = "cyl")
contrast(emm, method = "revpairwise", adjust = "none", df = Inf)
#>  contrast    estimate      SE  df z.ratio p.value
#>  cyl6 - cyl4   -0.905    1.63 Inf  -0.555  0.5789
#>  cyl8 - cyl4  -19.542 4367.17 Inf  -0.004  0.9964
#>  cyl8 - cyl6  -18.637 4367.16 Inf  -0.004  0.9966
#> 
#> Degrees-of-freedom method: user-specified 
#> Results are given on the log odds ratio (not the response) scale.

comparisons(mod,
            type = "link",
            newdata = "mean",
            variables = list(cyl = "pairwise"))
#> 
#>  Term Contrast Estimate Std. Error        z Pr(>|z|)   S   2.5 %  97.5 %  hp cyl
#>   cyl    6 - 4   -0.905       1.63 -0.55506    0.579 0.8    -4.1    2.29 147   8
#>   cyl    8 - 4  -19.542    4367.17 -0.00447    0.996 0.0 -8579.0 8539.95 147   8
#>   cyl    8 - 6  -18.637    4367.17 -0.00427    0.997 0.0 -8578.1 8540.85 147   8
#> 
#> Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted, vs, hp, cyl 
#> Type:  link
```

Response scale, reference groups:

``` r
emm <- emmeans(mod, specs = "cyl", regrid = "response")
contrast(emm, method = "trt.vs.ctrl1", adjust = "none", df = Inf, ratios = FALSE)
#>  contrast    estimate    SE  df z.ratio p.value
#>  cyl6 - cyl4   -0.222 0.394 Inf  -0.564  0.5727
#>  cyl8 - cyl4   -0.595 0.511 Inf  -1.163  0.2447
#> 
#> Degrees-of-freedom method: user-specified

comparisons(mod, newdata = "mean")
#> 
#>  Term Contrast  Estimate Std. Error         z Pr(>|z|)   S     2.5 %   97.5 %  hp cyl
#>   cyl    6 - 4 -2.22e-01   3.94e-01 -0.564103    0.573 0.8 -9.94e-01 5.50e-01 147   8
#>   cyl    8 - 4 -5.95e-01   5.11e-01 -1.163332    0.245 2.0 -1.60e+00 4.07e-01 147   8
#>   hp     +1    -1.53e-10   6.69e-07 -0.000229    1.000 0.0 -1.31e-06 1.31e-06 147   8
#> 
#> Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted, vs, hp, cyl 
#> Type:  response
```

### Contrasts by group

Here is a slightly more complicated example with contrasts estimated by
subgroup in a `lme4` mixed effects model. First we estimate a model and
compute pairwise contrasts by subgroup using `emmeans`:

``` r
library(dplyr)
library(lme4)
library(emmeans)

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/lme4/VerbAgg.csv")
dat$woman <- as.numeric(dat$Gender == "F")

mod <- glmer(
    woman ~ btype * resp + situ + (1 + Anger | item),
    family = binomial,
    data = dat)

emmeans(mod, specs = "btype", by = "resp") |>
    contrast(method = "revpairwise", adjust = "none")
#> resp = no:
#>  contrast      estimate     SE  df z.ratio p.value
#>  scold - curse  -0.0152 0.1097 Inf  -0.139  0.8898
#>  shout - curse  -0.2533 0.1022 Inf  -2.479  0.0132
#>  shout - scold  -0.2381 0.0886 Inf  -2.686  0.0072
#> 
#> resp = perhaps:
#>  contrast      estimate     SE  df z.ratio p.value
#>  scold - curse  -0.2393 0.1178 Inf  -2.031  0.0422
#>  shout - curse  -0.0834 0.1330 Inf  -0.627  0.5309
#>  shout - scold   0.1559 0.1358 Inf   1.148  0.2510
#> 
#> resp = yes:
#>  contrast      estimate     SE  df z.ratio p.value
#>  scold - curse   0.0391 0.1292 Inf   0.302  0.7624
#>  shout - curse   0.5802 0.1784 Inf   3.252  0.0011
#>  shout - scold   0.5411 0.1888 Inf   2.866  0.0042
#> 
#> Results are averaged over the levels of: situ 
#> Results are given on the log odds ratio (not the response) scale.
```

What did `emmeans` do to obtain these results? Roughly speaking:

1.  Create a prediction grid with one cell for each combination of
    categorical predictors in the model, and all numeric variables held
    at their means.
2.  Make adjusted predictions in each cell of the prediction grid.
3.  Take the average of those predictions (marginal means) for each
    combination of `btype` (focal variable) and `resp` (group `by`
    variable).
4.  Compute pairwise differences (contrasts) in marginal means across
    different levels of the focal variable `btype`.

In short, `emmeans` computes pairwise contrasts between *marginal
means*, which are themselves averages of adjusted predictions. This is
different from the default types of contrasts produced by
`comparisons()`, which reports contrasts between adjusted predictions,
*without* averaging across a pre-specified grid of predictors. What does
`comparisons()` do instead?

Let `newdata` be a data frame supplied by the user (or the original data
frame used to fit the model), then:

1.  Create a new data frame called `newdata2`, which is identical to
    `newdata` except that the focal variable is incremented by one
    level.
2.  Compute contrasts as the difference between adjusted predictions
    made on the two datasets:
    -   `predict(model, newdata = newdata2) - predict(model, newdata = newdata)`

Although it is not idiomatic, we can use still use `comparisons()` to
emulate the `emmeans` results. First, we create a prediction grid with
one cell for each combination of categorical predictor in the model:

``` r
nd <- datagrid(
    model = mod,
    resp = dat$resp,
    situ = dat$situ,
    btype = dat$btype)
nrow(nd)
#> [1] 18
```

This grid has 18 rows, one for each combination of levels for the `resp`
(3), `situ` (2), and `btype` (3) variables (3 \* 2 \* 3 = 18).

Then we compute pairwise contrasts over this grid:

``` r
cmp <- comparisons(mod,
    variables = list("btype" = "pairwise"),
    newdata = nd,
    type = "link")
nrow(cmp)
#> [1] 54
```

There are 3 pairwise contrasts, corresponding to the 3 pairwise
comparisons possible between the 3 levels of the focal variable `btype`:
`scold-curse`, `shout-scold`, `shout-curse`. The `comparisons()`
function estimates those 3 contrasts for each row of `newdata`, so we
get 18 × 3 = 54 rows.

Finally, if we wanted contrasts averaged over each subgroup of the
`resp` variable, we can use the `avg_comparisons()` function with the
`by` argument:

``` r
avg_comparisons(mod,
    by = "resp",
    variables = list("btype" = "pairwise"),
    newdata = nd,
    type = "link")
#> 
#>   Term                  Contrast    resp Estimate Std. Error      z Pr(>|z|)   S  2.5 %   97.5 %
#>  btype mean(scold) - mean(curse) no       -0.0152     0.1097 -0.139  0.88976 0.2 -0.230  0.19972
#>  btype mean(scold) - mean(curse) perhaps  -0.2393     0.1178 -2.031  0.04221 4.6 -0.470 -0.00842
#>  btype mean(scold) - mean(curse) yes       0.0391     0.1292  0.302  0.76239 0.4 -0.214  0.29234
#>  btype mean(shout) - mean(curse) no       -0.2533     0.1022 -2.479  0.01319 6.2 -0.454 -0.05300
#>  btype mean(shout) - mean(curse) perhaps  -0.0834     0.1330 -0.627  0.53090 0.9 -0.344  0.17737
#>  btype mean(shout) - mean(curse) yes       0.5802     0.1784  3.252  0.00115 9.8  0.230  0.92987
#>  btype mean(shout) - mean(scold) no       -0.2381     0.0886 -2.686  0.00723 7.1 -0.412 -0.06436
#>  btype mean(shout) - mean(scold) perhaps   0.1559     0.1358  1.148  0.25103 2.0 -0.110  0.42215
#>  btype mean(shout) - mean(scold) yes       0.5411     0.1888  2.866  0.00416 7.9  0.171  0.91116
#> 
#> Columns: term, contrast, resp, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted 
#> Type:  link
```

These results are identical to those produced by `emmeans` (except for
*t* vs. *z*).

### Marginal Effects

As far as I can tell, `emmeans::emtrends` makes it easier to compute
marginal effects for a few user-specified values than for large grids or
for the full original dataset.

Response scale, user-specified values:

``` r
mod <- glm(vs ~ hp + factor(cyl), data = mtcars, family = binomial)

emtrends(mod, ~hp, "hp", regrid = "response", at = list(cyl = 4))
#>   hp hp.trend    SE  df asymp.LCL asymp.UCL
#>  147 -0.00786 0.011 Inf   -0.0294    0.0137
#> 
#> Confidence level used: 0.95

slopes(mod, newdata = datagrid(cyl = 4))
#> 
#>  Term Contrast cyl Estimate Std. Error      z Pr(>|z|)   S   2.5 % 97.5 %
#>   cyl    6 - 4   4 -0.22219      0.394 -0.564    0.573 0.8 -0.9942 0.5498
#>   cyl    8 - 4   4 -0.59469      0.511 -1.163    0.245 2.0 -1.5966 0.4072
#>   hp     dY/dX   4 -0.00785      0.011 -0.713    0.476 1.1 -0.0294 0.0137
#> 
#> Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, cyl, predicted_lo, predicted_hi, predicted, vs, hp 
#> Type:  response
```

Link scale, user-specified values:

``` r
emtrends(mod, ~hp, "hp", at = list(cyl = 4))
#>   hp hp.trend     SE  df asymp.LCL asymp.UCL
#>  147  -0.0326 0.0339 Inf    -0.099    0.0338
#> 
#> Confidence level used: 0.95

slopes(mod, type = "link", newdata = datagrid(cyl = 4))
#> 
#>  Term Contrast cyl Estimate Std. Error        z Pr(>|z|)   S     2.5 %   97.5 %
#>   cyl    6 - 4   4  -0.9049   1.63e+00 -0.55506    0.579 0.8    -4.100 2.29e+00
#>   cyl    8 - 4   4 -19.5418   4.37e+03 -0.00447    0.996 0.0 -8579.030 8.54e+03
#>   hp     dY/dX   4  -0.0326   3.39e-02 -0.96147    0.336 1.6    -0.099 3.38e-02
#> 
#> Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, cyl, predicted_lo, predicted_hi, predicted, vs, hp 
#> Type:  link
```

### More examples

Here are a few more `emmeans` vs. `marginaleffects` comparisons:

``` r
## Example of examining a continuous x categorical interaction using emmeans and marginaleffects
## Authors: Cameron Patrick and Vincent Arel-Bundock

library(tidyverse)
library(emmeans)
library(marginaleffects)

## use the mtcars data, set up am as a factor
data(mtcars)
mc <- mtcars |> mutate(am = factor(am))

## fit a linear model to mpg with wt x am interaction
m <- lm(mpg ~ wt*am, data = mc)
summary(m)

## 1. means for each level of am at mean wt.
emmeans(m, "am")
marginal_means(m, variables = "am")
predictions(m, newdata = datagrid(am = 0:1))

## 2. means for each level of am at wt = 2.5, 3, 3.5.
emmeans(m, c("am", "wt"), at = list(wt = c(2.5, 3, 3.5)))
predictions(m, newdata = datagrid(am = 0:1, wt = c(2.5, 3, 3.5))

## 3. means for wt = 2.5, 3, 3.5, averaged over levels of am (implicitly!).
emmeans(m, "wt", at = list(wt = c(2.5, 3, 3.5)))

## same thing, but the averaging is more explicit, using the `by` argument
predictions(
  m,
  newdata = datagrid(am = 0:1, wt = c(2.5, 3, 3.5)),
  by = "wt")

## 4. graphical version of 2.
emmip(m, am ~ wt, at = list(wt = c(2.5, 3, 3.5)), CIs = TRUE)
plot_predictions(m, condition = c("wt", "am"))

## 5. compare levels of am at specific values of wt.
## this is a bit ugly because the emmeans defaults for pairs() are silly.
## infer = TRUE: enable confidence intervals.
## adjust = "none": begone, Tukey.
## reverse = TRUE: contrasts as (later level) - (earlier level)
pairs(emmeans(m, "am", by = "wt", at = list(wt = c(2.5, 3, 3.5))),
      infer = TRUE, adjust = "none", reverse = TRUE)

comparisons(
  m,
  variables = "am",
  newdata = datagrid(wt = c(2.5, 3, 3.5)))

## 6. plot of pairswise comparisons
plot(pairs(emmeans(m, "am", by = "wt", at = list(wt = c(2.5, 3, 3.5))),
      infer = TRUE, adjust = "none", reverse = TRUE))

## Since `wt` is numeric, the default is to plot it as a continuous variable on
## the x-axis.  But not that this is the **exact same info** as in the emmeans plot.
plot_comparisons(m, variables = "am", condition = "wt")

## You of course customize everything, set draw=FALSE, and feed the raw data to feed to ggplot2
p <- plot_comparisons(
  m,
  variables = "am",
  condition = list(wt = c(2.5, 3, 3.5)),
  draw = FALSE)

ggplot(p, aes(y = wt, x = comparison, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange()

## 7. slope of wt for each level of am
emtrends(m, "am", "wt")
slopes(m, newdata = datagrid(am = 0:1))
```

## `margins` and `prediction`

The
[`margins`](https://cran.r-project.org/web/packages/margins/index.html)
and
[`prediction`](https://cran.r-project.org/web/packages/prediction/index.html)
packages for `R` were designed by Thomas Leeper to emulate the behavior
of the `margins` command from `Stata`. These packages are trailblazers
and strongly influenced the development of `marginaleffects`. The main
benefits of `marginaleffects` over these packages are:

-   Support more model types
-   Faster
-   Memory efficient
-   Plots using `ggplot2` instead of Base R
-   More extensive test suite
-   Active development

The syntax of the two packages is very similar.

### Average Marginal Effects

``` r
library(margins)
library(marginaleffects)

mod <- lm(mpg ~ cyl + hp + wt, data = mtcars)

mar <- margins(mod)
summary(mar)
#>  factor     AME     SE       z      p   lower   upper
#>     cyl -0.9416 0.5509 -1.7092 0.0874 -2.0214  0.1382
#>      hp -0.0180 0.0119 -1.5188 0.1288 -0.0413  0.0052
#>      wt -3.1670 0.7406 -4.2764 0.0000 -4.6185 -1.7155

mfx <- slopes(mod)
```

### Individual-Level Marginal Effects

Marginal effects in a user-specified data frame:

``` r
head(data.frame(mar))
#>    mpg cyl disp  hp drat    wt  qsec vs am gear carb   fitted se.fitted   dydx_cyl    dydx_hp   dydx_wt Var_dydx_cyl  Var_dydx_hp Var_dydx_wt X_weights X_at_number
#> 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4 22.82043 0.6876212 -0.9416168 -0.0180381 -3.166973    0.3035104 0.0001410451   0.5484521        NA           1
#> 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4 22.01285 0.6056817 -0.9416168 -0.0180381 -3.166973    0.3035104 0.0001410451   0.5484521        NA           1
#> 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1 25.96040 0.7349593 -0.9416168 -0.0180381 -3.166973    0.3035104 0.0001410451   0.5484521        NA           1
#> 4 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1 20.93608 0.5800910 -0.9416168 -0.0180381 -3.166973    0.3035104 0.0001410451   0.5484521        NA           1
#> 5 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2 17.16780 0.8322986 -0.9416168 -0.0180381 -3.166973    0.3035104 0.0001410451   0.5484521        NA           1
#> 6 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1 20.25036 0.6638322 -0.9416168 -0.0180381 -3.166973    0.3035104 0.0001410451   0.5484521        NA           1

head(mfx)
#> 
#>  Term Estimate Std. Error     z Pr(>|z|)   S 2.5 % 97.5 %
#>   cyl   -0.942      0.551 -1.71   0.0875 3.5 -2.02  0.138
#>   cyl   -0.942      0.551 -1.71   0.0875 3.5 -2.02  0.138
#>   cyl   -0.942      0.551 -1.71   0.0875 3.5 -2.02  0.138
#>   cyl   -0.942      0.551 -1.71   0.0875 3.5 -2.02  0.138
#>   cyl   -0.942      0.551 -1.71   0.0875 3.5 -2.02  0.138
#>   cyl   -0.942      0.551 -1.71   0.0875 3.5 -2.02  0.138
#> 
#> Columns: rowid, term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted, mpg, cyl, hp, wt 
#> Type:  response
nd <- data.frame(cyl = 4, hp = 110, wt = 3)
```

### Marginal Effects at the Mean

``` r
mar <- margins(mod, data = data.frame(prediction::mean_or_mode(mtcars)), unit_ses = TRUE)
data.frame(mar)
#>        mpg    cyl     disp       hp     drat      wt     qsec     vs      am   gear   carb   fitted se.fitted   dydx_cyl    dydx_hp   dydx_wt Var_dydx_cyl  Var_dydx_hp Var_dydx_wt SE_dydx_cyl SE_dydx_hp SE_dydx_wt X_weights X_at_number
#> 1 20.09062 6.1875 230.7219 146.6875 3.596563 3.21725 17.84875 0.4375 0.40625 3.6875 2.8125 20.09062 0.4439832 -0.9416168 -0.0180381 -3.166973    0.3035013 0.0001410453     0.54846   0.5509096 0.01187625  0.7405808        NA           1

slopes(mod, newdata = "mean")
#> 
#>  Term Estimate Std. Error     z Pr(>|z|)    S   2.5 %   97.5 %
#>   cyl   -0.942     0.5511 -1.71   0.0875  3.5 -2.0217  0.13847
#>   hp    -0.018     0.0119 -1.52   0.1288  3.0 -0.0413  0.00524
#>   wt    -3.167     0.7407 -4.28   <0.001 15.7 -4.6187 -1.71523
#> 
#> Columns: rowid, term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted, mpg, cyl, hp, wt 
#> Type:  response
```

### Counterfactual Average Marginal Effects

The `at` argument of the `margins` package emulates `Stata` by fixing
the values of some variables at user-specified values, and by
replicating the full dataset several times for each combination of the
supplied values (see the `Stata` section below). For example, if the
dataset includes 32 rows and the user calls `at=list(cyl=c(4, 6))`,
`margins` will compute 64 unit-level marginal effects estimates:

``` r
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod <- lm(mpg ~ cyl * hp + wt, data = mtcars)

mar <- margins(mod, at = list(cyl = c(4, 6, 8)))
summary(mar)
#>  factor    cyl     AME     SE       z      p   lower   upper
#>     cyl 4.0000  0.0381 0.6000  0.0636 0.9493 -1.1378  1.2141
#>     cyl 6.0000  0.0381 0.5999  0.0636 0.9493 -1.1376  1.2139
#>     cyl 8.0000  0.0381 0.5999  0.0636 0.9493 -1.1376  1.2139
#>      hp 4.0000 -0.0878 0.0267 -3.2937 0.0010 -0.1400 -0.0355
#>      hp 6.0000 -0.0499 0.0154 -3.2397 0.0012 -0.0800 -0.0197
#>      hp 8.0000 -0.0120 0.0108 -1.1065 0.2685 -0.0332  0.0092
#>      wt 4.0000 -3.1198 0.6613 -4.7175 0.0000 -4.4160 -1.8236
#>      wt 6.0000 -3.1198 0.6613 -4.7175 0.0000 -4.4160 -1.8236
#>      wt 8.0000 -3.1198 0.6613 -4.7175 0.0000 -4.4160 -1.8236

avg_slopes(
    mod,
    by = "cyl",
    newdata = datagridcf(cyl = c(4, 6, 8)))
#> 
#>  Term    Contrast cyl Estimate Std. Error       z Pr(>|z|)    S   2.5 %   97.5 %
#>   cyl mean(dY/dX)   4   0.0381     0.5999  0.0636   0.9493  0.1 -1.1376  1.21390
#>   cyl mean(dY/dX)   6   0.0381     0.5999  0.0636   0.9493  0.1 -1.1377  1.21396
#>   cyl mean(dY/dX)   8   0.0381     0.5999  0.0636   0.9493  0.1 -1.1376  1.21385
#>   hp  mean(dY/dX)   4  -0.0878     0.0267 -3.2935   <0.001 10.0 -0.1400 -0.03554
#>   hp  mean(dY/dX)   6  -0.0499     0.0154 -3.2395   0.0012  9.7 -0.0800 -0.01970
#>   hp  mean(dY/dX)   8  -0.0120     0.0108 -1.1065   0.2685  1.9 -0.0332  0.00923
#>   wt  mean(dY/dX)   4  -3.1198     0.6613 -4.7175   <0.001 18.7 -4.4160 -1.82364
#>   wt  mean(dY/dX)   6  -3.1198     0.6613 -4.7176   <0.001 18.7 -4.4160 -1.82366
#>   wt  mean(dY/dX)   8  -3.1198     0.6613 -4.7175   <0.001 18.7 -4.4160 -1.82364
#> 
#> Columns: term, contrast, cyl, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted 
#> Type:  response
```

### Adjusted Predictions

The syntax to compute adjusted predictions using the `predictions`
package or `marginaleffects` is very similar:

``` r
prediction::prediction(mod) |> head()
#>    mpg cyl disp  hp drat    wt  qsec vs am gear carb   fitted se.fitted
#> 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4 21.90488 0.6927034
#> 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4 21.10933 0.6266557
#> 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1 25.64753 0.6652076
#> 4 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1 20.04859 0.6041400
#> 5 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2 17.25445 0.7436172
#> 6 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1 19.53360 0.6436862

marginaleffects::predictions(mod) |> head()
#> 
#>  Estimate Std. Error    z Pr(>|z|)     S 2.5 % 97.5 %
#>      21.9      0.693 31.6   <0.001 726.6  20.5   23.3
#>      21.1      0.627 33.7   <0.001 823.9  19.9   22.3
#>      25.6      0.665 38.6   <0.001   Inf  24.3   27.0
#>      20.0      0.604 33.2   <0.001 799.8  18.9   21.2
#>      17.3      0.744 23.2   <0.001 393.2  15.8   18.7
#>      19.5      0.644 30.3   <0.001 669.5  18.3   20.8
#> 
#> Columns: rowid, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, mpg, cyl, hp, wt 
#> Type:  response
```

## `Stata`

`Stata` is a good but expensive software package for statistical
analysis. It is published by StataCorp LLC. This section compares
`Stata`’s `margins` command to `marginaleffects`.

The results produced by `marginaleffects` are extensively tested against
`Stata`. See the [test
suite](https://github.com/vincentarelbundock/marginaleffects/tree/main/inst/tinytest)
for a list of the dozens of models where we compared estimates and
standard errors.

### Average Marginal Effect (AMEs)

Marginal effects are unit-level quantities. To compute “average marginal
effects”, we first calculate marginal effects for each observation in a
dataset. Then, we take the mean of those unit-level marginal effects.

#### Stata

Both Stata’s `margins` command and the `slopes` function can calculate
average marginal effects (AMEs). Here is an example showing how to
estimate AMEs in Stata:

    quietly reg mpg cyl hp wt
    margins, dydx(*)

    Average marginal effects                        Number of obs     =         32
    Model VCE    : OLS
     
    Expression   : Linear prediction, predict()
    dy/dx w.r.t. : cyl hp wt
     
    ------------------------------------------------------------------------------
        |            Delta-method
        |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
    ------------------------------------------------------------------------------
    cyl |  -.9416168   .5509164    -1.71   0.098    -2.070118    .1868842
     hp |  -.0180381   .0118762    -1.52   0.140    -.0423655    .0062893
     wt |  -3.166973   .7405759    -4.28   0.000    -4.683974   -1.649972
    ------------------------------------------------------------------------------

#### marginaleffects

The same results can be obtained with `slopes()` and `summary()` like
this:

``` r
library("marginaleffects")
mod <- lm(mpg ~ cyl + hp + wt, data = mtcars)
avg_slopes(mod)
#> 
#>  Term Estimate Std. Error     z Pr(>|z|)    S   2.5 %   97.5 %
#>   cyl   -0.942     0.5510 -1.71   0.0875  3.5 -2.0215  0.13832
#>   hp    -0.018     0.0119 -1.52   0.1288  3.0 -0.0413  0.00524
#>   wt    -3.167     0.7405 -4.28   <0.001 15.7 -4.6184 -1.71553
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

Note that `Stata` reports t statistics while `marginaleffects` reports
Z. This produces slightly different p-values because this model has low
degrees of freedom: `mtcars` only has 32 rows

### Counterfactual Marginal Effects

A “counterfactual marginal effect” is a special quantity obtained by
replicating a dataset while fixing some regressor to user-defined
values.

Concretely, Stata computes counterfactual marginal effects in 3 steps:

1.  Duplicate the whole dataset 3 times and sets the values of `cyl` to
    the three specified values in each of those subsets.
2.  Calculate marginal effects for each observation in that large grid.
3.  Take the average of marginal effects for each value of the variable
    of interest.

#### Stata

With the `at` argument, Stata’s `margins` command estimates average
*counterfactual* marginal effects. Here is an example:

    quietly reg mpg i.cyl##c.hp wt
    margins, dydx(hp) at(cyl = (4 6 8))

    Average marginal effects                        Number of obs     =         32
    Model VCE    : OLS

    Expression   : Linear prediction, predict()
    dy/dx w.r.t. : hp

    1._at        : cyl             =           4

    2._at        : cyl             =           6

    3._at        : cyl             =           8

    ------------------------------------------------------------------------------
                 |            Delta-method
                 |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
    -------------+----------------------------------------------------------------
    hp           |
             _at |
              1  |   -.099466   .0348665    -2.85   0.009    -.1712749   -.0276571
              2  |  -.0213768    .038822    -0.55   0.587    -.1013323    .0585787
              3  |   -.013441   .0125138    -1.07   0.293    -.0392137    .0123317
    ------------------------------------------------------------------------------

<!-- Adapted from this GitHub issue: https://github.com/strengejacke/ggeffects/issues/249 -->

#### marginaleffects

You can estimate average counterfactual marginal effects with `slopes()`
by using the [`datagridcf()`](reference/datagrid.html) to create a
counterfactual dataset in which the full original dataset is replicated
for each potential value of the `cyl` variable. Then, we tell the `by`
argument to average within groups:

``` r
mod <- lm(mpg ~ as.factor(cyl) * hp + wt, data = mtcars)

avg_slopes(
    mod,
    variables = "hp",
    by = "cyl",
    newdata = datagridcf(cyl = c(4, 6, 8)))
#> 
#>  Term    Contrast cyl Estimate Std. Error      z Pr(>|z|)   S   2.5 %  97.5 %
#>    hp mean(dY/dX)   4  -0.0995     0.0349 -2.853  0.00433 7.8 -0.1678 -0.0311
#>    hp mean(dY/dX)   6  -0.0214     0.0388 -0.551  0.58190 0.8 -0.0975  0.0547
#>    hp mean(dY/dX)   8  -0.0134     0.0125 -1.074  0.28278 1.8 -0.0380  0.0111
#> 
#> Columns: term, contrast, cyl, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted 
#> Type:  response
```

This is equivalent to taking the group-wise mean of observation-level
marginal effects (without the `by` argument):

``` r
mfx <- slopes(
    mod,
    variables = "hp",
    newdata = datagridcf(cyl = c(4, 6, 8)))
aggregate(estimate ~ term + cyl, data = mfx, FUN = mean)
#>   term cyl    estimate
#> 1   hp   4 -0.09946598
#> 2   hp   6 -0.02137679
#> 3   hp   8 -0.01344103
```

<!-- Taken from https://github.com/vincentarelbundock/marginaleffects/issues/226 -->

Note that following `Stata`, the standard errors for group-averaged
marginal effects are computed by taking the “Jacobian at the mean:”

``` r
J <- attr(mfx, "jacobian")
J_mean <- aggregate(J, by = list(mfx$cyl), FUN = mean)
J_mean <- as.matrix(J_mean[, 2:ncol(J_mean)])
sqrt(diag(J_mean %*% vcov(mod) %*% t(J_mean)))
#> [1] 0.03486675 0.03882338 0.01251377
```

### Average Counterfactual Adjusted Predictions

#### Stata

Just like Stata’s `margins` command computes average counterfactual
marginal effects, it can also estimate *average counterfactual adjusted
predictions*.

Here is an example:

    quietly reg mpg i.cyl##c.hp wt
    margins, at(cyl = (4 6 8))

    Predictive margins                              Number of obs     =         32
    Model VCE    : OLS

    Expression   : Linear prediction, predict()

    1._at        : cyl             =           4

    2._at        : cyl             =           6

    3._at        : cyl             =           8

    ------------------------------------------------------------------------------
                 |            Delta-method
                 |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
    -------------+----------------------------------------------------------------
             _at |
              1  |   17.44233   2.372914     7.35   0.000     12.55522    22.32944
              2  |    18.9149   1.291483    14.65   0.000     16.25505    21.57476
              3  |   18.33318   1.123874    16.31   0.000     16.01852    20.64785
    ------------------------------------------------------------------------------

Again, this is what Stata does in the background:

1.  It duplicates the whole dataset 3 times and sets the values of `cyl`
    to the three specified values in each of those subsets.
2.  It calculates predictions for that large grid.
3.  It takes the average prediction for each value of `cyl`.

In other words, average counterfactual adjusted predictions as
implemented by Stata are a hybrid between predictions at the observed
values (the default in `marginaleffects::predictions`) and predictions
at representative values.

#### marginaleffects

You can estimate average counterfactual adjusted predictions with
`predictions()` by, first, setting the `grid_type` argument of
`datagrid()` to `"counterfactual"` and, second, by averaging the
predictions using the `by` argument of `summary()`, or a manual function
like `dplyr::summarise()`.

``` r
mod <- lm(mpg ~ as.factor(cyl) * hp + wt, data = mtcars)

predictions(
    mod,
    by = "cyl",
    newdata = datagridcf(cyl = c(4, 6, 8)))
#> 
#>  cyl Estimate Std. Error     z Pr(>|z|)     S 2.5 % 97.5 %
#>    4     17.4       2.37  7.35   <0.001  42.2  12.8   22.1
#>    6     18.9       1.29 14.65   <0.001 158.9  16.4   21.4
#>    8     18.3       1.12 16.31   <0.001 196.3  16.1   20.5
#> 
#> Columns: cyl, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response

predictions(
    mod,
    newdata = datagridcf(cyl = c(4, 6, 8))) |>
    group_by(cyl) |>
    summarize(AAP = mean(estimate))
#> # A tibble: 3 × 2
#>   cyl     AAP
#>   <fct> <dbl>
#> 1 4      17.4
#> 2 6      18.9
#> 3 8      18.3
```

## `brmsmargins`

[The `brmsmargins` package](https://joshuawiley.com/brmsmargins/) is
developed by Joshua Wiley:

> This package has functions to calculate marginal effects from brms
> models ( http://paul-buerkner.github.io/brms/ ). A central motivator
> is to calculate average marginal effects (AMEs) for continuous and
> discrete predictors in fixed effects only and mixed effects regression
> models including location scale models.

The main advantage of `brmsmargins` over `marginaleffects` is its
ability to compute “Marginal Coefficients” following the method
described in [Hedeker et al (2012).](https://doi.org/10.1111/biom.12707)

The main advantages of `marginaleffects` over `brmsmargins` are:

1.  Support for 60+ model types, rather than just the `brms` package.
2.  Simpler user interface (subjective).
3.  At the time of writing (2022-05-25) `brmsmargins` did not support
    certain `brms` models such as those with multivariate or multinomial
    outcomes. It also did not support custom outcome transformations.

The rest of this section presents side-by-side replications of some of
the analyses from the `brmsmargins` vignettes in order to show highlight
parallels and differences in syntax.

### Marginal Effects for Fixed Effects Models

#### AMEs for Logistic Regression

Estimate a logistic regression model with `brms`:

``` r
library(brms)
library(brmsmargins)
library(marginaleffects)
library(data.table)
library(withr)
setDTthreads(5)
h <- 1e-4

void <- capture.output(
    bayes.logistic <- brm(
      vs ~ am + mpg, data = mtcars,
      family = "bernoulli", seed = 1234,
      silent = 2, refresh = 0,
      backend = "cmdstanr",
      chains = 4L, cores = 4L)
)
```

Compute AMEs manually:

``` r
d1 <- d2 <- mtcars
d2$mpg <- d2$mpg + h
p1 <- posterior_epred(bayes.logistic, newdata = d1)
p2 <- posterior_epred(bayes.logistic, newdata = d2)
m <- (p2 - p1) / h
quantile(rowMeans(m), c(.5, .025, .975))
#>        50%       2.5%      97.5% 
#> 0.07010427 0.05418413 0.09092451
```

Compute AMEs with `brmsmargins`:

``` r
bm <- brmsmargins(
  bayes.logistic,
  add = data.frame(mpg = c(0, 0 + h)),
  contrasts = cbind("AME MPG" = c(-1 / h, 1 / h)),
  CI = 0.95,
  CIType = "ETI")
data.frame(bm$ContrastSummary)
#>            M        Mdn         LL         UL PercentROPE PercentMID   CI CIType ROPE  MID   Label
#> 1 0.07105468 0.07010427 0.05418413 0.09092451          NA         NA 0.95    ETI <NA> <NA> AME MPG
```

Compute AMEs using `marginaleffects`:

``` r
avg_slopes(bayes.logistic) 
#> 
#>  Term Contrast Estimate   2.5 %  97.5 %
#>   am     1 - 0  -0.2665 -0.4242 -0.0703
#>   mpg    dY/dX   0.0701  0.0542  0.0909
#> 
#> Columns: term, contrast, estimate, conf.low, conf.high 
#> Type:  response
```

The `mpg` element of the `Effect` column from `marginaleffects` matches
the the `M` column of the output from `brmsmargins`.

### Marginal Effects for Mixed Effects Models

Estimate a mixed effects logistic regression model with `brms`:

``` r
d <- withr::with_seed(
  seed = 12345, code = {
    nGroups <- 100
    nObs <- 20
    theta.location <- matrix(rnorm(nGroups * 2), nrow = nGroups, ncol = 2)
    theta.location[, 1] <- theta.location[, 1] - mean(theta.location[, 1])
    theta.location[, 2] <- theta.location[, 2] - mean(theta.location[, 2])
    theta.location[, 1] <- theta.location[, 1] / sd(theta.location[, 1])
    theta.location[, 2] <- theta.location[, 2] / sd(theta.location[, 2])
    theta.location <- theta.location %*% chol(matrix(c(1.5, -.25, -.25, .5^2), 2))
    theta.location[, 1] <- theta.location[, 1] - 2.5
    theta.location[, 2] <- theta.location[, 2] + 1
    d <- data.table(
      x = rep(rep(0:1, each = nObs / 2), times = nGroups))
    d[, ID := rep(seq_len(nGroups), each = nObs)]

    for (i in seq_len(nGroups)) {
      d[ID == i, y := rbinom(
        n = nObs,
        size = 1,
        prob = plogis(theta.location[i, 1] + theta.location[i, 2] * x))
        ]
    }
    copy(d)
  })

void <- capture.output(
    mlogit <- brms::brm(
      y ~ 1 + x + (1 + x | ID), family = "bernoulli",
      data = d, seed = 1234,
      backend = "cmdstanr",
      silent = 2, refresh = 0,
      chains = 4L, cores = 4L)
)
```

#### AME: Including Random Effects

``` r
bm <- brmsmargins(
  mlogit,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  effects = "includeRE",
  CI = .95,
  CIType = "ETI")
data.frame(bm$ContrastSummary)
#>          M       Mdn         LL        UL PercentROPE PercentMID   CI CIType ROPE  MID Label
#> 1 0.111492 0.1115944 0.08095807 0.1420166          NA         NA 0.95    ETI <NA> <NA> AME x

avg_slopes(mlogit)
#> 
#>  Term Contrast Estimate  2.5 % 97.5 %
#>     x    1 - 0    0.111 0.0806   0.14
#> 
#> Columns: term, contrast, estimate, conf.low, conf.high 
#> Type:  response
```

#### AME: Fixed Effects Only (Grand Mean)

``` r
bm <- brmsmargins(
  mlogit,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  effects = "fixedonly",
  CI = .95,
  CIType = "ETI")
data.frame(bm$ContrastSummary)
#>           M       Mdn         LL        UL PercentROPE PercentMID   CI CIType ROPE  MID Label
#> 1 0.1039555 0.1034452 0.06319565 0.1491665          NA         NA 0.95    ETI <NA> <NA> AME x

avg_slopes(mlogit, re_formula = NA)
#> 
#>  Term Contrast Estimate  2.5 % 97.5 %
#>     x    1 - 0    0.101 0.0623  0.143
#> 
#> Columns: term, contrast, estimate, conf.low, conf.high 
#> Type:  response
```

### Marginal Effects for Location Scale Models

#### AMEs for Fixed Effects Location Scale Models

Estimate a fixed effects location scale model with `brms`:

``` r
d <- withr::with_seed(
  seed = 12345, code = {
    nObs <- 1000L
    d <- data.table(
      grp = rep(0:1, each = nObs / 2L),
      x = rnorm(nObs, mean = 0, sd = 0.25))
    d[, y := rnorm(nObs,
                   mean = x + grp,
                   sd = exp(1 + x + grp))]
    copy(d)
  })

void <- capture.output(
    ls.fe <- brm(bf(
      y ~ 1 + x + grp,
      sigma ~ 1 + x + grp),
      family = "gaussian",
      data = d, seed = 1234,
      silent = 2, refresh = 0,
      backend = "cmdstanr",
      chains = 4L, cores = 4L)
)
```

#### Fixed effects only

``` r
bm <- brmsmargins(
  ls.fe,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  CI = 0.95, CIType = "ETI",
  effects = "fixedonly")
data.frame(bm$ContrastSummary)
#>          M     Mdn        LL      UL PercentROPE PercentMID   CI CIType ROPE  MID Label
#> 1 1.626186 1.63215 0.7349262 2.46998          NA         NA 0.95    ETI <NA> <NA> AME x

avg_slopes(ls.fe, re_formula = NA)
#> 
#>  Term Contrast Estimate 2.5 % 97.5 %
#>   grp    1 - 0     1.02 0.355   1.70
#>   x      dY/dX     1.63 0.735   2.47
#> 
#> Columns: term, contrast, estimate, conf.low, conf.high 
#> Type:  response
```

#### Discrete change and distributional parameter (`dpar`)

Compute the contrast between adjusted predictions on the `sigma`
parameter, when `grp=0` and `grp=1`:

``` r
bm <- brmsmargins(
  ls.fe,
  at = data.frame(grp = c(0, 1)),
  contrasts = cbind("AME grp" = c(-1, 1)),
  CI = 0.95, CIType = "ETI", dpar = "sigma",
  effects = "fixedonly")
data.frame(bm$ContrastSummary)
#>          M     Mdn       LL       UL PercentROPE PercentMID   CI CIType ROPE  MID   Label
#> 1 4.899239 4.89621 4.423663 5.422412          NA         NA 0.95    ETI <NA> <NA> AME grp
```

In `marginaleffects` we use the `comparisons()` function and the
`variables` argument:

``` r
avg_comparisons(
  ls.fe,
  variables = list(grp = 0:1),
  dpar = "sigma")
#> 
#>  Term Contrast Estimate 2.5 % 97.5 %
#>   grp    1 - 0      4.9  4.42   5.42
#> 
#> Columns: term, contrast, estimate, conf.low, conf.high 
#> Type:  response
```

#### Marginal effect (continuous) on sigma

``` r
bm <- brmsmargins(
  ls.fe,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  CI = 0.95, CIType = "ETI", dpar = "sigma",
  effects = "fixedonly")
data.frame(bm$ContrastSummary)
#>          M     Mdn       LL       UL PercentROPE PercentMID   CI CIType ROPE  MID Label
#> 1 4.458758 4.46162 3.498163 5.443716          NA         NA 0.95    ETI <NA> <NA> AME x

avg_slopes(ls.fe, dpar = "sigma", re_formula = NA)
#> 
#>  Term Contrast Estimate 2.5 % 97.5 %
#>   grp    1 - 0     4.90  4.42   5.42
#>   x      dY/dX     4.46  3.50   5.44
#> 
#> Columns: term, contrast, estimate, conf.low, conf.high 
#> Type:  response
```

## `fmeffects`

The [`fmeffects` package](https://cran.r-project.org/package=fmeffects)
is described as follows:

> fmeffects: Model-Agnostic Interpretations with Forward Marginal
> Effects. Create local, regional, and global explanations for any
> machine learning model with forward marginal effects. You provide a
> model and data, and ‘fmeffects’ computes feature effects. The package
> is based on the theory in: C. A. Scholbeck, G. Casalicchio, C. Molnar,
> B. Bischl, and C. Heumann (2022)

As the name says, this package is focused on “forward marginal effects”
in the context of machine learning models estimated using the `mlr3` or
`tidymodels` frameworks. Since version 0.16.0, `marginaleffects` also
supports these machine learning frameworks, and it covers a superset of
the `fmeffects` functionality. Consider a random forest model trained on
the `bikes` data:

``` r
library("mlr3verse")
library("fmeffects")
data("bikes", package = "fmeffects")
task <- as_task_regr(x = bikes, id = "bikes", target = "count")
forest <- lrn("regr.ranger")$train(task)
```

Now, we use the `avg_comparisons()` function to compute *forward*
marginal effects:

``` r
avg_comparisons(forest, variables = list(temp = 1), newdata = bikes)
#> 
#>  Term Contrast Estimate
#>  temp       +1     2.33
#> 
#> Columns: term, contrast, estimate 
#> Type:  response
```

This is equivalent to the key quantity reported by the `fmeffects`
package:

``` r
fmeffects::fme(
    model = forest,
    data = bikes,
    target = "count",
    feature = "temp",
    step.size = 1)$ame
#> [1] 2.331442
```

Another interesting feature of `fmeffects` is the ability treat
categorical predictors in an unconventional way: pick a reference level,
then compute the average difference between the predicted values for
that level, and the predicted values for the observed levels (which may
be the same as the reference level).

In the `bikes` example, we can answer the question: how does the
expected number of bike rentals increases, on average, if all days were
misty? With `marginaleffects`, we can use a function in the `variables`
argument to specify a custom contrast:

``` r
FUN <- function(x) data.frame(lo = x, hi = "misty")

avg_comparisons(
  forest,
  newdata = bikes,
  variables = list(weather = FUN)
)
#> 
#>     Term Contrast Estimate
#>  weather   custom     1.77
#> 
#> Columns: term, contrast, estimate 
#> Type:  response
```

Two more functionalities deserve to be highlight. First, `fmeffects`
includes functions to explore heterogeneity in marginal effects using
recursive partitioning trees. The [heterogeneity
vignette](vignettes/heterogeneity/) illustrates how to achieve something
similar with `marginaleffects`.

Second, `fmeffects` also implements a non-linearity measure. At the
moment, there is no analogue to this in `marginaleffects`.

## `effects`

The [`effects` package](https://cran.r-project.org/package=effects) was
created by John Fox and colleagues.

-   `marginaleffects` supports 30+ more model types than `effects`.
-   `effects` focuses on the computation of [“adjusted
    predictions.”](predictions.html) The plots it produces are roughly
    equivalent to the ones produced by the `plot_predictions` and
    `predictions` functions in `marginaleffects`.
-   `effects` does not appear support marginal effects (slopes),
    marginal means, or contrasts
-   `effects` uses Base graphics whereas `marginaleffects` uses
    `ggplot2`
-   `effects` includes *a lot* of very powerful options to customize
    plots. In contrast, `marginaleffects` produces objects which can be
    customized by chaining `ggplot2` functions. Users can also call
    `plot_predictions(model, draw=FALSE)` to create a prediction grid,
    and then work the raw data directly to create the plot they need

`effects` offers several options which are not currently available in
`marginaleffects`, including:

-   Partial residuals plots
-   Many types of ways to plot adjusted predictions: [package
    vignette](https://cran.r-project.org/web/packages/effects/vignettes/predictor-effects-gallery.pdf)

## `modelbased`

The [`modelbased` package](https://easystats.github.io/modelbased/) is
developed by the `easystats` team.

This section is incomplete; contributions are welcome.

-   Wrapper around `emmeans` to compute marginal means and marginal
    effects.
-   Powerful functions to create beautiful plots.

## `ggeffects`

The [`ggeffects`](https://strengejacke.github.io/ggeffects/) package is
developed by Daniel Lüdecke.

This section is incomplete; contributions are welcome.

-   Wrapper around `emmeans` to compute marginal means.
