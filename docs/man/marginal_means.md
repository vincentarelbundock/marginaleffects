
# marginal_means

Marginal Means

## Description

Marginal means are adjusted predictions, averaged across a grid of
categorical predictors, holding other numeric predictors at their means.
To learn more, read the marginal means vignette, visit the package
website, or scroll down this page for a full list of vignettes:

<ul>
<li>

<a href="https://marginaleffects.com/articles/marginalmeans.html">https://marginaleffects.com/articles/marginalmeans.html</a>

</li>
<li>

<a href="https://marginaleffects.com/">https://marginaleffects.com/</a>

</li>
</ul>

## Usage

<pre><code class='language-R'>marginal_means(
  model,
  variables = NULL,
  newdata = NULL,
  vcov = TRUE,
  conf_level = 0.95,
  type = NULL,
  transform = NULL,
  cross = FALSE,
  hypothesis = NULL,
  equivalence = NULL,
  p_adjust = NULL,
  df = Inf,
  wts = "equal",
  by = NULL,
  numderiv = "fdforward",
  ...
)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_model">model</code>
</td>
<td>
Model object
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_variables">variables</code>
</td>
<td>

Focal variables

<ul>
<li>

Character vector of variable names: compute marginal means for each
category of the listed variables.

</li>
<li>

<code>NULL</code>: calculate marginal means for all logical, character,
or factor variables in the dataset used to fit <code>model</code>. Hint:
Set <code>cross=TRUE</code> to compute marginal means for combinations
of focal variables.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_newdata">newdata</code>
</td>
<td>

Grid of predictor values over which we marginalize.

<ul>
<li>

Warning: Please avoid modifying your dataset between fitting the model
and calling a <code>marginaleffects</code> function. This can sometimes
lead to unexpected results.

</li>
<li>

<code>NULL</code> create a grid with all combinations of all categorical
predictors in the model. Warning: can be expensive.

</li>
<li>

Character vector: subset of categorical variables to use when building
the balanced grid of predictors. Other variables are held to their mean
or mode.

</li>
<li>

Data frame: A data frame which includes all the predictors in the
original model. The full dataset is replicated once for every
combination of the focal variables in the <code>variables</code>
argument, using the <code>datagridcf()</code> function.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_vcov">vcov</code>
</td>
<td>

Type of uncertainty estimates to report (e.g., for robust standard
errors). Acceptable values:

<ul>
<li>

FALSE: Do not compute standard errors. This can speed up computation
considerably.

</li>
<li>

TRUE: Unit-level standard errors using the default
<code>vcov(model)</code> variance-covariance matrix.

</li>
<li>

String which indicates the kind of uncertainty estimates to return.

<ul>
<li>

Heteroskedasticity-consistent: <code>“HC”</code>, <code>“HC0”</code>,
<code>“HC1”</code>, <code>“HC2”</code>, <code>“HC3”</code>,
<code>“HC4”</code>, <code>“HC4m”</code>, <code>“HC5”</code>. See
<code>?sandwich::vcovHC</code>

</li>
<li>

Heteroskedasticity and autocorrelation consistent: <code>“HAC”</code>

</li>
<li>

Mixed-Models degrees of freedom: "satterthwaite", "kenward-roger"

</li>
<li>

Other: <code>“NeweyWest”</code>, <code>“KernHAC”</code>,
<code>“OPG”</code>. See the <code>sandwich</code> package documentation.

</li>
</ul>
</li>
<li>

One-sided formula which indicates the name of cluster variables (e.g.,
<code>~unit_id</code>). This formula is passed to the
<code>cluster</code> argument of the <code>sandwich::vcovCL</code>
function.

</li>
<li>

Square covariance matrix

</li>
<li>

Function which returns a covariance matrix (e.g.,
<code>stats::vcov(model)</code>)

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_conf_level">conf_level</code>
</td>
<td>
numeric value between 0 and 1. Confidence level to use to build a
confidence interval.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_type">type</code>
</td>
<td>
string indicates the type (scale) of the predictions used to compute
marginal effects or contrasts. This can differ based on the model type,
but will typically be a string such as: "response", "link", "probs", or
"zero". When an unsupported string is entered, the model-specific list
of acceptable values is returned in an error message. When
<code>type</code> is <code>NULL</code>, the first entry in the error
message is used by default.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_transform">transform</code>
</td>
<td>
A function applied to unit-level adjusted predictions and confidence
intervals just before the function returns results. For bayesian models,
this function is applied to individual draws from the posterior
distribution, before computing summaries.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_cross">cross</code>
</td>
<td>

TRUE or FALSE

<ul>
<li>

<code>FALSE</code> (default): Marginal means are computed for each
predictor individually.

</li>
<li>

<code>TRUE</code>: Marginal means are computed for each combination of
predictors specified in the <code>variables</code> argument.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_hypothesis">hypothesis</code>
</td>
<td>

specify a hypothesis test or custom contrast using a numeric value,
vector, or matrix, a string, or a string formula.

<ul>
<li>

Numeric:

<ul>
<li>

Single value: the null hypothesis used in the computation of Z and p
(before applying <code>transform</code>).

</li>
<li>

Vector: Weights to compute a linear combination of (custom contrast
between) estimates. Length equal to the number of rows generated by the
same function call, but without the <code>hypothesis</code> argument.

</li>
<li>

Matrix: Each column is a vector of weights, as describe above, used to
compute a distinct linear combination of (contrast between) estimates.
The column names of the matrix are used as labels in the output.

</li>
</ul>
</li>
<li>

String formula to specify linear or non-linear hypothesis tests. If the
<code>term</code> column uniquely identifies rows, terms can be used in
the formula. Otherwise, use <code>b1</code>, <code>b2</code>, etc. to
identify the position of each parameter. The
<code style="white-space: pre;">⁠b\*⁠</code> wildcard can be used to test
hypotheses on all estimates. Examples:

<ul>
<li>

<code>hp = drat</code>

</li>
<li>

<code>hp + drat = 12</code>

</li>
<li>

<code>b1 + b2 + b3 = 0</code>

</li>
<li>

<code style="white-space: pre;">⁠b\* / b1 = 1⁠</code>

</li>
</ul>
</li>
<li>

String:

<ul>
<li>

"pairwise": pairwise differences between estimates in each row.

</li>
<li>

"reference": differences between the estimates in each row and the
estimate in the first row.

</li>
<li>

"sequential": difference between an estimate and the estimate in the
next row.

</li>
<li>

"revpairwise", "revreference", "revsequential": inverse of the
corresponding hypotheses, as described above.

</li>
</ul>
</li>
<li>

See the Examples section below and the vignette:
https://marginaleffects.com/articles/hypothesis.html

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_equivalence">equivalence</code>
</td>
<td>
Numeric vector of length 2: bounds used for the two-one-sided test
(TOST) of equivalence, and for the non-inferiority and non-superiority
tests. See Details section below.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_p_adjust">p_adjust</code>
</td>
<td>
Adjust p-values for multiple comparisons: "holm", "hochberg", "hommel",
"bonferroni", "BH", "BY", or "fdr". See stats::p.adjust
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_df">df</code>
</td>
<td>
Degrees of freedom used to compute p values and confidence intervals. A
single numeric value between 1 and <code>Inf</code>. When
<code>df</code> is <code>Inf</code>, the normal distribution is used.
When <code>df</code> is finite, the <code>t</code> distribution is used.
See insight::get_df for a convenient function to extract degrees of
freedom. Ex: <code>slopes(model, df = insight::get_df(model))</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_wts">wts</code>
</td>
<td>

character value. Weights to use in the averaging.

<ul>
<li>

"equal": each combination of variables in <code>newdata</code> gets
equal weight.

</li>
<li>

"cells": each combination of values for the variables in the
<code>newdata</code> gets a weight proportional to its frequency in the
original data.

</li>
<li>

"proportional": each combination of values for the variables in
<code>newdata</code> – except for those in the <code>variables</code>
argument – gets a weight proportional to its frequency in the original
data.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_by">by</code>
</td>
<td>
Collapse marginal means into categories. Data frame with a
<code>by</code> column of group labels, and merging columns shared by
<code>newdata</code> or the data frame produced by calling the same
function without the <code>by</code> argument.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_numderiv">numderiv</code>
</td>
<td>

string or list of strings indicating the method to use to for the
numeric differentiation used in to compute delta method standard errors.

<ul>
<li>

"fdforward": finite difference method with forward differences

</li>
<li>

"fdcenter": finite difference method with central differences (default)

</li>
<li>

"richardson": Richardson extrapolation method

</li>
<li>

Extra arguments can be specified by passing a list to the
<code>numDeriv</code> argument, with the name of the method first and
named arguments following, ex: <code>numderiv=list(“fdcenter”, eps =
1e-5)</code>. When an unknown argument is used,
<code>marginaleffects</code> prints the list of valid arguments for each
method.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="marginal_means_:_...">…</code>
</td>
<td>
Additional arguments are passed to the <code>predict()</code> method
supplied by the modeling package.These arguments are particularly useful
for mixed-effects or bayesian models (see the online vignettes on the
<code>marginaleffects</code> website). Available arguments can vary from
model to model, depending on the range of supported arguments by each
modeling package. See the "Model-Specific Arguments" section of the
<code>?marginaleffects</code> documentation for a non-exhaustive list of
available arguments.
</td>
</tr>
</table>

## Details

This function begins by calling the <code>predictions</code> function to
obtain a grid of predictors, and adjusted predictions for each cell. The
grid includes all combinations of the categorical variables listed in
the <code>variables</code> and <code>newdata</code> arguments, or all
combinations of the categorical variables used to fit the model if
<code>newdata</code> is <code>NULL</code>. In the prediction grid,
numeric variables are held at their means.

After constructing the grid and filling the grid with adjusted
predictions, <code>marginal_means</code> computes marginal means for the
variables listed in the <code>variables</code> argument, by average
across all categories in the grid.

<code>marginal_means</code> can only compute standard errors for linear
models, or for predictions on the link scale, that is, with the
<code>type</code> argument set to "link".

The <code>marginaleffects</code> website compares the output of this
function to the popular <code>emmeans</code> package, which provides
similar but more advanced functionality: https://marginaleffects.com/

## Value

Data frame of marginal means with one row per variable-value
combination.

## Standard errors using the delta method

Standard errors for all quantities estimated by
<code>marginaleffects</code> can be obtained via the delta method. This
requires differentiating a function with respect to the coefficients in
the model using a finite difference approach. In some models, the delta
method standard errors can be sensitive to various aspects of the
numeric differentiation strategy, including the step size. By default,
the step size is set to <code>1e-8</code>, or to <code>1e-4</code> times
the smallest absolute model coefficient, whichever is largest.

<code>marginaleffects</code> can delegate numeric differentiation to the
<code>numDeriv</code> package, which allows more flexibility. To do
this, users can pass arguments to the <code>numDeriv::jacobian</code>
function through a global option. For example:

<ul>
<li>

<code>options(marginaleffects_numDeriv = list(method = “simple”,
method.args = list(eps = 1e-6)))</code>

</li>
<li>

<code>options(marginaleffects_numDeriv = list(method = “Richardson”,
method.args = list(eps = 1e-5)))</code>

</li>
<li>

<code>options(marginaleffects_numDeriv = NULL)</code>

</li>
</ul>

See the "Standard Errors and Confidence Intervals" vignette on the
<code>marginaleffects</code> website for more details on the computation
of standard errors:

https://marginaleffects.com/articles/uncertainty.html

Note that the <code>inferences()</code> function can be used to compute
uncertainty estimates using a bootstrap or simulation-based inference.
See the vignette:

https://marginaleffects.com/articles/bootstrap.html

## Model-Specific Arguments

Some model types allow model-specific arguments to modify the nature of
marginal effects, predictions, marginal means, and contrasts. Please
report other package-specific <code>predict()</code> arguments on Github
so we can add them to the table below.

https://github.com/vincentarelbundock/marginaleffects/issues

<table>
<tr>
<td style="text-align: left;">
Package
</td>
<td style="text-align: left;">
Class
</td>
<td style="text-align: left;">
Argument
</td>
<td style="text-align: left;">
Documentation
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>brms</code>
</td>
<td style="text-align: left;">
<code>brmsfit</code>
</td>
<td style="text-align: left;">
<code>ndraws</code>
</td>
<td style="text-align: left;">
brms::posterior_predict
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
<code>re_formula</code>
</td>
<td style="text-align: left;">
brms::posterior_predict
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>lme4</code>
</td>
<td style="text-align: left;">
<code>merMod</code>
</td>
<td style="text-align: left;">
<code>re.form</code>
</td>
<td style="text-align: left;">
lme4::predict.merMod
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
<code>allow.new.levels</code>
</td>
<td style="text-align: left;">
lme4::predict.merMod
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>glmmTMB</code>
</td>
<td style="text-align: left;">
<code>glmmTMB</code>
</td>
<td style="text-align: left;">
<code>re.form</code>
</td>
<td style="text-align: left;">
glmmTMB::predict.glmmTMB
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
<code>allow.new.levels</code>
</td>
<td style="text-align: left;">
glmmTMB::predict.glmmTMB
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
<code>zitype</code>
</td>
<td style="text-align: left;">
glmmTMB::predict.glmmTMB
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>mgcv</code>
</td>
<td style="text-align: left;">
<code>bam</code>
</td>
<td style="text-align: left;">
<code>exclude</code>
</td>
<td style="text-align: left;">
mgcv::predict.bam
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>robustlmm</code>
</td>
<td style="text-align: left;">
<code>rlmerMod</code>
</td>
<td style="text-align: left;">
<code>re.form</code>
</td>
<td style="text-align: left;">
robustlmm::predict.rlmerMod
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
</td>
<td style="text-align: left;">
<code>allow.new.levels</code>
</td>
<td style="text-align: left;">
robustlmm::predict.rlmerMod
</td>
</tr>
<tr>
<td style="text-align: left;">
<code>MCMCglmm</code>
</td>
<td style="text-align: left;">
<code>MCMCglmm</code>
</td>
<td style="text-align: left;">
<code>ndraws</code>
</td>
<td style="text-align: left;">
</td>
</tr>
<tr>
<td style="text-align: left;">
</td>
</tr>
</table>

## Bayesian posterior summaries

By default, credible intervals in bayesian models are built as
equal-tailed intervals. This can be changed to a highest density
interval by setting a global option:

<code>options(“marginaleffects_posterior_interval” = “eti”)</code>

<code>options(“marginaleffects_posterior_interval” = “hdi”)</code>

By default, the center of the posterior distribution in bayesian models
is identified by the median. Users can use a different summary function
by setting a global option:

<code>options(“marginaleffects_posterior_center” = “mean”)</code>

<code>options(“marginaleffects_posterior_center” = “median”)</code>

When estimates are averaged using the <code>by</code> argument, the
<code>tidy()</code> function, or the <code>summary()</code> function,
the posterior distribution is marginalized twice over. First, we take
the average <em>across</em> units but <em>within</em> each iteration of
the MCMC chain, according to what the user requested in <code>by</code>
argument or <code>tidy()/summary()</code> functions. Then, we identify
the center of the resulting posterior using the function supplied to the
<code>“marginaleffects_posterior_center”</code> option (the median by
default).

## Equivalence, Inferiority, Superiority

*θ* is an estimate, *σ*<sub>*θ*</sub> its estimated standard error, and
\[*a*,*b*\] are the bounds of the interval supplied to the
<code>equivalence</code> argument.

Non-inferiority:

<ul>
<li>

*H*<sub>0</sub>: *θ* ≤ *a*

</li>
<li>

*H*<sub>1</sub>: *θ* \> *a*

</li>
<li>

*t* = (*θ*−*a*)/*σ*<sub>*θ*</sub>

</li>
<li>

p: Upper-tail probability

</li>
</ul>

Non-superiority:

<ul>
<li>

*H*<sub>0</sub>: *θ* ≥ *b*

</li>
<li>

*H*<sub>1</sub>: *θ* \< *b*

</li>
<li>

*t* = (*θ*−*b*)/*σ*<sub>*θ*</sub>

</li>
<li>

p: Lower-tail probability

</li>
</ul>

Equivalence: Two One-Sided Tests (TOST)

<ul>
<li>

p: Maximum of the non-inferiority and non-superiority p values.

</li>
</ul>

Thanks to Russell V. Lenth for the excellent <code>emmeans</code>
package and documentation which inspired this feature.

## Prediction types

The <code>type</code> argument determines the scale of the predictions
used to compute quantities of interest with functions from the
<code>marginaleffects</code> package. Admissible values for
<code>type</code> depend on the model object. When users specify an
incorrect value for <code>type</code>, <code>marginaleffects</code> will
raise an informative error with a list of valid <code>type</code> values
for the specific model object. The first entry in the list in that error
message is the default type.

The <code>invlink(link)</code> is a special type defined by
<code>marginaleffects</code>. It is available for some (but not all)
models and functions. With this link type, we first compute predictions
on the link scale, then we use the inverse link function to
backtransform the predictions to the response scale. This is useful for
models with non-linear link functions as it can ensure that confidence
intervals stay within desirable bounds, ex: 0 to 1 for a logit model.
Note that an average of estimates with <code>type=“invlink(link)”</code>
will not always be equivalent to the average of estimates with
<code>type=“response”</code>.

Some of the most common <code>type</code> values are:

response, link, E, Ep, average, class, conditional, count, cum.prob,
cumprob, density, disp, ev, expected, expvalue, fitted, invlink(link),
latent, linear.predictor, linpred, location, lp, mean, numeric, p, ppd,
pr, precision, prediction, prob, probability, probs, quantile, risk,
scale, survival, unconditional, utility, variance, xb, zero, zlink,
zprob

## References

<ul>
<li>

Greenland S. 2019. "Valid P-Values Behave Exactly as They Should: Some
Misleading Criticisms of P-Values and Their Resolution With S-Values."
The American Statistician. 73(S1): 106–114.

</li>
<li>

Cole, Stephen R, Jessie K Edwards, and Sander Greenland. 2020.
"Surprise!" American Journal of Epidemiology 190 (2): 191–93.
https://doi.org/10.1093/aje/kwaa136

</li>
</ul>

## Examples

``` r
library(marginaleffects)

library(marginaleffects)

# simple marginal means for each level of `cyl`
dat <- mtcars
dat$carb <- factor(dat$carb)
dat$cyl <- factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- lm(mpg ~ carb + cyl + am, dat)

marginal_means(
  mod,
  variables = "cyl")
```


     Term Value Mean Std. Error    z Pr(>|z|)     S 2.5 % 97.5 %
      cyl     4 23.1       1.66 13.9   <0.001 144.3  19.9   26.4
      cyl     6 20.4       1.34 15.2   <0.001 171.9  17.8   23.0
      cyl     8 16.2       1.07 15.1   <0.001 169.0  14.1   18.3

    Results averaged over levels of: carb, am, cyl 
    Columns: term, value, cyl, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
    Type:  response 

``` r
# collapse levels of cyl by averaging
by <- data.frame(
  cyl = c(4, 6, 8),
  by = c("4 &amp; 6", "4 &amp; 6", "8"))
marginal_means(mod,
  variables = "cyl",
  by = by)
```


            By Mean Std. Error    z Pr(>|z|)     S 2.5 % 97.5 %
     4 &amp; 6 21.7       1.13 19.2   <0.001 270.8  19.5   24.0
     8         16.2       1.07 15.1   <0.001 169.0  14.1   18.3

    Results averaged over levels of: carb, am, cyl 
    Columns: by, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
    Type:  response 

``` r
# pairwise differences between collapsed levels
marginal_means(mod,
  variables = "cyl",
  by = by,
  hypothesis = "pairwise")
```


              Term Mean Std. Error    z Pr(>|z|)    S 2.5 % 97.5 %
     4 &amp; 6 - 8 5.54       1.51 3.66   <0.001 12.0  2.57    8.5

    Results averaged over levels of: carb, am, cyl 
    Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
    Type:  response 

``` r
# cross
marginal_means(mod,
  variables = c("cyl", "carb"),
  cross = TRUE)
```


     Mean Std. Error     z Pr(>|z|)     S 2.5 % 97.5 %
     25.8       1.26 20.43   <0.001 305.7 23.34   28.3
     25.6       1.17 21.93   <0.001 351.8 23.30   27.9
     25.3       2.37 10.71   <0.001  86.6 20.70   30.0
     21.9       1.90 11.51   <0.001  99.4 18.15   25.6
     20.3       3.77  5.39   <0.001  23.7 12.91   27.7
     19.8       3.81  5.18   <0.001  22.1 12.29   27.2
     23.1       1.77 13.08   <0.001 127.4 19.63   26.5
     22.9       1.87 12.24   <0.001 112.0 19.20   26.5
     22.6       2.37  9.56   <0.001  69.5 17.98   27.2
     19.1       1.34 14.31   <0.001 151.8 16.53   21.8
     17.6       3.00  5.85   <0.001  27.6 11.68   23.5
     17.0       3.48  4.89   <0.001  19.9 10.21   23.9
     18.9       1.94  9.74   <0.001  72.1 15.11   22.7
     18.7       1.57 11.90   <0.001 106.0 15.61   21.8
     18.4       1.83 10.07   <0.001  76.8 14.85   22.0
     15.0       1.20 12.53   <0.001 117.2 12.63   17.3
     13.4       3.36  3.99   <0.001  13.9  6.81   20.0
     12.9       3.00  4.28   <0.001  15.7  6.98   18.8

    Results averaged over levels of: am 
    Columns: cyl, carb, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
    Type:  response 

``` r
# collapsed cross
by <- expand.grid(
  cyl = unique(mtcars$cyl),
  carb = unique(mtcars$carb))
by$by <- ifelse(
  by$cyl == 4,
  paste("Control:", by$carb),
  paste("Treatment:", by$carb))


# Convert numeric variables to categorical before fitting the model
dat <- mtcars
dat$am <- as.logical(dat$am)
dat$carb <- as.factor(dat$carb)
mod <- lm(mpg ~ hp + am + carb, data = dat)

# Compute and summarize marginal means
marginal_means(mod)
```


     Term Value Mean Std. Error     z Pr(>|z|)     S 2.5 % 97.5 %
     am   FALSE 17.9      1.244 14.37   <0.001 153.0  15.4   20.3
     am   TRUE  23.1      0.974 23.72   <0.001 410.9  21.2   25.0
     carb 1     22.0      1.345 16.35   <0.001 197.2  19.4   24.6
     carb 2     21.5      1.025 20.95   <0.001 321.5  19.5   23.5
     carb 3     20.6      1.780 11.55   <0.001 100.1  17.1   24.0
     carb 4     18.8      1.042 18.06   <0.001 239.9  16.8   20.9
     carb 6     18.5      3.019  6.12   <0.001  30.0  12.6   24.4
     carb 8     21.6      4.055  5.33   <0.001  23.3  13.7   29.6

    Results averaged over levels of: hp, am, carb 
    Columns: term, value, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
    Type:  response 

``` r
# Contrast between marginal means (carb2 - carb1), or "is the 1st marginal means equal to the 2nd?"
# see the vignette on "Hypothesis Tests and Custom Contrasts" on the `marginaleffects` website.
lc <- c(-1, 1, 0, 0, 0, 0)
marginal_means(mod, variables = "carb", hypothesis = "b2 = b1")
```


      Term   Mean Std. Error      z Pr(>|z|)   S 2.5 % 97.5 %
     b2=b1 -0.514       1.48 -0.348    0.728 0.5 -3.41   2.38

    Results averaged over levels of: am, carb 
    Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
    Type:  response 

``` r
marginal_means(mod, variables = "carb", hypothesis = lc)
```


       Term   Mean Std. Error      z Pr(>|z|)   S 2.5 % 97.5 %
     custom -0.514       1.48 -0.348    0.728 0.5 -3.41   2.38

    Results averaged over levels of: am, carb 
    Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
    Type:  response 

``` r
# Multiple custom contrasts
lc <- matrix(c(
    -2, 1, 1, 0, -1, 1,
    -1, 1, 0, 0, 0, 0
    ),
  ncol = 2,
  dimnames = list(NULL, c("A", "B")))
marginal_means(mod, variables = "carb", hypothesis = lc)
```


     Term   Mean Std. Error      z Pr(>|z|)   S  2.5 % 97.5 %
        A  1.199       6.15  0.195    0.845 0.2 -10.85  13.25
        B -0.514       1.48 -0.348    0.728 0.5  -3.41   2.38

    Results averaged over levels of: am, carb 
    Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
    Type:  response 
