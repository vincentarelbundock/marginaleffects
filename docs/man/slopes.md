
# slopes

Slopes (aka Partial derivatives, Marginal Effects, or Trends)

## Description

Partial derivative of the regression equation with respect to a
regressor of interest.

<ul>
<li>

<code>slopes()</code>: unit-level (conditional) estimates.

</li>
<li>

<code>avg_slopes()</code>: average (marginal) estimates.

</li>
</ul>

The <code>newdata</code> argument and the <code>datagrid()</code>
function can be used to control where statistics are evaluated in the
predictor space: "at observed values", "at the mean", "at representative
values", etc.

See the slopes vignette and package website for worked examples and case
studies:

<ul>
<li>

<a href="https://marginaleffects.com/articles/slopes.html">https://marginaleffects.com/articles/slopes.html</a>

</li>
<li>

<a href="https://marginaleffects.com/">https://marginaleffects.com/</a>

</li>
</ul>

## Usage

<pre><code class='language-R'>slopes(
  model,
  newdata = NULL,
  variables = NULL,
  type = NULL,
  by = FALSE,
  vcov = TRUE,
  conf_level = 0.95,
  slope = "dydx",
  wts = NULL,
  hypothesis = NULL,
  equivalence = NULL,
  p_adjust = NULL,
  df = Inf,
  eps = NULL,
  numderiv = "fdforward",
  ...
)

avg_slopes(
  model,
  newdata = NULL,
  variables = NULL,
  type = NULL,
  by = TRUE,
  vcov = TRUE,
  conf_level = 0.95,
  slope = "dydx",
  wts = NULL,
  hypothesis = NULL,
  equivalence = NULL,
  p_adjust = NULL,
  df = Inf,
  eps = NULL,
  numderiv = "fdforward",
  ...
)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="slopes_:_model">model</code>
</td>
<td>
Model object
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="slopes_:_newdata">newdata</code>
</td>
<td>

Grid of predictor values at which we evaluate the slopes.

<ul>
<li>

Warning: Please avoid modifying your dataset between fitting the model
and calling a <code>marginaleffects</code> function. This can sometimes
lead to unexpected results.

</li>
<li>

<code>NULL</code> (default): Unit-level slopes for each observed value
in the dataset (empirical distribution). The dataset is retrieved using
<code>insight::get_data()</code>, which tries to extract data from the
environment. This may produce unexpected results if the original data
frame has been altered since fitting the model.

</li>
<li>

<code>datagrid()</code> call to specify a custom grid of regressors. For
example:

<ul>
<li>

<code>newdata = datagrid(cyl = c(4, 6))</code>: <code>cyl</code>
variable equal to 4 and 6 and other regressors fixed at their means or
modes.

</li>
<li>

See the Examples section and the <code>datagrid()</code> documentation.

</li>
</ul>
</li>
<li>

string:

<ul>
<li>

"mean": Marginal Effects at the Mean. Slopes when each predictor is held
at its mean or mode.

</li>
<li>

"median": Marginal Effects at the Median. Slopes when each predictor is
held at its median or mode.

</li>
<li>

"marginalmeans": Marginal Effects at Marginal Means. See Details section
below.

</li>
<li>

"tukey": Marginal Effects at Tukey’s 5 numbers.

</li>
<li>

"grid": Marginal Effects on a grid of representative numbers (Tukey’s 5
numbers and unique values of categorical predictors).

</li>
</ul>
</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="slopes_:_variables">variables</code>
</td>
<td>

Focal variables

<ul>
<li>

<code>NULL</code>: compute slopes or comparisons for all the variables
in the model object (can be slow).

</li>
<li>

Character vector: subset of variables (usually faster).

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="slopes_:_type">type</code>
</td>
<td>
string indicates the type (scale) of the predictions used to compute
contrasts or slopes. This can differ based on the model type, but will
typically be a string such as: "response", "link", "probs", or "zero".
When an unsupported string is entered, the model-specific list of
acceptable values is returned in an error message. When
<code>type</code> is <code>NULL</code>, the first entry in the error
message is used by default.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="slopes_:_by">by</code>
</td>
<td>

Aggregate unit-level estimates (aka, marginalize, average over). Valid
inputs:

<ul>
<li>

<code>FALSE</code>: return the original unit-level estimates.

</li>
<li>

<code>TRUE</code>: aggregate estimates for each term.

</li>
<li>

Character vector of column names in <code>newdata</code> or in the data
frame produced by calling the function without the <code>by</code>
argument.

</li>
<li>

Data frame with a <code>by</code> column of group labels, and merging
columns shared by <code>newdata</code> or the data frame produced by
calling the same function without the <code>by</code> argument.

</li>
<li>

See examples below.

</li>
<li>

For more complex aggregations, you can use the <code>FUN</code> argument
of the <code>hypotheses()</code> function. See that function’s
documentation and the Hypothesis Test vignettes on the
<code>marginaleffects</code> website.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="slopes_:_vcov">vcov</code>
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
<code id="slopes_:_conf_level">conf_level</code>
</td>
<td>
numeric value between 0 and 1. Confidence level to use to build a
confidence interval.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="slopes_:_slope">slope</code>
</td>
<td>

string indicates the type of slope or (semi-)elasticity to compute:

<ul>
<li>

"dydx": dY/dX

</li>
<li>

"eyex": dY/dX \* Y / X

</li>
<li>

"eydx": dY/dX \* Y

</li>
<li>

"dyex": dY/dX / X

</li>
<li>

Y is the predicted value of the outcome; X is the observed value of the
predictor.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="slopes_:_wts">wts</code>
</td>
<td>

string or numeric: weights to use when computing average contrasts or
slopes. These weights only affect the averaging in
<code style="white-space: pre;">⁠avg\_\*()⁠</code> or with the
<code>by</code> argument, and not the unit-level estimates themselves.
Internally, estimates and weights are passed to the
<code>weighted.mean()</code> function.

<ul>
<li>

string: column name of the weights variable in <code>newdata</code>.
When supplying a column name to <code>wts</code>, it is recommended to
supply the original data (including the weights variable) explicitly to
<code>newdata</code>.

</li>
<li>

numeric: vector of length equal to the number of rows in the original
data or in <code>newdata</code> (if supplied).

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="slopes_:_hypothesis">hypothesis</code>
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
<code id="slopes_:_equivalence">equivalence</code>
</td>
<td>
Numeric vector of length 2: bounds used for the two-one-sided test
(TOST) of equivalence, and for the non-inferiority and non-superiority
tests. See Details section below.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="slopes_:_p_adjust">p_adjust</code>
</td>
<td>
Adjust p-values for multiple comparisons: "holm", "hochberg", "hommel",
"bonferroni", "BH", "BY", or "fdr". See stats::p.adjust
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="slopes_:_df">df</code>
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
<code id="slopes_:_eps">eps</code>
</td>
<td>
NULL or numeric value which determines the step size to use when
calculating numerical derivatives: (f(x+eps)-f(x))/eps. When
<code>eps</code> is <code>NULL</code>, the step size is 0.0001
multiplied by the difference between the maximum and minimum values of
the variable with respect to which we are taking the derivative.
Changing <code>eps</code> may be necessary to avoid numerical problems
in certain models.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="slopes_:_numderiv">numderiv</code>
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
<code id="slopes_:_...">…</code>
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

A "slope" or "marginal effect" is the partial derivative of the
regression equation with respect to a variable in the model. This
function uses automatic differentiation to compute slopes for a vast
array of models, including non-linear models with transformations (e.g.,
polynomials). Uncertainty estimates are computed using the delta method.

Numerical derivatives for the <code>slopes</code> function are
calculated using a simple epsilon difference approach:
∂*Y*/∂*X* = (*f*(*X*+*ε*/2)−*f*(*X*−*ε*/2))/*ε*, where f is the
<code>predict()</code> method associated with the model class, and *ε*
is determined by the <code>eps</code> argument.

## Value

A <code>data.frame</code> with one row per observation (per term/group)
and several columns:

<ul>
<li>

<code>rowid</code>: row number of the <code>newdata</code> data frame

</li>
<li>

<code>type</code>: prediction type, as defined by the <code>type</code>
argument

</li>
<li>

<code>group</code>: (optional) value of the grouped outcome (e.g.,
categorical outcome models)

</li>
<li>

<code>term</code>: the variable whose marginal effect is computed

</li>
<li>

<code>dydx</code>: slope of the outcome with respect to the term, for a
given combination of predictor values

</li>
<li>

<code>std.error</code>: standard errors computed by via the delta
method.

</li>
<li>

<code>p.value</code>: p value associated to the <code>estimate</code>
column. The null is determined by the <code>hypothesis</code> argument
(0 by default), and p values are computed before applying the
<code>transform</code> argument. For models of class <code>feglm</code>,
<code>Gam</code>, <code>glm</code> and <code>negbin</code>, p values are
computed on the link scale by default unless the <code>type</code>
argument is specified explicitly.

</li>
<li>

<code>s.value</code>: Shannon information transforms of p values. How
many consecutive "heads" tosses would provide the same amount of
evidence (or "surprise") against the null hypothesis that the coin is
fair? The purpose of S is to calibrate the analyst’s intuition about the
strength of evidence encoded in p against a well-known physical
phenomenon. See Greenland (2019) and Cole et al. (2020).

</li>
<li>

<code>conf.low</code>: lower bound of the confidence interval (or
equal-tailed interval for bayesian models)

</li>
<li>

<code>conf.high</code>: upper bound of the confidence interval (or
equal-tailed interval for bayesian models)

</li>
</ul>

See <code>?print.marginaleffects</code> for printing options.

## Functions

<ul>
<li>

<code>avg_slopes()</code>: Average slopes

</li>
</ul>

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




# Unit-level (conditional) Marginal Effects
mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
mfx <- slopes(mod)
head(mfx)

# Average Marginal Effect (AME)
avg_slopes(mod, by = TRUE)


# Marginal Effect at the Mean (MEM)
slopes(mod, newdata = datagrid())

# Marginal Effect at User-Specified Values
# Variables not explicitly included in `datagrid()` are held at their means
slopes(mod, newdata = datagrid(hp = c(100, 110)))

# Group-Average Marginal Effects (G-AME)
# Calculate marginal effects for each observation, and then take the average
# marginal effect within each subset of observations with different observed
# values for the `cyl` variable:
mod2 <- lm(mpg ~ hp * cyl, data = mtcars)
avg_slopes(mod2, variables = "hp", by = "cyl")

# Marginal Effects at User-Specified Values (counterfactual)
# Variables not explicitly included in `datagrid()` are held at their
# original values, and the whole dataset is duplicated once for each
# combination of the values in `datagrid()`
mfx <- slopes(mod,
              newdata = datagrid(hp = c(100, 110),
              grid_type = "counterfactual"))
head(mfx)

# Heteroskedasticity robust standard errors
mfx <- slopes(mod, vcov = sandwich::vcovHC(mod))
head(mfx)

# hypothesis test: is the `hp` marginal effect at the mean equal to the `drat` marginal effect
mod <- lm(mpg ~ wt + drat, data = mtcars)

slopes(
    mod,
    newdata = "mean",
    hypothesis = "wt = drat")

# same hypothesis test using row indices
slopes(
    mod,
    newdata = "mean",
    hypothesis = "b1 - b2 = 0")

# same hypothesis test using numeric vector of weights
slopes(
    mod,
    newdata = "mean",
    hypothesis = c(1, -1))

# two custom contrasts using a matrix of weights
lc <- matrix(c(
    1, -1,
    2, 3),
    ncol = 2)
colnames(lc) <- c("Contrast A", "Contrast B")
slopes(
    mod,
    newdata = "mean",
    hypothesis = lc)
```
