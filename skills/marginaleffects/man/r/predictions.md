## Predictions {.unnumbered}


### Description

Outcome predicted by a fitted model on a specified scale for a given combination of values of the predictor variables, such as their observed values, their means, or factor levels (a.k.a. &quot;reference grid&quot;).


<ul>
<li> <code>predictions()</code>: unit-level (conditional) estimates.

</li>
<li> <code>avg_predictions()</code>: average (marginal) estimates.

</li></ul>

The <code>newdata</code> argument and the <code>datagrid()</code> function can be used to control where statistics are evaluated in the predictor space: &quot;at observed values&quot;, &quot;at the mean&quot;, &quot;at representative values&quot;, etc.

See the predictions vignette and package website for worked examples and case studies:


<ul>
<li> <a href="https://marginaleffects.com/chapters/predictions.html">https://marginaleffects.com/chapters/predictions.html</a>

</li>
<li> <a href="https://marginaleffects.com/">https://marginaleffects.com/</a>

</li></ul>



### Usage

<pre><code class='language-R'>predictions(
  model,
  newdata = NULL,
  variables = NULL,
  vcov = TRUE,
  conf_level = 0.95,
  type = NULL,
  by = FALSE,
  byfun = NULL,
  wts = FALSE,
  transform = NULL,
  hypothesis = NULL,
  equivalence = NULL,
  df = Inf,
  numderiv = "fdforward",
  ...
)

avg_predictions(
  model,
  newdata = NULL,
  variables = NULL,
  vcov = TRUE,
  conf_level = 0.95,
  type = NULL,
  by = TRUE,
  byfun = NULL,
  wts = FALSE,
  transform = NULL,
  hypothesis = NULL,
  equivalence = NULL,
  df = Inf,
  numderiv = "fdforward",
  ...
)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="model">model</code></td>
<td>
Model object
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="newdata">newdata</code></td>
<td>
Grid of predictor values at which we evaluate predictions.


<ul>
<li> Warning: Please avoid modifying your dataset between fitting the model and calling a <code>marginaleffects</code> function. This can sometimes lead to unexpected results.

</li>
<li> <code>NULL</code> (default): Unit-level predictions for each observed value in the dataset (empirical distribution). The dataset is retrieved using <code>insight::get_data()</code>, which tries to extract data from the environment. This may produce unexpected results if the original data frame has been altered since fitting the model.

</li>
<li> string:


<ul>
<li> &quot;mean&quot;: Predictions evaluated when each predictor is held at its mean or mode.

</li>
<li> &quot;median&quot;: Predictions evaluated when each predictor is held at its median or mode.

</li>
<li> &quot;balanced&quot;: Predictions evaluated on a balanced grid with every combination of categories and numeric variables held at their means.

</li>
<li> &quot;tukey&quot;: Predictions evaluated at Tukey's 5 numbers.

</li>
<li> &quot;grid&quot;: Predictions evaluated on a grid of representative numbers (Tukey's 5 numbers and unique values of categorical predictors).

</li></ul>

</li>
<li> <code>datagrid()</code> call to specify a custom grid of regressors. For example:


<ul>
<li> <code>newdata = datagrid(cyl = c(4, 6))</code>: <code>cyl</code> variable equal to 4 and 6 and other regressors fixed at their means or modes.

</li>
<li> See the Examples section and the <code>datagrid()</code> documentation.

</li></ul>

</li>
<li> <code>subset()</code> call with a single argument to select a subset of the dataset used to fit the model, ex: <code>newdata = subset(treatment == 1)</code>

</li>
<li> <code>dplyr::filter()</code> call with a single argument to select a subset of the dataset used to fit the model, ex: <code>newdata = filter(treatment == 1)</code>

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="variables">variables</code></td>
<td>
Counterfactual variables.


<ul>
<li> Output:


<ul>
<li> <code>predictions()</code>: The entire dataset is replicated once for each unique combination of <code>variables</code>, and predictions are made.

</li>
<li> <code>avg_predictions()</code>: The entire dataset is replicated, predictions are made, and they are marginalized by <code>variables</code> categories.

</li>
<li> Warning: This can be expensive in large datasets.

</li>
<li> Warning: Users who need &quot;conditional&quot; predictions should use the <code>newdata</code> argument instead of <code>variables</code>.

</li></ul>

</li>
<li> Input:


<ul>
<li> <code>NULL</code>: computes one prediction per row of <code>newdata</code>

</li>
<li> Character vector: the dataset is replicated once of every combination of unique values of the variables identified in <code>variables</code>.

</li>
<li> Named list: names identify the subset of variables of interest and their values. For numeric variables, the <code>variables</code> argument supports functions and string shortcuts:


<ul>
<li> A function which returns a numeric value

</li>
<li> Numeric vector: Contrast between the 2nd element and the 1st element of the <code>x</code> vector.

</li>
<li> &quot;iqr&quot;: Contrast across the interquartile range of the regressor.

</li>
<li> &quot;sd&quot;: Contrast across one standard deviation around the regressor mean.

</li>
<li> &quot;2sd&quot;: Contrast across two standard deviations around the regressor mean.

</li>
<li> &quot;minmax&quot;: Contrast between the maximum and the minimum values of the regressor.

</li>
<li> &quot;threenum&quot;: mean and 1 standard deviation on both sides

</li>
<li> &quot;fivenum&quot;: Tukey's five numbers

</li></ul>

</li></ul>

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="vcov">vcov</code></td>
<td>
Type of uncertainty estimates to report (e.g., for robust standard errors). Acceptable values:


<ul>
<li> FALSE: Do not compute standard errors. This can speed up computation considerably.

</li>
<li> TRUE: Unit-level standard errors using the default <code>vcov(model)</code> variance-covariance matrix.

</li>
<li> String which indicates the kind of uncertainty estimates to return.


<ul>
<li> Heteroskedasticity-consistent: <code>"HC"</code>, <code>"HC0"</code>, <code>"HC1"</code>, <code>"HC2"</code>, <code>"HC3"</code>, <code>"HC4"</code>, <code>"HC4m"</code>, <code>"HC5"</code>. See <code>?sandwich::vcovHC</code>

</li>
<li> Heteroskedasticity and autocorrelation consistent: <code>"HAC"</code>

</li>
<li> Mixed-Models degrees of freedom: &quot;satterthwaite&quot;, &quot;kenward-roger&quot;

</li>
<li> Other: <code>"NeweyWest"</code>, <code>"KernHAC"</code>, <code>"OPG"</code>. See the <code>sandwich</code> package documentation.

</li>
<li> &quot;rsample&quot;, &quot;boot&quot;, &quot;fwb&quot;, and &quot;simulation&quot; are passed to the <code>method</code> argument of the <code>inferences()</code> function. To customize the bootstrap or simulation process, call <code>inferences()</code> directly.

</li></ul>

</li>
<li> One-sided formula which indicates the name of cluster variables (e.g., <code>~unit_id</code>). This formula is passed to the <code>cluster</code> argument of the <code>sandwich::vcovCL</code> function.

</li>
<li> Square covariance matrix

</li>
<li> Function which returns a covariance matrix (e.g., <code>stats::vcov(model)</code>)

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="conf_level">conf_level</code></td>
<td>
numeric value between 0 and 1. Confidence level to use to build a confidence interval.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="type">type</code></td>
<td>
string indicates the type (scale) of the predictions used to
compute contrasts or slopes. This can differ based on the model
type, but will typically be a string such as: &quot;response&quot;, &quot;link&quot;, &quot;probs&quot;,
or &quot;zero&quot;. When an unsupported string is entered, the model-specific list of
acceptable values is returned in an error message. When <code>type</code> is <code>NULL</code>, the
first entry in the error message is used by default. See the Type section in the documentation below.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="by">by</code></td>
<td>
Aggregate unit-level estimates (aka, marginalize, average over). Valid inputs:


<ul>
<li> <code>FALSE</code>: return the original unit-level estimates.

</li>
<li> <code>TRUE</code>: aggregate estimates for each term.

</li>
<li> Character vector of column names in <code>newdata</code> or in the data frame produced by calling the function without the <code>by</code> argument.

</li>
<li> Data frame with a <code>by</code> column of group labels, and merging columns shared by <code>newdata</code> or the data frame produced by calling the same function without the <code>by</code> argument.

</li>
<li> See examples below.

</li>
<li> For more complex aggregations, you can use the <code>FUN</code> argument of the <code>hypotheses()</code> function. See that function's documentation and the Hypothesis Test vignettes on the <code>marginaleffects</code> website.

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="byfun">byfun</code></td>
<td>
A function such as <code>mean()</code> or <code>sum()</code> used to aggregate
estimates within the subgroups defined by the <code>by</code> argument. <code>NULL</code> uses the
<code>mean()</code> function. Must accept a numeric vector and return a single numeric
value. This is sometimes used to take the sum or mean of predicted
probabilities across outcome or predictor
levels. See examples section.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="wts">wts</code></td>
<td>
logical, string or numeric: weights to use when computing average predictions, contrasts or slopes. These weights only affect the averaging in <code style="white-space: pre;">avg_*()</code> or with the <code>by</code> argument, and not unit-level estimates. See <code>?weighted.mean</code>


<ul>
<li> string: column name of the weights variable in <code>newdata</code>. When supplying a column name to <code>wts</code>, it is recommended to supply the original data (including the weights variable) explicitly to <code>newdata</code>.

</li>
<li> numeric: vector of length equal to the number of rows in the original data or in <code>newdata</code> (if supplied).

</li>
<li> FALSE: Equal weights.

</li>
<li> TRUE: Extract weights from the fitted object with <code>insight::find_weights()</code> and use them when taking weighted averages of estimates. Warning: <code>newdata=datagrid()</code> returns a single average weight, which is equivalent to using <code>wts=FALSE</code>

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="transform">transform</code></td>
<td>
A function applied to unit-level adjusted predictions and confidence intervals just before the function returns results. For bayesian models, this function is applied to individual draws from the posterior distribution, before computing summaries.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="hypothesis">hypothesis</code></td>
<td>
specify a hypothesis test or custom contrast using a number , formula, string equation, vector, matrix, or function.


<ul>
<li> Number: The null hypothesis used in the computation of Z and p (before applying <code>transform</code>).

</li>
<li> String: Equation to specify linear or non-linear hypothesis tests. Two-tailed tests must include an equal <code>=</code> sign. One-tailed tests must start with <code>&lt;</code> or <code>&gt;</code>. If the terms in <code>coef(object)</code> uniquely identify estimates, they can be used in the formula. Otherwise, use <code>b1</code>, <code>b2</code>, etc. to identify the position of each parameter. The <code style="white-space: pre;">b*</code> wildcard can be used to test hypotheses on all estimates. When the hypothesis string represents a two-sided equation, the <code>estimate</code> column holds the value of the left side minus the right side of the equation. If a named vector is used, the names are used as labels in the output. Examples:


<ul>
<li> <code>hp = drat</code>

</li>
<li> <code>hp + drat = 12</code>

</li>
<li> <code>b1 + b2 + b3 = 0</code>

</li>
<li> <code style="white-space: pre;">b* / b1 = 1</code>

</li>
<li> <code style="white-space: pre;">&lt;= 0</code>

</li>
<li> <code style="white-space: pre;">&gt;= -3.5</code>

</li>
<li> <code>b1 &gt;= 10</code>

</li></ul>

</li>
<li> Formula: <code>lhs ~ rhs | group</code>


<ul>
<li> <code>lhs</code>


<ul>
<li> <code>ratio</code> (null = 1)

</li>
<li> <code>difference</code> (null = 0)

</li>
<li> Leave empty for default value

</li></ul>

</li>
<li> <code>rhs</code>


<ul>
<li> <code>pairwise</code> and <code>revpairwise</code>: pairwise differences between estimates in each row.

</li>
<li> <code>reference</code>: differences between the estimates in each row and the estimate in the first row.

</li>
<li> <code>sequential</code>: difference between an estimate and the estimate in the next row.

</li>
<li> <code>meandev</code>: difference between an estimate and the mean of all estimates.

</li>
<li> <code>meanotherdev</code>: difference between an estimate and the mean of all other estimates, excluding the current one.

</li>
<li> <code>poly</code>: polynomial contrasts, as computed by the <code>stats::contr.poly()</code> function.

</li>
<li> <code>helmert</code>: Helmert contrasts, as computed by the <code>stats::contr.helmert()</code> function. Contrast 2nd level to the first, 3rd to the average of the first two, and so on.

</li>
<li> <code>trt_vs_ctrl</code>: difference between the mean of estimates (except the first) and the first estimate.

</li>
<li> <code>I(fun(x))</code>: custom function to manipulate the vector of estimates <code>x</code>. The function <code>fun()</code> can return multiple (potentially named) estimates.

</li></ul>

</li>
<li> <code>group</code> (optional)


<ul>
<li> Column name of <code>newdata</code>. Conduct hypothesis tests withing subsets of the data.

</li></ul>

</li>
<li> Examples:


<ul>
<li> <code>~ poly</code>

</li>
<li> <code>~ sequential | groupid</code>

</li>
<li> <code>~ reference</code>

</li>
<li> <code>ratio ~ pairwise</code>

</li>
<li> <code>difference ~ pairwise | groupid</code>

</li>
<li> <code>~ I(x - mean(x)) | groupid</code>

</li>
<li> <code style="white-space: pre;">~ I(\(x) c(a = x[1], b = mean(x[2:3]))) | groupid</code>

</li></ul>

</li></ul>

</li>
<li> Matrix or Vector: Each column is a vector of weights. The the output is the dot product between these vectors of weights and the vector of estimates. The matrix can have column names to label the estimates.

</li>
<li> Function:


<ul>
<li> Accepts an argument <code>x</code>: object produced by a <code>marginaleffects</code> function or a data frame with column <code>rowid</code> and <code>estimate</code>

</li>
<li> Returns a data frame with columns <code>term</code> and <code>estimate</code> (mandatory) and <code>rowid</code> (optional).

</li>
<li> The function can also accept optional input arguments: <code>newdata</code>, <code>by</code>, <code>draws</code>.

</li>
<li> This function approach will not work for Bayesian models or with bootstrapping. In those cases, it is easy to use <code>get_draws()</code> to extract and manipulate the draws directly.

</li></ul>

</li>
<li> See the Examples section below and the vignette: <a href="https://marginaleffects.com/chapters/hypothesis.html">https://marginaleffects.com/chapters/hypothesis.html</a>

</li>
<li> Warning: When calling <code>predictions()</code> with <code>type="invlink(link)"</code> (the default in some models), <code>hypothesis</code> is tested and p values are computed on the link scale.

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="equivalence">equivalence</code></td>
<td>
Numeric vector of length 2: bounds used for the two-one-sided test (TOST) of equivalence, and for the non-inferiority and non-superiority tests. For bayesian models, this report the proportion of posterior draws in the interval and the ROPE. See Details section below.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="df">df</code></td>
<td>
Degrees of freedom used to compute p values and confidence intervals.


<ul>
<li> A single numeric value between 1 and <code>Inf</code>, or a numeric vector with length equal to the number of rows in the output. When <code>df</code> is <code>Inf</code>, the normal distribution is used. When <code>df</code> is finite, the <code>t</code> distribution is used.

</li>
<li> &quot;residual&quot;: Calls insight::get_df to extract degrees of freedom from the model automatically.

</li>
<li> &quot;satterthwaite&quot; or &quot;kenward-roger&quot;: Use the Satterthwaite or Kenward-Roger approximation to compute degrees of freedom in mixed effects models.

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="numderiv">numderiv</code></td>
<td>
string or list of strings indicating the method to use to for the numeric differentiation used in to compute delta method standard errors.


<ul>
<li> &quot;fdforward&quot;: finite difference method with forward differences (default)

</li>
<li> &quot;fdcenter&quot;: finite difference method with central differences

</li>
<li> &quot;richardson&quot;: Richardson extrapolation method

</li>
<li> Extra arguments can be specified by passing a list to the <code>numDeriv</code> argument, with the name of the method first and named arguments following, ex: <code>numderiv=list("fdcenter", eps = 1e-5)</code>. When an unknown argument is used, <code>marginaleffects</code> prints the list of valid arguments for each method.

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="...">...</code></td>
<td>
Additional arguments are passed to the <code>predict()</code> method
supplied by the modeling package.These arguments are particularly useful
for mixed-effects or bayesian models (see the online vignettes on the
<code>marginaleffects</code> website). Available arguments can vary from model to
model, depending on the range of supported arguments by each modeling
package. See the &quot;Model-Specific Arguments&quot; section of the
<code>?slopes</code> documentation for a non-exhaustive list of available
arguments.
</td></tr>
</table>


### Value

A <code>data.frame</code> with one row per estimate. This data frame is pretty-printed by default, but users can interact with it as a regular data frame, with functions like <code>nrow()</code>, <code>head()</code>, <code>colnames()</code>, etc. Values can be extracted using standard <code style="white-space: pre;">[,]</code> or <code>\$</code> operators, and manipulated using external packages like <code>dplyr</code> or <code>data.table</code>.

Columns may include:


<ul>
<li> <code>rowid</code>: row number of the <code>newdata</code> data frame

</li>
<li> <code>group</code>: (optional) value of the grouped outcome (e.g., categorical outcome models)

</li>
<li> <code>term</code>: the focal variable.

</li>
<li> <code>estimate</code>: an estimate of the prediction, counterfactual comparison, or slope.

</li>
<li> <code>std.error</code>: standard errors computed via the delta method.

</li>
<li> <code>p.value</code>: p value associated to the <code>estimate</code> column. The null is determined by the <code>hypothesis</code> argument (0 by default).

</li>
<li> <code>s.value</code>: Shannon information transforms of p values. See the S values vignette at <a href="https://marginaleffects.com">https://marginaleffects.com</a> the marginaleffects website.

</li>
<li> <code>conf.low</code>: lower bound of the confidence (or credible) interval defined by the <code>conf_level</code> argument.

</li>
<li> <code>conf.high</code>: upper bound of the confidence (or credible) interval defined by the <code>conf_level</code> argument.

</li>
<li> <code>predicted_lo</code>: predicted outcome for the &quot;low&quot; value of the focal predictor in a counterfactual comparison.

</li>
<li> <code>predicted_hi</code>: predicted outcome for the &quot;high&quot; value of the focal predictor in a counterfactual comparison.

</li>
<li> <code>p.rope.unconditional</code>: share of posterior draws in the interval specified by the <code>equivalence</code> argument. This is only available for Bayesian models.

</li>
<li> <code>p.rope.conditional</code>: share of posterior draws in the interval specified by the <code>equivalence</code> argument, among draws in the confidence interval. This is only available for Bayesian models.

</li>
<li> <code>rope</code>: share of the posterior draws between <code>conf.low</code> and <code>conf.high</code> that are covered by the interval specified by the <code>equivalence</code> argument.

</li>
<li> <code>statistic.noninf</code>: test statistic for non-inferiority test (when <code>equivalence</code> argument is used).

</li>
<li> <code>statistic.nonsup</code>: test statistic for non-superiority test (when <code>equivalence</code> argument is used).

</li>
<li> <code>p.value.noninf</code>: p-value for non-inferiority test (when <code>equivalence</code> argument is used).

</li>
<li> <code>p.value.nonsup</code>: p-value for non-superiority test (when <code>equivalence</code> argument is used).

</li>
<li> <code>p.value.equiv</code>: p-value for equivalence test using Two One-Sided Tests (TOST) approach (when <code>equivalence</code> argument is used).

</li></ul>

See <code>?print.marginaleffects</code> for printing options.

The <code>data.frame</code>s produced by <code>marginaleffects</code> stores an attribute that holds many internal objects, such as the original model, data, and much other information that can be used for post-processing. This information can be inspected using the <code>components()</code> function.

Warning: The internal attributes retrieved by <code>components()</code> are not considered part of the public API of the package. Their names and contents can change without warning or notice. Users should not rely on them.

Warning: In some cases, the internal attributes used by <code>marginaleffects()</code> can use up a substantial amount of memory. To clear this data, use the <code>prune()</code> function or set <code>options(marginaleffects_lean=TRUE)</code>.



### Functions


<ul>
<li> <code>avg_predictions()</code>: Average predictions

</li></ul>


### Standard errors using the delta method

Standard errors for all quantities estimated by <code>marginaleffects</code> can be obtained via the delta method. This requires differentiating a function with respect to the coefficients in the model using a finite difference approach. In some models, the delta method standard errors can be sensitive to various aspects of the numeric differentiation strategy, including the step size. By default, the step size is set to <code>1e-8</code>, or to <code>1e-4</code> times the smallest absolute model coefficient, whichever is largest.

<code>marginaleffects</code> can delegate numeric differentiation to the <code>numDeriv</code> package, which allows more flexibility. To do this, users can pass arguments to the <code>numDeriv::jacobian</code> function through a global option. For example:


<ul>
<li> <code>options(marginaleffects_numDeriv = list(method = "simple", method.args = list(eps = 1e-6)))</code>

</li>
<li> <code>options(marginaleffects_numDeriv = list(method = "Richardson", method.args = list(eps = 1e-5)))</code>

</li>
<li> <code>options(marginaleffects_numDeriv = NULL)</code>

</li></ul>

See the &quot;Uncertainty&quot; chapter on the <code>marginaleffects</code> website for more details on the computation of standard errors, bootstrapping, and more:

https://marginaleffects.com/chapters/uncertainty.html



### Model-Specific Arguments

Some model types allow model-specific arguments to modify the nature of
marginal effects, predictions, marginal means, and contrasts. Please report
other package-specific <code>predict()</code> arguments on Github so we can add them to
the table below.

https://github.com/vincentarelbundock/marginaleffects/issues

<table>
<tr>
 <td style="text-align: left;">
   Package </td><td style="text-align: left;"> Class </td><td style="text-align: left;"> Argument </td><td style="text-align: left;"> Documentation </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>brms</code> </td><td style="text-align: left;"> <code>brmsfit</code> </td><td style="text-align: left;"> <code>ndraws</code> </td><td style="text-align: left;"> brms::posterior_predict </td>
</tr>
<tr>
 <td style="text-align: left;">
    </td><td style="text-align: left;">  </td><td style="text-align: left;"> <code>re_formula</code> </td><td style="text-align: left;"> brms::posterior_predict </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>lme4</code> </td><td style="text-align: left;"> <code>merMod</code> </td><td style="text-align: left;"> <code>re.form</code> </td><td style="text-align: left;"> lme4::predict.merMod </td>
</tr>
<tr>
 <td style="text-align: left;">
    </td><td style="text-align: left;">  </td><td style="text-align: left;"> <code>allow.new.levels</code> </td><td style="text-align: left;"> lme4::predict.merMod </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>glmmTMB</code> </td><td style="text-align: left;"> <code>glmmTMB</code> </td><td style="text-align: left;"> <code>re.form</code> </td><td style="text-align: left;"> glmmTMB::predict.glmmTMB </td>
</tr>
<tr>
 <td style="text-align: left;">
    </td><td style="text-align: left;">  </td><td style="text-align: left;"> <code>allow.new.levels</code> </td><td style="text-align: left;"> glmmTMB::predict.glmmTMB </td>
</tr>
<tr>
 <td style="text-align: left;">
    </td><td style="text-align: left;">  </td><td style="text-align: left;"> <code>zitype</code> </td><td style="text-align: left;"> glmmTMB::predict.glmmTMB </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>mgcv</code> </td><td style="text-align: left;"> <code>bam</code> </td><td style="text-align: left;"> <code>exclude</code> </td><td style="text-align: left;"> mgcv::predict.bam </td>
</tr>
<tr>
 <td style="text-align: left;">
    </td><td style="text-align: left;"> <code>gam</code> </td><td style="text-align: left;"> <code>exclude</code> </td><td style="text-align: left;"> mgcv::predict.gam </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>robustlmm</code> </td><td style="text-align: left;"> <code>rlmerMod</code> </td><td style="text-align: left;"> <code>re.form</code> </td><td style="text-align: left;"> robustlmm::predict.rlmerMod </td>
</tr>
<tr>
 <td style="text-align: left;">
    </td><td style="text-align: left;">  </td><td style="text-align: left;"> <code>allow.new.levels</code> </td><td style="text-align: left;"> robustlmm::predict.rlmerMod </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>MCMCglmm</code> </td><td style="text-align: left;"> <code>MCMCglmm</code> </td><td style="text-align: left;"> <code>ndraws</code> </td><td style="text-align: left;">  </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>sampleSelection</code> </td><td style="text-align: left;"> <code>selection</code> </td><td style="text-align: left;"> <code>part</code> </td><td style="text-align: left;"> sampleSelection::predict.selection </td>
</tr>
<tr>
 <td style="text-align: left;">
</td>
</tr>

</table>



### Bayesian posterior summaries

By default, credible intervals in bayesian models are built as equal-tailed
intervals. This can be changed to a highest density interval by setting a global
option:

<code>options("marginaleffects_posterior_interval" = "eti")</code>

<code>options("marginaleffects_posterior_interval" = "hdi")</code>

By default, the center of the posterior distribution in bayesian models is
identified by the median. Users can use a different summary function by setting a
global option:

<code>options("marginaleffects_posterior_center" = "mean")</code>

<code>options("marginaleffects_posterior_center" = "median")</code>

When estimates are averaged using the <code>by</code> argument, the <code>tidy()</code> function, or
the <code>summary()</code> function, the posterior distribution is marginalized twice over.
First, we take the average <em>across</em> units but <em>within</em> each iteration of the
MCMC chain, according to what the user requested in <code>by</code> argument or
<code>tidy()/summary()</code> functions. Then, we identify the center of the resulting
posterior using the function supplied to the
<code>"marginaleffects_posterior_center"</code> option (the median by default).



### Equivalence, Inferiority, Superiority

$\theta$ is an estimate, $\sigma_\theta$ its estimated standard error, and $[a, b]$ are the bounds of the interval supplied to the <code>equivalence</code> argument.

Non-inferiority:


<ul>
<li> $H_0$: $\theta \leq a$

</li>
<li> $H_1$: $\theta > a$

</li>
<li> $t=(\theta - a)/\sigma_\theta$

</li>
<li> p: Upper-tail probability

</li></ul>

Non-superiority:


<ul>
<li> $H_0$: $\theta \geq b$

</li>
<li> $H_1$: $\theta < b$

</li>
<li> $t=(\theta - b)/\sigma_\theta$

</li>
<li> p: Lower-tail probability

</li></ul>

Equivalence: Two One-Sided Tests (TOST)


<ul>
<li> p: Maximum of the non-inferiority and non-superiority p values.

</li></ul>

Thanks to Russell V. Lenth for the excellent <code>emmeans</code> package and documentation which inspired this feature.



### Order of operations

Behind the scenes, the arguments of <code>marginaleffects</code> functions are evaluated in this order:


<ol>
<li> <code>newdata</code>

</li>
<li> <code>variables</code>

</li>
<li> <code>comparison</code> and <code>slope</code>

</li>
<li> <code>by</code>

</li>
<li> <code>vcov</code>

</li>
<li> <code>hypothesis</code>

</li>
<li> <code>transform</code>

</li></ol>



### Parallel computation

The <code>slopes()</code> and <code>comparisons()</code> functions can use parallelism to
speed up computation. Operations are parallelized for the computation of
standard errors, at the model coefficient level. There is always
considerable overhead when using parallel computation, mainly involved
in passing the whole dataset to the different processes. Thus, parallel
computation is most likely to be useful when the model includes many parameters
and the dataset is relatively small.

Warning: In many cases, parallel processing will not be useful at all.

To activate parallel computation, users must load the <code>future.apply</code> package,
call <code>plan()</code> function, and set a global option.

<code>options(marginaleffects_parallel = TRUE)</code>: parallelize delta method computation of standard errors.
<code>options(marginaleffects_parallel_inferences = TRUE)</code>: parallelize <code>"rsample"</code> or <code>"fwb"</code> bootstrap computation in <code>inferences()</code>.
<code>options(marginaleffects_parallel_packages = TRUE)</code>: vector of strings with the names of modeling packages used to fit the model, ex: c(&quot;survival&quot;, &quot;splines&quot;)

For example:

<div class="sourceCode r"><pre>library(future.apply)
plan("multisession", workers = 4)
options(marginaleffects_parallel = FALSE)
options(marginaleffects_parallel_inferences = TRUE)
options(marginaleffects_parallel_packages = c("survival", "splines"))

slopes(model)
</pre></div>
To disable parallelism in <code>marginaleffects</code> altogether, you can set a global option:

<div class="sourceCode r"><pre>options(marginaleffects_parallel = FALSE)
</pre></div>


### Global options

The behavior of <code>marginaleffects</code> functions can be modified by setting global options.

Disable some safety checks and warnings:


<ul>
<li> <code>options(marginaleffects_startup_message = FALSE)</code>


<ul>
<li> Disable the startup message printed on <code>library(marginaleffects)</code>.

</li></ul>

</li>
<li> <code>options(marginaleffects_safe = FALSE)</code>


<ul>
<li> Disable safety checks and warnings.

</li></ul>

</li>
<li> <code>options(marginaleffects_print_omit = c("p.value", "s.value"))</code>


<ul>
<li> Omit some columns from the printed output.

</li></ul>

</li></ul>

Enforce lean return objects, sans information about the original model and
data, and other ancillary attributes. Note that this will disable some
advanced post-processing features and functions like hypotheses.

<div class="sourceCode r"><pre>options(marginaleffects_lean = TRUE)
</pre></div>
Other options:


<ul>
<li> <code>marginaleffects_plot_gray</code>: logical. If <code>TRUE</code>, the default color of the plot is gray. Default is <code>FALSE</code>.

</li></ul>



### Types

The <code>type</code> argument determines the scale of the predictions used to compute quantities of interest with functions from the <code>marginaleffects</code> package. Admissible values for <code>type</code> depend on the model object. When users specify an incorrect value for <code>type</code>, <code>marginaleffects</code> will raise an informative error with a list of valid <code>type</code> values for the specific model object. The first entry in the list in that error message is the default type.

The <code>invlink(link)</code> is a special type defined by <code>marginaleffects</code>. It is available for some (but not all) models, and only for the <code>predictions()</code> function. With this link type, we first compute predictions on the link scale, then we use the inverse link function to backtransform the predictions to the response scale. This is useful for models with non-linear link functions as it can ensure that confidence intervals stay within desirable bounds, ex: 0 to 1 for a logit model. Note that an average of estimates with <code>type="invlink(link)"</code> will not always be equivalent to the average of estimates with <code>type="response"</code>. This type is default when calling <code>predictions()</code>. It is available&mdash;but not default&mdash;when calling <code>avg_predictions()</code> or <code>predictions()</code> with the <code>by</code> argument.

Some of the most common <code>type</code> values are:

<table>
<tr>
 <td style="text-align: left;">
   class </td><td style="text-align: left;"> type </td>
</tr>
<tr>
 <td style="text-align: left;">
   Gam </td><td style="text-align: left;"> invlink(link), response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   Gls </td><td style="text-align: left;"> lp </td>
</tr>
<tr>
 <td style="text-align: left;">
   MCMCglmm </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   bam </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   bart </td><td style="text-align: left;"> ev, ppd </td>
</tr>
<tr>
 <td style="text-align: left;">
   betareg </td><td style="text-align: left;"> response, link, precision, quantile, variance </td>
</tr>
<tr>
 <td style="text-align: left;">
   bife </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   bracl </td><td style="text-align: left;"> probs </td>
</tr>
<tr>
 <td style="text-align: left;">
   brglmFit </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   brmsfit </td><td style="text-align: left;"> response, link, prediction, average </td>
</tr>
<tr>
 <td style="text-align: left;">
   brmultinom </td><td style="text-align: left;"> probs, class </td>
</tr>
<tr>
 <td style="text-align: left;">
   clm </td><td style="text-align: left;"> prob, cum.prob, linear.predictor </td>
</tr>
<tr>
 <td style="text-align: left;">
   clogit </td><td style="text-align: left;"> expected, lp, risk, survival </td>
</tr>
<tr>
 <td style="text-align: left;">
   coxph </td><td style="text-align: left;"> survival, expected, lp, risk </td>
</tr>
<tr>
 <td style="text-align: left;">
   coxph_weightit </td><td style="text-align: left;"> survival, expected, lp, risk </td>
</tr>
<tr>
 <td style="text-align: left;">
   crch </td><td style="text-align: left;"> response, location, scale, density </td>
</tr>
<tr>
 <td style="text-align: left;">
   fixest </td><td style="text-align: left;"> invlink(link), response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   flexsurvreg </td><td style="text-align: left;"> survival, response, mean, link, lp, linear, rmst, hazard, cumhaz </td>
</tr>
<tr>
 <td style="text-align: left;">
   gam </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   geeglm </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glimML </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glm </td><td style="text-align: left;"> invlink(link), response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glm_weightit </td><td style="text-align: left;"> invlink(link), probs, response, lp, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glmerMod </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glmgee </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   glmmPQL </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glmmTMB </td><td style="text-align: left;"> response, link, conditional, zprob, zlink, disp </td>
</tr>
<tr>
 <td style="text-align: left;">
   glmrob </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glmx </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   hetprob </td><td style="text-align: left;"> pr, xb </td>
</tr>
<tr>
 <td style="text-align: left;">
   hurdle </td><td style="text-align: left;"> response, prob, count, zero </td>
</tr>
<tr>
 <td style="text-align: left;">
   hxlr </td><td style="text-align: left;"> location, cumprob, scale, density </td>
</tr>
<tr>
 <td style="text-align: left;">
   iv_robust </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   ivpml </td><td style="text-align: left;"> pr, xb </td>
</tr>
<tr>
 <td style="text-align: left;">
   ivreg </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   lda </td><td style="text-align: left;"> class, posterior </td>
</tr>
<tr>
 <td style="text-align: left;">
   lm </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   lm_robust </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   lmerMod </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   lmerModLmerTest </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   lmrob </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   lrm </td><td style="text-align: left;"> fitted, lp, mean </td>
</tr>
<tr>
 <td style="text-align: left;">
   mblogit </td><td style="text-align: left;"> response, latent, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   mclogit </td><td style="text-align: left;"> response, latent, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   mhurdle </td><td style="text-align: left;"> E, Ep, p </td>
</tr>
<tr>
 <td style="text-align: left;">
   model_fit </td><td style="text-align: left;"> numeric, prob, class </td>
</tr>
<tr>
 <td style="text-align: left;">
   multinom </td><td style="text-align: left;"> probs, latent </td>
</tr>
<tr>
 <td style="text-align: left;">
   multinom_weightit </td><td style="text-align: left;"> probs, response, mean </td>
</tr>
<tr>
 <td style="text-align: left;">
   mvgam </td><td style="text-align: left;"> response, link, expected, detection, latent_N </td>
</tr>
<tr>
 <td style="text-align: left;">
   negbin </td><td style="text-align: left;"> invlink(link), response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   ols </td><td style="text-align: left;"> lp </td>
</tr>
<tr>
 <td style="text-align: left;">
   oohbchoice </td><td style="text-align: left;"> probability, utility </td>
</tr>
<tr>
 <td style="text-align: left;">
   ordinal_weightit </td><td style="text-align: left;"> probs, response, link, lp, mean </td>
</tr>
<tr>
 <td style="text-align: left;">
   orm </td><td style="text-align: left;"> fitted, mean, lp </td>
</tr>
<tr>
 <td style="text-align: left;">
   polr </td><td style="text-align: left;"> probs </td>
</tr>
<tr>
 <td style="text-align: left;">
   rendo.base </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   rlm </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   selection </td><td style="text-align: left;"> response, link, unconditional, conditional </td>
</tr>
<tr>
 <td style="text-align: left;">
   speedglm </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   speedlm </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   stanreg </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   survreg </td><td style="text-align: left;"> response, link, quantile </td>
</tr>
<tr>
 <td style="text-align: left;">
   svyglm </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   svyolr </td><td style="text-align: left;"> probs </td>
</tr>
<tr>
 <td style="text-align: left;">
   tobit </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   tobit1 </td><td style="text-align: left;"> expvalue, linpred, prob </td>
</tr>
<tr>
 <td style="text-align: left;">
   workflow </td><td style="text-align: left;"> numeric, prob, class </td>
</tr>
<tr>
 <td style="text-align: left;">
   zeroinfl </td><td style="text-align: left;"> response, prob, count, zero </td>
</tr>
<tr>
 <td style="text-align: left;">
</td>
</tr>

</table>



### References


<ul>
<li> Arel-Bundock V, Greifer N, Heiss A (2024). “How to Interpret Statistical Models Using marginaleffects for R and Python.” <em>Journal of Statistical Software</em>, <em>111</em>(9), 1-32. doi:10.18637/jss.v111.i09 [doi:10.18637/jss.v111.i09](https://doi.org/10.18637/jss.v111.i09)

</li>
<li> Arel-Bundock (2026). &quot;Model to Meaning: How to interpret statistical models in R and Python.&quot; CRC Press. https://routledge.com/9781032908724

</li>
<li> Greenland S. 2019. &quot;Valid P-Values Behave Exactly as They Should: Some Misleading Criticisms of P-Values and Their Resolution With S-Values.&quot; The American Statistician. 73(S1): 106–114.

</li>
<li> Cole, Stephen R, Jessie K Edwards, and Sander Greenland. 2020. &quot;Surprise!&quot; American Journal of Epidemiology 190 (2): 191–93. [doi:10.1093/aje/kwaa136](https://doi.org/10.1093/aje/kwaa136)

</li></ul>



### Examples
```{r, warning=FALSE, message=FALSE, eval=FALSE}
library("marginaleffects")


library("marginaleffects")
# Adjusted Prediction for every row of the original dataset
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
pred <- predictions(mod)
head(pred)

# Adjusted Predictions at User-Specified Values of the Regressors
predictions(mod, newdata = datagrid(hp = c(100, 120), cyl = 4))

m <- lm(mpg ~ hp + drat + factor(cyl) + factor(am), data = mtcars)
predictions(m, newdata = datagrid(FUN_factor = unique, FUN_numeric = median))

# Average Adjusted Predictions (AAP)
library(dplyr)
mod <- lm(mpg ~ hp * am * vs, mtcars)

avg_predictions(mod)

predictions(mod, by = "am")

# Conditional Adjusted Predictions
plot_predictions(mod, condition = "hp")

# Counterfactual predictions with the `variables` argument
# the `mtcars` dataset has 32 rows

mod <- lm(mpg ~ hp + am, data = mtcars)
p <- predictions(mod)
head(p)
nrow(p)

# average counterfactual predictions
avg_predictions(mod, variables = "am")

# counterfactual predictions obtained by replicating the entire for different
# values of the predictors
p <- predictions(mod, variables = list(hp = c(90, 110)))
nrow(p)


# hypothesis test: is the prediction in the 1st row equal to the prediction in the 2nd row
mod <- lm(mpg ~ wt + drat, data = mtcars)

predictions(
  mod,
  newdata = datagrid(wt = 2:3),
  hypothesis = "b1 = b2")

# same hypothesis test using row indices
predictions(
  mod,
  newdata = datagrid(wt = 2:3),
  hypothesis = "b1 - b2 = 0")

# same hypothesis test using numeric vector of weights
predictions(
  mod,
  newdata = datagrid(wt = 2:3),
  hypothesis = c(1, -1))

# two custom contrasts using a matrix of weights
lc <- matrix(
  c(
    1, -1,
    2, 3),
  ncol = 2)
predictions(
  mod,
  newdata = datagrid(wt = 2:3),
  hypothesis = lc)


# `by` argument
mod <- lm(mpg ~ hp * am * vs, data = mtcars)
predictions(mod, by = c("am", "vs"))

library(nnet)
nom <- multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)

# first 5 raw predictions
p <- predictions(nom, type = "probs")
head(p)

# average predictions
avg_predictions(nom, type = "probs", by = "group")

by <- data.frame(
  group = c("3", "4", "5"),
  by = c("3,4", "3,4", "5"))

predictions(nom, type = "probs", by = by)

# sum of predicted probabilities for combined response levels
mod <- multinom(factor(cyl) ~ mpg + am, data = mtcars, trace = FALSE)
by <- data.frame(
  by = c("4,6", "4,6", "8"),
  group = as.character(c(4, 6, 8)))
predictions(mod, newdata = "mean", byfun = sum, by = by)



```
