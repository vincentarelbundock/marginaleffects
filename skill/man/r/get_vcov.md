
## Get a named variance-covariance matrix from a model object {.unnumbered}


### Description

Mostly for internal use, but can be useful because the output is consistent across model classes.



### Usage

<pre><code class='language-R'>get_vcov(model, ...)

## Default S3 method:
get_vcov(model, vcov = NULL, ...)

## S3 method for class 'lda'
get_vcov(model, ...)

## S3 method for class 'MCMCglmm'
get_vcov(model, vcov = NULL, ...)

## S3 method for class 'afex_aov'
get_vcov(model, vcov = NULL, ...)

## S3 method for class 'glimML'
get_vcov(model, vcov = NULL, ...)

## S3 method for class 'biglm'
get_vcov(model, vcov = NULL, ...)

## S3 method for class 'brmsfit'
get_vcov(model, vcov = NULL, ...)

## S3 method for class 'bart'
get_vcov(model, vcov = NULL, ...)

## S3 method for class 'gamlss'
get_vcov(model, ...)

## S3 method for class 'glmmTMB'
get_vcov(model, vcov, ...)

## S3 method for class 'mhurdle'
get_vcov(model, vcov = NULL, ...)

## S3 method for class 'Learner'
get_vcov(model, ...)

## S3 method for class 'orm'
get_vcov(model, vcov = NULL, ...)

## S3 method for class 'stpm2'
get_vcov(model, ...)

## S3 method for class 'pstpm2'
get_vcov(model, ...)

## S3 method for class 'gsm'
get_vcov(model, ...)

## S3 method for class 'aft'
get_vcov(model, ...)

## S3 method for class 'scam'
get_vcov(model, vcov = NULL, ...)

## S3 method for class 'systemfit'
get_vcov(model, ...)

## S3 method for class 'systemfit'
get_predict(model, newdata = NULL, type = NULL, ...)

## S3 method for class 'model_fit'
get_vcov(model, vcov, type = NULL, ...)

## S3 method for class 'workflow'
get_vcov(model, vcov, type = NULL, ...)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="model">model</code></td>
<td>
Model object
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
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="newdata">newdata</code></td>
<td>
Grid of predictor values at which we evaluate the slopes.


<ul>
<li> Warning: Please avoid modifying your dataset between fitting the model and calling a <code>marginaleffects</code> function. This can sometimes lead to unexpected results.

</li>
<li> <code>NULL</code> (default): Unit-level slopes for each observed value in the dataset (empirical distribution). The dataset is retrieved using <code>insight::get_data()</code>, which tries to extract data from the environment. This may produce unexpected results if the original data frame has been altered since fitting the model.

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

</li>
<li> string:


<ul>
<li> &quot;mean&quot;: Slopes evaluated when each predictor is held at its mean or mode.

</li>
<li> &quot;median&quot;: Slopes evaluated when each predictor is held at its median or mode.

</li>
<li> &quot;balanced&quot;: Slopes evaluated on a balanced grid with every combination of categories and numeric variables held at their means.

</li>
<li> &quot;tukey&quot;: Slopes evaluated at Tukey's 5 numbers.

</li>
<li> &quot;grid&quot;: Slopes evaluated on a grid of representative numbers (Tukey's 5 numbers and unique values of categorical predictors).

</li></ul>

</li></ul>
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
</table>


### Value

A named square matrix of variance and covariances. The names must match the coefficient names.


