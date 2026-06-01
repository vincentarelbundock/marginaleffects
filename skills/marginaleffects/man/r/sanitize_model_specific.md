
## Method to raise model-specific warnings and errors {.unnumbered}


### Description

Method to raise model-specific warnings and errors



### Usage

<pre><code class='language-R'>## S3 method for class 'glimML'
sanitize_model_specific(model, ...)

## S3 method for class 'betareg'
sanitize_model_specific(model, ...)

## S3 method for class 'biglm'
sanitize_model_specific(model, vcov = NULL, ...)

sanitize_model_specific(model, ...)

## Default S3 method:
sanitize_model_specific(
  model,
  vcov = NULL,
  calling_function = "marginaleffects",
  ...
)

## S3 method for class 'brmsfit'
sanitize_model_specific(model, ...)

## S3 method for class 'bart'
sanitize_model_specific(model, calling_function, ...)

## S3 method for class 'fixest'
sanitize_model_specific(
  model,
  vcov = TRUE,
  calling_function = "predictions",
  ...
)

## S3 method for class 'gamlss'
sanitize_model_specific(model, calling_function, ...)

## S3 method for class 'glmmTMB'
sanitize_model_specific(model, vcov = TRUE, re.form, ...)

## S3 method for class 'merMod'
sanitize_model_specific(model, re.form, vcov = TRUE, ...)

## S3 method for class 'mblogit'
sanitize_model_specific(model, calling_function = "marginaleffects", ...)

## S3 method for class 'mlogit'
sanitize_model_specific(model, calling_function = NULL, ...)

## S3 method for class 'Learner'
sanitize_model_specific(model, calling_function, ...)

## S3 method for class 'clm'
sanitize_model_specific(model, ...)

## S3 method for class 'plm'
sanitize_model_specific(model, ...)

## S3 method for class 'plm'
sanitize_model_specific(model, ...)

## S3 method for class 'rqs'
sanitize_model_specific(model, ...)

## S3 method for class 'rms'
sanitize_model_specific(model, ...)

## S3 method for class 'orm'
sanitize_model_specific(model, ...)

## S3 method for class 'lrm'
sanitize_model_specific(model, ...)

## S3 method for class 'ols'
sanitize_model_specific(model, ...)

## S3 method for class 'svyolr'
sanitize_model_specific(model, wts = FALSE, by = FALSE, ...)

## S3 method for class 'svyglm'
sanitize_model_specific(model, wts = FALSE, by = FALSE, ...)

## S3 method for class 'coxph'
sanitize_model_specific(model, vcov, ...)
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
</table>


### Value

A warning, an error, or nothing


