
## Get predicted values from a model object (internal function) {.unnumbered}


### Description

Get predicted values from a model object (internal function)



### Usage

<pre><code class='language-R'>get_predict(
  model,
  newdata,
  type,
  mfx = NULL,
  newparams = NULL,
  ndraws = NULL,
  se.fit = NULL,
  ...
)

## Default S3 method:
get_predict(
  model,
  newdata = insight::get_data(model),
  type = "response",
  mfx = NULL,
  newparams = NULL,
  ndraws = NULL,
  se.fit = NULL,
  ...
)

## S3 method for class 'polr'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = "probs",
  mfx = NULL,
  ...
)

## S3 method for class 'glmmPQL'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = "response",
  mfx = NULL,
  ...
)

## S3 method for class 'lda'
get_predict(model, newdata = insight::get_data(model), type = "class", ...)

## S3 method for class 'MCMCglmm'
get_predict(
  model,
  newdata,
  type = "response",
  mfx = NULL,
  newparams = NULL,
  ndraws = 1000,
  se.fit = NULL,
  ...
)

## S3 method for class 'lm'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)

## S3 method for class 'glm'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)

## S3 method for class 'afex_aov'
get_predict(model, newdata = NULL, ...)

## S3 method for class 'glimML'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)

## S3 method for class 'betareg'
get_predict(model, newdata, type = "response", ...)

## S3 method for class 'bife'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)

## S3 method for class 'biglm'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = "response",
  mfx = NULL,
  ...
)

## S3 method for class 'multinom'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = "probs",
  mfx = NULL,
  ...
)

## S3 method for class 'brmultinom'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = "probs",
  mfx = NULL,
  ...
)

## S3 method for class 'brmsfit'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)

## S3 method for class 'crch'
get_predict(model, newdata = NULL, type = "location", ...)

## S3 method for class 'bart'
get_predict(model, newdata = NULL, ...)

## S3 method for class 'fixest'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = "response",
  mfx = NULL,
  ...
)

## S3 method for class 'flexsurvreg'
get_predict(model, newdata, type, ...)

## S3 method for class 'gamlss'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)

## S3 method for class 'glmmTMB'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = "response",
  mfx = NULL,
  newparams = NULL,
  ...
)

## S3 method for class 'glmgee'
get_predict(model, newdata, ...)

## S3 method for class 'merMod'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)

## S3 method for class 'lmerModLmerTest'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)

## S3 method for class 'lmerMod'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)

## S3 method for class 'mblogit'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)

## S3 method for class 'mhurdle'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)

## S3 method for class 'mlogit'
get_predict(model, newdata, ...)

## S3 method for class 'Learner'
get_predict(model, newdata, type = NULL, ...)

## S3 method for class 'clm'
get_predict(model, newdata = insight::get_data(model), type = "prob", ...)

## S3 method for class 'rq'
get_predict(model, newdata = insight::get_data(model), type = NULL, ...)

## S3 method for class 'rms'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = NULL,
  mfx = NULL,
  ...
)

## S3 method for class 'orm'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = NULL,
  mfx = NULL,
  ...
)

## S3 method for class 'lrm'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = NULL,
  mfx = NULL,
  ...
)

## S3 method for class 'ols'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = NULL,
  mfx = NULL,
  ...
)

## S3 method for class 'rlmerMod'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = "response",
  mfx = NULL,
  ...
)

## S3 method for class 'stanreg'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)

## S3 method for class 'stpm2'
get_predict(model, newdata = NULL, ...)

## S3 method for class 'pstpm2'
get_predict(model, newdata = NULL, ...)

## S3 method for class 'gsm'
get_predict(model, newdata = NULL, ...)

## S3 method for class 'aft'
get_predict(model, newdata = NULL, ...)

## S3 method for class 'svyolr'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = "probs",
  mfx = NULL,
  ...
)

## S3 method for class 'svyglm'
get_predict(
  model,
  newdata = insight::get_data(model),
  type = "response",
  mfx = NULL,
  newparams = NULL,
  ndraws = NULL,
  se.fit = FALSE,
  ...
)

## S3 method for class 'coxph'
get_predict(model, newdata = insight::get_data(model), type = "lp", ...)

## S3 method for class 'model_fit'
get_predict(model, newdata, type = NULL, ...)

## S3 method for class 'workflow'
get_predict(model, newdata, type = NULL, ...)

## S3 method for class 'tobit1'
get_predict(model, newdata = insight::get_data(model), type = "response", ...)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="model">model</code></td>
<td>
Model object
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

A data.frame of predicted values with a number of rows equal to the
number of rows in <code>newdata</code> and columns &quot;rowid&quot; and &quot;estimate&quot;. A &quot;group&quot;
column is added for multivariate models or models with categorical outcomes.


