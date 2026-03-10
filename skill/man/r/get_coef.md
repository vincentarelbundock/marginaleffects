
## Get a named vector of coefficients from a model object {.unnumbered}


### Description

Mostly for internal use, but can be useful because the output is consistent across model classes.



### Usage

<pre><code class='language-R'>get_coef(model, ...)

## Default S3 method:
get_coef(model, ...)

## S3 method for class 'polr'
get_coef(model, ...)

## S3 method for class 'multinom_weightit'
get_coef(model, ...)

## S3 method for class 'nls'
get_coef(model, ...)

## S3 method for class 'afex_aov'
get_coef(model, ...)

## S3 method for class 'betareg'
get_coef(model, ...)

## S3 method for class 'multinom'
get_coef(model, ...)

## S3 method for class 'brmultinom'
get_coef(model, ...)

## S3 method for class 'bracl'
get_coef(model, ...)

## S3 method for class 'brmsfit'
get_coef(model, ...)

## S3 method for class 'data.frame'
get_coef(model, ...)

## S3 method for class 'gamlss'
get_coef(model, ...)

## S3 method for class 'glmmTMB'
get_coef(model, ...)

## S3 method for class 'glmgee'
get_coef(model, ...)

## S3 method for class 'merMod'
get_coef(model, ...)

## S3 method for class 'lmerModLmerTest'
get_coef(model, ...)

## S3 method for class 'lmerMod'
get_coef(model, ...)

## S3 method for class 'mblogit'
get_coef(model, ...)

## S3 method for class 'gam'
get_coef(model, ...)

## S3 method for class 'mlm'
get_coef(model, ...)

## S3 method for class 'selection'
get_coef(model, ...)

## S3 method for class 'scam'
get_coef(model, ...)

## S3 method for class 'svyolr'
get_coef(model, ...)

## S3 method for class 'systemfit'
get_coef(model, ...)

## S3 method for class 'workflow'
get_coef(model, ...)
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
</table>


### Value

A named vector of coefficients. The names must match those of the variance matrix.


