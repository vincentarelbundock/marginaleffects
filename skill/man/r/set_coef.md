
## Internal function to set coefficients {.unnumbered}


### Description

Set the coefficients in a model to different values and return the modified object (internal function)



### Usage

<pre><code class='language-R'>set_coef(model, coefs, ...)

## Default S3 method:
set_coef(model, coefs, ...)

## S3 method for class 'tobit'
set_coef(model, coefs, ...)

## S3 method for class 'polr'
set_coef(model, coefs, ...)

## S3 method for class 'glmmPQL'
set_coef(model, coefs, ...)

## S3 method for class 'hetprob'
set_coef(model, coefs, ...)

## S3 method for class 'ivpml'
set_coef(model, coefs, ...)

## S3 method for class 'glm'
set_coef(model, coefs, ...)

## S3 method for class 'lm'
set_coef(model, coefs, ...)

## S3 method for class 'nls'
set_coef(model, coefs, ...)

## S3 method for class 'afex_aov'
set_coef(model, coefs, ...)

## S3 method for class 'glimML'
set_coef(model, coefs, ...)

## S3 method for class 'betareg'
set_coef(model, coefs, ...)

## S3 method for class 'multinom'
set_coef(model, coefs, ...)

## S3 method for class 'crch'
set_coef(model, coefs, ...)

## S3 method for class 'hxlr'
set_coef(model, coefs, ...)

## S3 method for class 'data.frame'
set_coef(model, coefs, ...)

## S3 method for class 'flexsurvreg'
set_coef(model, coefs, ...)

## S3 method for class 'gamlss'
set_coef(model, coefs, ...)

## S3 method for class 'glmmTMB'
set_coef(model, coefs, ...)

## S3 method for class 'glmgee'
set_coef(model, coefs, ...)

## S3 method for class 'glmx'
set_coef(model, coefs, ...)

## S3 method for class 'merMod'
set_coef(model, coefs, ...)

## S3 method for class 'lmerModLmerTest'
set_coef(model, coefs, ...)

## S3 method for class 'lmerMod'
set_coef(model, coefs, ...)

## S3 method for class 'mlm'
set_coef(model, coefs, ...)

## S3 method for class 'lme'
set_coef(model, coefs, ...)

## S3 method for class 'hurdle'
set_coef(model, coefs, ...)

## S3 method for class 'zeroinfl'
set_coef(model, coefs, ...)

## S3 method for class 'rlmerMod'
set_coef(model, coefs, ...)

## S3 method for class 'stpm2'
set_coef(model, coefs, ...)

## S3 method for class 'pstpm2'
set_coef(model, coefs, ...)

## S3 method for class 'gsm'
set_coef(model, coefs, ...)

## S3 method for class 'aft'
set_coef(model, coefs, ...)

## S3 method for class 'selection'
set_coef(model, coefs, ...)

## S3 method for class 'scam'
set_coef(model, coefs, ...)

## S3 method for class 'svyolr'
set_coef(model, coefs, ...)

## S3 method for class 'survreg'
set_coef(model, coefs, ...)

## S3 method for class 'systemfit'
set_coef(model, coefs, ...)

## S3 method for class 'model_fit'
set_coef(model, coefs, ...)

## S3 method for class 'workflow'
set_coef(model, coefs, ...)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="model">model</code></td>
<td>
object to modify
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="coefs">coefs</code></td>
<td>
vector of coefficients to insert in the model object
</td></tr>
</table>


### Details

To compute the variance of marginal effects we need to take the
Jacobian with



### Value

Model object of the same class as the <code>model</code> argument, but with
different stored coefficients.


