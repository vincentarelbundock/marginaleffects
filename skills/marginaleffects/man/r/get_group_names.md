
## Get levels of the outcome variable in grouped or multivariate models {.unnumbered}


### Description

Get levels of the outcome variable in grouped or multivariate models



### Usage

<pre><code class='language-R'>get_group_names(model, ...)

## Default S3 method:
get_group_names(model, ...)

## S3 method for class 'polr'
get_group_names(model, ...)

## S3 method for class 'multinom'
get_group_names(model, ...)

## S3 method for class 'bracl'
get_group_names(model, ...)

## S3 method for class 'brmsfit'
get_group_names(model, ...)

## S3 method for class 'mblogit'
get_group_names(model, type, ...)

## S3 method for class 'mlm'
get_group_names(model, ...)

## S3 method for class 'clm'
get_group_names(model, ...)

## S3 method for class 'hurdle'
get_group_names(model, type = "count", ...)

## S3 method for class 'svyolr'
get_group_names(model, ...)
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

A character vector


