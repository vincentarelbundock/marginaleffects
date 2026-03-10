
## Refit a marginaleffects object with new data {.unnumbered}


### Description

Refit a marginaleffects object with new data



### Usage

<pre><code class='language-R'>refit(object, ...)

## S3 method for class 'marginaleffects'
refit(object, data = NULL, newdata = NULL, vcov = NULL, ...)

## S3 method for class 'predictions'
refit(object, data = NULL, newdata = NULL, vcov = NULL, ...)

## S3 method for class 'comparisons'
refit(object, data = NULL, newdata = NULL, vcov = NULL, ...)

## S3 method for class 'slopes'
refit(object, data = NULL, newdata = NULL, vcov = NULL, ...)

## S3 method for class 'hypotheses'
refit(object, data = NULL, newdata = NULL, vcov = NULL, ...)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="object">object</code></td>
<td>
A marginaleffects object (predictions, comparisons, or slopes)
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="...">...</code></td>
<td>
Additional arguments passed to methods
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="data">data</code></td>
<td>
Optional data frame to refit the underlying model
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="newdata">newdata</code></td>
<td>
Optional data frame to re-evaluate the marginaleffects call
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="vcov">vcov</code></td>
<td>
Optional logical or variance-covariance matrix specification to pass to the marginaleffects call
</td></tr>
</table>


### Details

If <code>data</code> is supplied, the underlying model is refitted using that data.
If <code>newdata</code> is supplied, the marginaleffects call is re-evaluated with the new data.
Both can be supplied together to refit the model and make predictions on new data.



### Value

A marginaleffects object


