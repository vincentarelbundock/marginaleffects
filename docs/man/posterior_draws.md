
# posterior_draws

Extract Posterior Draws or Bootstrap Resamples from
<code>marginaleffects</code> Objects

## Description

Extract Posterior Draws or Bootstrap Resamples from
<code>marginaleffects</code> Objects

## Usage

<pre><code class='language-R'>posterior_draws(x, shape = "long")
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="posterior_draws_:_x">x</code>
</td>
<td>
An object produced by a <code>marginaleffects</code> package function,
such as <code>predictions()</code>, <code>avg_slopes()</code>,
<code>hypotheses()</code>, etc.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="posterior_draws_:_shape">shape</code>
</td>
<td>

string indicating the shape of the output format:

<ul>
<li>

"long": long format data frame

</li>
<li>

"DxP": Matrix with draws as rows and parameters as columns

</li>
<li>

"PxD": Matrix with draws as rows and parameters as columns

</li>
<li>

"rvar": Random variable datatype (see <code>posterior</code> package
documentation).

</li>
</ul>
</td>
</tr>
</table>

## Value

A data.frame with <code>drawid</code> and <code>draw</code> columns.
