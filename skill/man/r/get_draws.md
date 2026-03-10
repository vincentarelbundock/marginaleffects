
## Extract Posterior Draws or Bootstrap Resamples from <code>marginaleffects</code> Objects {.unnumbered}


### Description

Extract Posterior Draws or Bootstrap Resamples from <code>marginaleffects</code> Objects



### Usage

<pre><code class='language-R'>get_draws(x, shape = "long")
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="x">x</code></td>
<td>
An object produced by a <code>marginaleffects</code> package function, such as <code>predictions()</code>, <code>avg_slopes()</code>, <code>hypotheses()</code>, etc.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="shape">shape</code></td>
<td>
string indicating the shape of the output format:


<ul>
<li> &quot;long&quot; (default): long format data frame

</li>
<li> &quot;PxD&quot; (fastest): Matrix with parameters as rows and draws as columns

</li>
<li> &quot;DxP&quot;: Matrix with draws as rows and parameters as columns

</li>
<li> &quot;rvar&quot;: Random variable datatype (see <code>posterior</code> package documentation).

</li></ul>
</td></tr>
</table>


### Details

If DxP and PxD and the names returned by <code>coef(x)</code> are unique, <code>marginaleffects</code> sets parameter names to those names. Otherwise, it sets them to <code>b1</code>, <code>b2</code>, etc.



### Value

A data.frame with <code>drawid</code> and <code>draw</code> columns.


