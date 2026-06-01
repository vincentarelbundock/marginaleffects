## Pad newdata with factor levels and merge with original data {.unnumbered}


### Description

<code>model.matrix</code> breaks when <code>newdata</code> includes a factor
variable, but not all levels are present in the data. This is bad for us
because we often want to get predictions with one (or few) rows, where some
factor levels are inevitably missing.



### Usage

<pre><code class='language-R'>pad(model, newdata)
</code></pre>


### Arguments

<table>
<tr><td style = "white-space: collapse; font-family: monospace; vertical-align: top"><code id="model">model</code></td>
<td>
Model object to check for mlogit class
</td></tr>
<tr><td style = "white-space: collapse; font-family: monospace; vertical-align: top"><code id="newdata">newdata</code></td>
<td>
Data frame to pad
</td></tr>
</table>

