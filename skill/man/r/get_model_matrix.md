
## Get a named model matrix {.unnumbered}


### Description

Get a named model matrix



### Usage

<pre><code class='language-R'>get_model_matrix(model, newdata, mfx = NULL)

## Default S3 method:
get_model_matrix(model, newdata, mfx = NULL)
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
</table>

