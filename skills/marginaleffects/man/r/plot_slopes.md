## Plot Conditional or Marginal Slopes {.unnumbered}


### Description

Plot slopes on the y-axis against values of one or more predictors (x-axis, colors/shapes, and facets).

The <code>by</code> argument is used to plot marginal slopes, that is, slopes made on the original data, but averaged by subgroups. This is analogous to using the <code>by</code> argument in the <code>slopes()</code> function.

The <code>condition</code> argument is used to plot conditional slopes, that is, slopes computed on a user-specified grid. This is analogous to using the <code>newdata</code> argument and <code>datagrid()</code> function in a <code>slopes()</code> call. All variables whose values are not specified explicitly are treated as usual by <code>datagrid()</code>, that is, they are held at their mean or mode (or rounded mean for integers). This includes grouping variables in mixed-effects models, so analysts who fit such models may want to specify the groups of interest using the <code>condition</code> argument, or supply model-specific arguments to compute population-level estimates. See details below.
See the &quot;Plots&quot; vignette and website for tutorials and information on how to customize plots:


<ul>
<li> https://marginaleffects.com/bonus/plot.html

</li>
<li> https://marginaleffects.com

</li></ul>



### Usage

<pre><code class='language-R'>plot_slopes(
  model,
  variables = NULL,
  condition = NULL,
  by = NULL,
  newdata = NULL,
  type = NULL,
  vcov = NULL,
  conf_level = 0.95,
  wts = FALSE,
  slope = "dydx",
  rug = FALSE,
  gray = getOption("marginaleffects_plot_gray", default = FALSE),
  draw = TRUE,
  ...
)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="model">model</code></td>
<td>
Model object
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="variables">variables</code></td>
<td>
Name of the variable whose marginal effect (slope) we want to plot on the y-axis.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="condition">condition</code></td>
<td>
Conditional slopes


<ul>
<li> Character vector (max length 4): Names of the predictors to display.

</li>
<li> Named list (max length 4): List names correspond to predictors. List elements can be:


<ul>
<li> Numeric vector

</li>
<li> Function which returns a numeric vector or a set of unique categorical values

</li>
<li> Shortcut strings for common reference values: &quot;minmax&quot;, &quot;quartile&quot;, &quot;threenum&quot;

</li></ul>

</li>
<li> 1: x-axis. 2: color/shape. 3: facet (wrap if no fourth variable, otherwise cols of grid). 4: facet (rows of grid).

</li>
<li> Numeric variables in positions 2 and 3 are summarized by Tukey's five numbers <code>?stats::fivenum</code>.

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
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="newdata">newdata</code></td>
<td>
When <code>newdata</code> is <code>NULL</code>, the grid is determined by the <code>condition</code> argument. When <code>newdata</code> is not <code>NULL</code>, the argument behaves in the same way as in the <code>predictions()</code> function. Note that the <code>condition</code> argument builds its own grid, so the <code>newdata</code> argument is ignored if the <code>condition</code> argument is supplied.
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
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="conf_level">conf_level</code></td>
<td>
numeric value between 0 and 1. Confidence level to use to build a confidence interval.
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
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="slope">slope</code></td>
<td>
string indicates the type of slope or (semi-)elasticity to compute:


<ul>
<li> &quot;dydx&quot;: dY/dX

</li>
<li> &quot;eyex&quot;: dY/dX * Y / X

</li>
<li> &quot;eydx&quot;: dY/dX * Y

</li>
<li> &quot;dyex&quot;: dY/dX / X

</li>
<li> Y is the predicted value of the outcome; X is the observed value of the predictor.

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="rug">rug</code></td>
<td>
TRUE displays tick marks on the axes to mark the distribution of raw data.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="gray">gray</code></td>
<td>
FALSE grayscale or color plot
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="draw">draw</code></td>
<td>
<code>TRUE</code> returns a <code>ggplot2</code> plot. <code>FALSE</code> returns a <code>data.frame</code> of the underlying data.
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

A <code>ggplot2</code> object



### Model-Specific Arguments

Some model types allow model-specific arguments to modify the nature of
marginal effects, predictions, marginal means, and contrasts. Please report
other package-specific <code>predict()</code> arguments on Github so we can add them to
the table below.

https://github.com/vincentarelbundock/marginaleffects/issues

<table>
<tr>
 <td style="text-align: left;">
   Package </td><td style="text-align: left;"> Class </td><td style="text-align: left;"> Argument </td><td style="text-align: left;"> Documentation </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>brms</code> </td><td style="text-align: left;"> <code>brmsfit</code> </td><td style="text-align: left;"> <code>ndraws</code> </td><td style="text-align: left;"> brms::posterior_predict </td>
</tr>
<tr>
 <td style="text-align: left;">
    </td><td style="text-align: left;">  </td><td style="text-align: left;"> <code>re_formula</code> </td><td style="text-align: left;"> brms::posterior_predict </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>lme4</code> </td><td style="text-align: left;"> <code>merMod</code> </td><td style="text-align: left;"> <code>re.form</code> </td><td style="text-align: left;"> lme4::predict.merMod </td>
</tr>
<tr>
 <td style="text-align: left;">
    </td><td style="text-align: left;">  </td><td style="text-align: left;"> <code>allow.new.levels</code> </td><td style="text-align: left;"> lme4::predict.merMod </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>glmmTMB</code> </td><td style="text-align: left;"> <code>glmmTMB</code> </td><td style="text-align: left;"> <code>re.form</code> </td><td style="text-align: left;"> glmmTMB::predict.glmmTMB </td>
</tr>
<tr>
 <td style="text-align: left;">
    </td><td style="text-align: left;">  </td><td style="text-align: left;"> <code>allow.new.levels</code> </td><td style="text-align: left;"> glmmTMB::predict.glmmTMB </td>
</tr>
<tr>
 <td style="text-align: left;">
    </td><td style="text-align: left;">  </td><td style="text-align: left;"> <code>zitype</code> </td><td style="text-align: left;"> glmmTMB::predict.glmmTMB </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>mgcv</code> </td><td style="text-align: left;"> <code>bam</code> </td><td style="text-align: left;"> <code>exclude</code> </td><td style="text-align: left;"> mgcv::predict.bam </td>
</tr>
<tr>
 <td style="text-align: left;">
    </td><td style="text-align: left;"> <code>gam</code> </td><td style="text-align: left;"> <code>exclude</code> </td><td style="text-align: left;"> mgcv::predict.gam </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>robustlmm</code> </td><td style="text-align: left;"> <code>rlmerMod</code> </td><td style="text-align: left;"> <code>re.form</code> </td><td style="text-align: left;"> robustlmm::predict.rlmerMod </td>
</tr>
<tr>
 <td style="text-align: left;">
    </td><td style="text-align: left;">  </td><td style="text-align: left;"> <code>allow.new.levels</code> </td><td style="text-align: left;"> robustlmm::predict.rlmerMod </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>MCMCglmm</code> </td><td style="text-align: left;"> <code>MCMCglmm</code> </td><td style="text-align: left;"> <code>ndraws</code> </td><td style="text-align: left;">  </td>
</tr>
<tr>
 <td style="text-align: left;">
   <code>sampleSelection</code> </td><td style="text-align: left;"> <code>selection</code> </td><td style="text-align: left;"> <code>part</code> </td><td style="text-align: left;"> sampleSelection::predict.selection </td>
</tr>
<tr>
 <td style="text-align: left;">
</td>
</tr>

</table>



### Examples
```{r, warning=FALSE, message=FALSE, eval=FALSE}
library("marginaleffects")


mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)

plot_slopes(mod, variables = "hp", condition = "drat")

plot_slopes(mod, variables = "hp", condition = c("drat", "am"))

plot_slopes(mod, variables = "hp", condition = list("am", "drat" = 3:5))

plot_slopes(mod, variables = "am", condition = list("hp", "drat" = range))

plot_slopes(mod, variables = "am", condition = list("hp", "drat" = "threenum"))

# marginal slopes
plot_slopes(mod, variables = "hp", by = "am")

# marginal slopes on a counterfactual grid
plot_slopes(mod,
    variables = "hp",
    by = "am",
    newdata = datagrid(am = 0:1, grid_type = "counterfactual")
)



```
