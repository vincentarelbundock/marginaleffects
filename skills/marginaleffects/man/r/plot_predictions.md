## Plot Conditional or Marginal Predictions {.unnumbered}


### Description

Plot predictions on the y-axis against values of one or more predictors (x-axis, colors/shapes, and facets).

The <code>by</code> argument is used to plot marginal predictions, that is, predictions made on the original data, but averaged by subgroups. This is analogous to using the <code>by</code> argument in the <code>predictions()</code> function.

The <code>condition</code> argument is used to plot conditional predictions, that is, predictions made on a user-specified grid. This is analogous to using the <code>newdata</code> argument and <code>datagrid()</code> function in a <code>predictions()</code> call. All variables whose values are not specified explicitly are treated as usual by <code>datagrid()</code>, that is, they are held at their mean or mode (or rounded mean for integers). This includes grouping variables in mixed-effects models, so analysts who fit such models may want to specify the groups of interest using the <code>condition</code> argument, or supply model-specific arguments to compute population-level estimates. See details below.

See the &quot;Plots&quot; vignette and website for tutorials and information on how to customize plots:


<ul>
<li> https://marginaleffects.com/bonus/plot.html

</li>
<li> https://marginaleffects.com

</li></ul>



### Usage

<pre><code class='language-R'>plot_predictions(
  model,
  condition = NULL,
  by = NULL,
  newdata = NULL,
  type = NULL,
  vcov = NULL,
  conf_level = 0.95,
  wts = FALSE,
  transform = NULL,
  points = 0,
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
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="condition">condition</code></td>
<td>
Conditional predictions.


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
<li> Numeric variables in positions 2 and 3 are summarized by Tukey's five numbers <code>?stats::fivenum</code>

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="by">by</code></td>
<td>
Marginal predictions


<ul>
<li> Character vector (max length 3): Names of the categorical predictors to marginalize across.

</li>
<li> 1: x-axis. 2: color. 3: facets.

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
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="transform">transform</code></td>
<td>
A function applied to unit-level adjusted predictions and confidence intervals just before the function returns results. For bayesian models, this function is applied to individual draws from the posterior distribution, before computing summaries.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="points">points</code></td>
<td>
Number between 0 and 1 which controls the transparency of raw data points. 0 (default) does not display any points. Warning: The points displayed are raw data, so the resulting plot is not a &quot;partial residual plot.&quot;
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

A <code>ggplot2</code> object or data frame (if <code>draw=FALSE</code>)



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



### Types

The <code>type</code> argument determines the scale of the predictions used to compute quantities of interest with functions from the <code>marginaleffects</code> package. Admissible values for <code>type</code> depend on the model object. When users specify an incorrect value for <code>type</code>, <code>marginaleffects</code> will raise an informative error with a list of valid <code>type</code> values for the specific model object. The first entry in the list in that error message is the default type.

The <code>invlink(link)</code> is a special type defined by <code>marginaleffects</code>. It is available for some (but not all) models, and only for the <code>predictions()</code> function. With this link type, we first compute predictions on the link scale, then we use the inverse link function to backtransform the predictions to the response scale. This is useful for models with non-linear link functions as it can ensure that confidence intervals stay within desirable bounds, ex: 0 to 1 for a logit model. Note that an average of estimates with <code>type="invlink(link)"</code> will not always be equivalent to the average of estimates with <code>type="response"</code>. This type is default when calling <code>predictions()</code>. It is available&mdash;but not default&mdash;when calling <code>avg_predictions()</code> or <code>predictions()</code> with the <code>by</code> argument.

Some of the most common <code>type</code> values are:

<table>
<tr>
 <td style="text-align: left;">
   class </td><td style="text-align: left;"> type </td>
</tr>
<tr>
 <td style="text-align: left;">
   Gam </td><td style="text-align: left;"> invlink(link), response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   Gls </td><td style="text-align: left;"> lp </td>
</tr>
<tr>
 <td style="text-align: left;">
   MCMCglmm </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   bam </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   bart </td><td style="text-align: left;"> ev, ppd </td>
</tr>
<tr>
 <td style="text-align: left;">
   betareg </td><td style="text-align: left;"> response, link, precision, quantile, variance </td>
</tr>
<tr>
 <td style="text-align: left;">
   bife </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   bracl </td><td style="text-align: left;"> probs </td>
</tr>
<tr>
 <td style="text-align: left;">
   brglmFit </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   brmsfit </td><td style="text-align: left;"> response, link, prediction, average </td>
</tr>
<tr>
 <td style="text-align: left;">
   brmultinom </td><td style="text-align: left;"> probs, class </td>
</tr>
<tr>
 <td style="text-align: left;">
   clm </td><td style="text-align: left;"> prob, cum.prob, linear.predictor </td>
</tr>
<tr>
 <td style="text-align: left;">
   clogit </td><td style="text-align: left;"> expected, lp, risk, survival </td>
</tr>
<tr>
 <td style="text-align: left;">
   coxph </td><td style="text-align: left;"> survival, expected, lp, risk </td>
</tr>
<tr>
 <td style="text-align: left;">
   coxph_weightit </td><td style="text-align: left;"> survival, expected, lp, risk </td>
</tr>
<tr>
 <td style="text-align: left;">
   crch </td><td style="text-align: left;"> response, location, scale, density </td>
</tr>
<tr>
 <td style="text-align: left;">
   fixest </td><td style="text-align: left;"> invlink(link), response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   flexsurvreg </td><td style="text-align: left;"> survival, response, mean, link, lp, linear, rmst, hazard, cumhaz </td>
</tr>
<tr>
 <td style="text-align: left;">
   gam </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   geeglm </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glimML </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glm </td><td style="text-align: left;"> invlink(link), response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glm_weightit </td><td style="text-align: left;"> invlink(link), probs, response, lp, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glmerMod </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glmgee </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   glmmPQL </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glmmTMB </td><td style="text-align: left;"> response, link, conditional, zprob, zlink, disp </td>
</tr>
<tr>
 <td style="text-align: left;">
   glmrob </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   glmx </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   hetprob </td><td style="text-align: left;"> pr, xb </td>
</tr>
<tr>
 <td style="text-align: left;">
   hurdle </td><td style="text-align: left;"> response, prob, count, zero </td>
</tr>
<tr>
 <td style="text-align: left;">
   hxlr </td><td style="text-align: left;"> location, cumprob, scale, density </td>
</tr>
<tr>
 <td style="text-align: left;">
   iv_robust </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   ivpml </td><td style="text-align: left;"> pr, xb </td>
</tr>
<tr>
 <td style="text-align: left;">
   ivreg </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   lda </td><td style="text-align: left;"> class, posterior </td>
</tr>
<tr>
 <td style="text-align: left;">
   lm </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   lm_robust </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   lmerMod </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   lmerModLmerTest </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   lmrob </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   lrm </td><td style="text-align: left;"> fitted, lp, mean </td>
</tr>
<tr>
 <td style="text-align: left;">
   mblogit </td><td style="text-align: left;"> response, latent, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   mclogit </td><td style="text-align: left;"> response, latent, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   mhurdle </td><td style="text-align: left;"> E, Ep, p </td>
</tr>
<tr>
 <td style="text-align: left;">
   model_fit </td><td style="text-align: left;"> numeric, prob, class </td>
</tr>
<tr>
 <td style="text-align: left;">
   multinom </td><td style="text-align: left;"> probs, latent </td>
</tr>
<tr>
 <td style="text-align: left;">
   multinom_weightit </td><td style="text-align: left;"> probs, response, mean </td>
</tr>
<tr>
 <td style="text-align: left;">
   mvgam </td><td style="text-align: left;"> response, link, expected, detection, latent_N </td>
</tr>
<tr>
 <td style="text-align: left;">
   negbin </td><td style="text-align: left;"> invlink(link), response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   ols </td><td style="text-align: left;"> lp </td>
</tr>
<tr>
 <td style="text-align: left;">
   oohbchoice </td><td style="text-align: left;"> probability, utility </td>
</tr>
<tr>
 <td style="text-align: left;">
   ordinal_weightit </td><td style="text-align: left;"> probs, response, link, lp, mean </td>
</tr>
<tr>
 <td style="text-align: left;">
   orm </td><td style="text-align: left;"> fitted, mean, lp </td>
</tr>
<tr>
 <td style="text-align: left;">
   polr </td><td style="text-align: left;"> probs </td>
</tr>
<tr>
 <td style="text-align: left;">
   rendo.base </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   rlm </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   selection </td><td style="text-align: left;"> response, link, unconditional, conditional </td>
</tr>
<tr>
 <td style="text-align: left;">
   speedglm </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   speedlm </td><td style="text-align: left;"> response </td>
</tr>
<tr>
 <td style="text-align: left;">
   stanreg </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   survreg </td><td style="text-align: left;"> response, link, quantile </td>
</tr>
<tr>
 <td style="text-align: left;">
   svyglm </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   svyolr </td><td style="text-align: left;"> probs </td>
</tr>
<tr>
 <td style="text-align: left;">
   tobit </td><td style="text-align: left;"> response, link </td>
</tr>
<tr>
 <td style="text-align: left;">
   tobit1 </td><td style="text-align: left;"> expvalue, linpred, prob </td>
</tr>
<tr>
 <td style="text-align: left;">
   workflow </td><td style="text-align: left;"> numeric, prob, class </td>
</tr>
<tr>
 <td style="text-align: left;">
   zeroinfl </td><td style="text-align: left;"> response, prob, count, zero </td>
</tr>
<tr>
 <td style="text-align: left;">
</td>
</tr>

</table>



### Examples
```{r, warning=FALSE, message=FALSE, eval=FALSE}
library("marginaleffects")


mod <- lm(mpg ~ hp + wt, data = mtcars)
plot_predictions(mod, condition = "wt")

mod <- lm(mpg ~ hp * wt * am, data = mtcars)
plot_predictions(mod, condition = c("hp", "wt"))

plot_predictions(mod, condition = list("hp", wt = "threenum"))

plot_predictions(mod, condition = list("hp", wt = range))

# marginal predictions
mod <- lm(mpg ~ hp * am, data = mtcars)
plot_predictions(mod, by = "am")

# marginal predictions on a counterfactual grid
plot_predictions(mod,
    by = "am",
    newdata = datagrid(am = 0:1, grid_type = "counterfactual")
)



```
