## (EXPERIMENTAL) Bootstrap, Conformal, and Simulation-Based Inference {.unnumbered}


### Description

Warning: This function is experimental. It may be renamed, the user interface may change, or the functionality may migrate to arguments in other <code>marginaleffects</code> functions.

Apply this function to a <code>marginaleffects</code> object to change the inferential method used to compute uncertainty estimates.



### Usage

<pre><code class='language-R'>inferences(
  x,
  method,
  R = 1000,
  conf_type = "perc",
  data_train = NULL,
  data_test = NULL,
  data_calib = NULL,
  conformal_score = "residual_abs",
  estimator = NULL,
  ...
)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="x">x</code></td>
<td>
Object produced by one of the core <code>marginaleffects</code> functions, or a data frame suitable for the function supplied to the <code>estimator</code> argument.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="method">method</code></td>
<td>
String


<ul>
<li> &quot;delta&quot;: delta method standard errors

</li>
<li> &quot;boot&quot; package

</li>
<li> &quot;fwb&quot;: fractional weighted bootstrap

</li>
<li> &quot;rsample&quot; package

</li>
<li> &quot;simulation&quot; from a multivariate normal distribution (Krinsky &amp; Robb, 1986)

</li>
<li> &quot;conformal_split&quot;: prediction intervals using split conformal prediction (see Angelopoulos &amp; Bates, 2022)

</li>
<li> &quot;conformal_cv+&quot;: prediction intervals using cross-validation+ conformal prediction (see Barber et al., 2020)

</li>
<li> &quot;conformal_full&quot;: prediction intervals using full conformal prediction (see Lei et al., 2018). <strong>Warning</strong>: This method is computationally expensive and typically much slower than split or CV+ methods.

</li>
<li> &quot;conformal_quantile&quot;: prediction intervals using full conformal prediction (see Romano et al., 2020).

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="R">R</code></td>
<td>
Number of resamples, simulations, or cross-validation folds.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="conf_type">conf_type</code></td>
<td>
String: type of bootstrap interval to construct.


<ul>
<li> <code>boot</code>: &quot;perc&quot;, &quot;norm&quot;, &quot;basic&quot;, or &quot;bca&quot;

</li>
<li> <code>fwb</code>: &quot;perc&quot;, &quot;norm&quot;, &quot;wald&quot;, &quot;basic&quot;, &quot;bc&quot;, or &quot;bca&quot;

</li>
<li> <code>rsample</code>: &quot;perc&quot; or &quot;bca&quot;

</li>
<li> <code>simulation</code>: &quot;perc&quot; or &quot;wald&quot;

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="data_train">data_train</code></td>
<td>
Data frame used to train/fit the model. If <code>NULL</code>, <code>marginaleffects</code> tries to extract the data from the original model object. Test data are inferred directly from the <code>newdata</code> supplied to the originating <code>marginaleffects</code> call (e.g., <code>predictions()</code>).
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="data_test">data_test</code></td>
<td>
Data frame make out of sample prediction. Only used for conformal inference. If <code>NULL</code>, the data frame supplied to <code>newdata</code> in the original <code>marginaleffects</code> call is used.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="data_calib">data_calib</code></td>
<td>
Data frame used for calibration in split conformal prediction.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="conformal_score">conformal_score</code></td>
<td>
String. Warning: The <code>type</code> argument in <code>predictions()</code> must generate predictions which are on the same scale as the outcome variable. Typically, this means that <code>type</code> must be &quot;response&quot; or &quot;probs&quot;.


<ul>
<li> &quot;residual_abs&quot; or &quot;residual_sq&quot; for regression tasks (numeric outcome)

</li>
<li> &quot;softmax&quot; for classification tasks (when <code>predictions()</code> returns a <code>group</code> columns, such as multinomial or ordinal logit models.

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="estimator">estimator</code></td>
<td>
Function that accepts a data frame, fits a model, applies a <code>marginaleffects</code> function, and returns the object. Only supported with <code>method = "rsample"</code> or <code>method = "boot"</code>. When <code>method = "rsample"</code>, the output must include a &quot;term&quot; column. This is not always the case for <code>predictions()</code>, in which case users may have to create the column manually.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="...">...</code></td>
<td>

<ul>
<li> If <code>method = "boot"</code>, additional arguments are passed to <code>boot::boot()</code>.

</li>
<li> If <code>method = "fwb"</code>, additional arguments are passed to <code>fwb::fwb()</code>.

</li>
<li> If <code>method = "rsample"</code>, additional arguments are passed to <code>rsample::bootstraps()</code>, unless the user supplies a <code>group</code> argument, in which case all arguments are passed to <code>rsample::group_bootstraps()</code>.

</li>
<li> If <code>method = "conformal_full"</code>, additional arguments control the optimization process:


<ul>
<li> <code>var_multiplier</code>: multiplier for initial search bounds (default: 10)

</li>
<li> <code>max_iter</code>: maximum iterations for root finding (default: 100)

</li>
<li> <code>tolerance</code>: tolerance for root finding convergence (default: <code>.Machine\$double.eps^0.25</code>)

</li></ul>

</li>
<li> If <code>method = "conformal_quantile"</code>, additional arguments are passed to <code>quantregForest::quantregForest()</code> for fitting the quantile regression forest (e.g., <code>ntree</code>, <code>mtry</code>, <code>nodesize</code>, <code>nthreads</code>).

</li>
<li> Additional arguments are ignored for other conformal methods (<code>conformal_split</code>, <code style="white-space: pre;">conformal_cv+</code>).

</li></ul>
</td></tr>
</table>


### Details

When <code>method = "simulation"</code>, we conduct simulation-based inference following the method discussed in Krinsky &amp; Robb (1986):


<ol>
<li> Draw <code>R</code> sets of simulated coefficients from a multivariate normal distribution with mean equal to the original model's estimated coefficients and variance equal to the model's variance-covariance matrix (classical, &quot;HC3&quot;, or other).

</li>
<li> Use the <code>R</code> sets of coefficients to compute <code>R</code> sets of estimands: predictions, comparisons, slopes, or hypotheses.

</li>
<li> Take quantiles of the resulting distribution of estimands to obtain a confidence interval (when <code>conf_type = "perc"</code>) and the standard deviation of simulated estimates to estimate the standard error (which is used for a Z-test and Wald confidence intervals when <code>conf_type = "wald"</code>).

</li></ol>

When <code>method = "fwb"</code>, drawn weights are supplied to the model fitting function's <code>weights</code> argument; if the model doesn't accept non-integer weights, this method should not be used. If weights were included in the original model fit, they are extracted by <code>weights()</code> and multiplied by the drawn weights. These weights are supplied to the <code>wts</code> argument of the estimation function (e.g., <code>comparisons()</code>).

Warning: custom model classes are not supported by <code>inferences()</code> because they are not guaranteed to come with an appropriate <code>update()</code> method.



### Value

A <code>marginaleffects</code> object with simulation or bootstrap resamples and objects attached.



### References

Krinsky, I., and A. L. Robb. 1986. &quot;On Approximating the Statistical Properties of Elasticities.&quot; Review of Economics and Statistics 68 (4): 715–9.

King, Gary, Michael Tomz, and Jason Wittenberg. &quot;Making the most of statistical analyses: Improving interpretation and presentation.&quot; American journal of political science (2000): 347-361

Dowd, Bryan E., William H. Greene, and Edward C. Norton. &quot;Computation of standard errors.&quot; Health services research 49.2 (2014): 731-750.

Angelopoulos, Anastasios N., and Stephen Bates. 2022. &quot;A Gentle Introduction to Conformal Prediction and Distribution-Free Uncertainty Quantification.&quot; arXiv. https://doi.org/10.48550/arXiv.2107.07511.

Barber, Rina Foygel, Emmanuel J. Candes, Aaditya Ramdas, and Ryan J. Tibshirani. 2020. &quot;Predictive Inference with the Jackknife+.&quot; arXiv. http://arxiv.org/abs/1905.02928.

Lei, Jing, Max G'Sell, Alessandro Rinaldo, Ryan J. Tibshirani, and Larry Wasserman. 2018. &quot;Distribution-Free Predictive Inference for Regression.&quot; Journal of the American Statistical Association 113 (523): 1094–1111.

Romano, Yaniv, Evan Patterson, and Emmanuel Candes. 2020. &quot;Conformalized quantile regression.&quot; Advances in neural information processing systems 32.



### Parallel computation

The <code>slopes()</code> and <code>comparisons()</code> functions can use parallelism to
speed up computation. Operations are parallelized for the computation of
standard errors, at the model coefficient level. There is always
considerable overhead when using parallel computation, mainly involved
in passing the whole dataset to the different processes. Thus, parallel
computation is most likely to be useful when the model includes many parameters
and the dataset is relatively small.

Warning: In many cases, parallel processing will not be useful at all.

To activate parallel computation, users must load the <code>future.apply</code> package,
call <code>plan()</code> function, and set a global option.

<code>options(marginaleffects_parallel = TRUE)</code>: parallelize delta method computation of standard errors.
<code>options(marginaleffects_parallel_inferences = TRUE)</code>: parallelize <code>"rsample"</code> or <code>"fwb"</code> bootstrap computation in <code>inferences()</code>.
<code>options(marginaleffects_parallel_packages = TRUE)</code>: vector of strings with the names of modeling packages used to fit the model, ex: c(&quot;survival&quot;, &quot;splines&quot;)

For example:

<div class="sourceCode r"><pre>library(future.apply)
plan("multisession", workers = 4)
options(marginaleffects_parallel = FALSE)
options(marginaleffects_parallel_inferences = TRUE)
options(marginaleffects_parallel_packages = c("survival", "splines"))

slopes(model)
</pre></div>
To disable parallelism in <code>marginaleffects</code> altogether, you can set a global option:

<div class="sourceCode r"><pre>options(marginaleffects_parallel = FALSE)
</pre></div>


### Examples
```{r, warning=FALSE, message=FALSE, eval=FALSE}
library("marginaleffects")

library(magrittr)
set.seed(1024)
mod <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)

# bootstrap
avg_predictions(mod, by = "Species") %>%
    inferences(method = "boot")

avg_predictions(mod, by = "Species") %>%
    inferences(method = "rsample")

# Fractional (bayesian) bootstrap
avg_slopes(mod, by = "Species") %>%
    inferences(method = "fwb") %>%
    get_draws("rvar") %>%
    data.frame()

# Simulation-based inference
slopes(mod) %>%
    inferences(method = "simulation") %>%
    head()

# Two-step estimation procedure: Propensity score + G-Computation
lalonde <- get_dataset("lalonde")
estimator <- function(data) {
    # Step 1: Estimate propensity scores
    fit1 <- glm(treat ~ age + educ + race, family = binomial, data = data)
    ps <- predict(fit1, type = "response")
    # Step 2: Fit weighted outcome model
    m <- lm(re78 ~ treat * (re75 + age + educ + race),
        data = data, weight = ps
    )
    # Step 3: Compute average treatment effect by G-computation
    avg_comparisons(m, variables = "treat", wts = ps, vcov = FALSE)
}
inferences(lalonde, method = "rsample", estimator = estimator)



```
