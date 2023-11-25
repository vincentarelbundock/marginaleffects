
# inferences

(EXPERIMENTAL) Bootstrap, Conformal, and Simulation-Based Inference

## Description

Warning: This function is experimental. It may be renamed, the user
interface may change, or the functionality may migrate to arguments in
other <code>marginaleffects</code> functions.

Apply this function to a <code>marginaleffects</code> object to change
the inferential method used to compute uncertainty estimates.

## Usage

<pre><code class='language-R'>inferences(
  x,
  method,
  R = 1000,
  conf_type = "perc",
  conformal_test = NULL,
  conformal_calibration = NULL,
  conformal_score = "residual_abs",
  ...
)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="inferences_:_x">x</code>
</td>
<td>
Object produced by one of the core <code>marginaleffects</code>
functions.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="inferences_:_method">method</code>
</td>
<td>

String

<ul>
<li>

"delta": delta method standard errors

</li>
<li>

"boot" package

</li>
<li>

"fwb": fractional weighted bootstrap

</li>
<li>

"rsample" package

</li>
<li>

"simulation" from a multivariate normal distribution (Krinsky & Robb,
1986)

</li>
<li>

"mi" multiple imputation for missing data

</li>
<li>

"conformal_split": prediction intervals using split conformal prediction
(see Angelopoulos & Bates, 2022)

</li>
<li>

"conformal_cv+": prediction intervals using cross-validation+ conformal
prediction (see Barber et al., 2020)

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="inferences_:_R">R</code>
</td>
<td>
Number of resamples, simulations, or cross-validation folds.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="inferences_:_conf_type">conf_type</code>
</td>
<td>

String: type of bootstrap interval to construct.

<ul>
<li>

<code>boot</code>: "perc", "norm", "basic", or "bca"

</li>
<li>

<code>fwb</code>: "perc", "norm", "basic", "bc", or "bca"

</li>
<li>

<code>rsample</code>: "perc" or "bca"

</li>
<li>

<code>simulation</code>: argument ignored.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="inferences_:_conformal_test">conformal_test</code>
</td>
<td>
Data frame of test data for conformal prediction.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="inferences_:_conformal_calibration">conformal_calibration</code>
</td>
<td>
Data frame of calibration data for split conformal prediction
(<code style="white-space: pre;">⁠method=“conformal_split⁠</code>).
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="inferences_:_conformal_score">conformal_score</code>
</td>
<td>

String. Warning: The <code>type</code> argument in
<code>predictions()</code> must generate predictions which are on the
same scale as the outcome variable. Typically, this means that
<code>type</code> must be "response" or "probs".

<ul>
<li>

"residual_abs" or "residual_sq" for regression tasks (numeric outcome)

</li>
<li>

"softmax" for classification tasks (when <code>predictions()</code>
returns a <code>group</code> columns, such as multinomial or ordinal
logit models.

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="inferences_:_...">…</code>
</td>
<td>
<ul>
<li>

If <code>method=“boot”</code>, additional arguments are passed to
<code>boot::boot()</code>.

</li>
<li>

If <code>method=“fwb”</code>, additional arguments are passed to
<code>fwb::fwb()</code>.

</li>
<li>

If <code>method=“rsample”</code>, additional arguments are passed to
<code>rsample::bootstraps()</code>.

</li>
<li>

Additional arguments are ignored for all other methods.

</li>
</ul>
</td>
</tr>
</table>

## Details

When <code>method=“simulation”</code>, we conduct simulation-based
inference following the method discussed in Krinsky & Robb (1986):

<ol>
<li>

Draw <code>R</code> sets of simulated coefficients from a multivariate
normal distribution with mean equal to the original model’s estimated
coefficients and variance equal to the model’s variance-covariance
matrix (classical, "HC3", or other).

</li>
<li>

Use the <code>R</code> sets of coefficients to compute <code>R</code>
sets of estimands: predictions, comparisons, slopes, or hypotheses.

</li>
<li>

Take quantiles of the resulting distribution of estimands to obtain a
confidence interval and the standard deviation of simulated estimates to
estimate the standard error.

</li>
</ol>

When <code>method=“fwb”</code>, drawn weights are supplied to the model
fitting function’s <code>weights</code> argument; if the model doesn’t
accept non-integer weights, this method should not be used. If weights
were included in the original model fit, they are extracted by
<code>weights()</code> and multiplied by the drawn weights. These
weights are supplied to the <code>wts</code> argument of the estimation
function (e.g., <code>comparisons()</code>).

## Value

A <code>marginaleffects</code> object with simulation or bootstrap
resamples and objects attached.

## References

Krinsky, I., and A. L. Robb. 1986. “On Approximating the Statistical
Properties of Elasticities.” Review of Economics and Statistics 68 (4):
715–9.

King, Gary, Michael Tomz, and Jason Wittenberg. "Making the most of
statistical analyses: Improving interpretation and presentation."
American journal of political science (2000): 347-361

Dowd, Bryan E., William H. Greene, and Edward C. Norton. "Computation of
standard errors." Health services research 49.2 (2014): 731-750.

Angelopoulos, Anastasios N., and Stephen Bates. 2022. "A Gentle
Introduction to Conformal Prediction and Distribution-Free Uncertainty
Quantification." arXiv. https://doi.org/10.48550/arXiv.2107.07511.

Barber, Rina Foygel, Emmanuel J. Candes, Aaditya Ramdas, and Ryan J.
Tibshirani. 2020. “Predictive Inference with the Jackknife+.” arXiv.
http://arxiv.org/abs/1905.02928.

## Examples

``` r
library(marginaleffects)

library(marginaleffects)
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
  posterior_draws("rvar") %>%
  data.frame()

# Simulation-based inference
slopes(mod) %>%
  inferences(method = "simulation") %>%
  head()
```
