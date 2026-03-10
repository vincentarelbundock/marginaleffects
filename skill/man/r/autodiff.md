## EXPERIMENTAL &ndash; Enable Automatic Differentiation with JAX {.unnumbered}


### Description

This function enables or disables automatic differentiation using the JAX
package in Python, which can considerably speed up and increase the accuracy
of standard errors when a model includes many parameters.



### Usage

<pre><code class='language-R'>autodiff(autodiff = NULL, install = FALSE)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="autodiff">autodiff</code></td>
<td>
Logical flag. If <code>TRUE</code>, enables automatic differentiation
with JAX. If <code>FALSE</code> (default), disables automatic differentiation and
reverts to finite difference methods.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="install">install</code></td>
<td>
Logical flag. If <code>TRUE</code>, installs the <code>marginaleffects</code>
Python package via <code>reticulate::py_install()</code>. Default is <code>FALSE</code>. This is
only necessary if you are self-managing a Python installation.
</td></tr>
</table>


### Details

Automatic differentiation needs to be enabled once per session.

When <code>autodiff = TRUE</code>, this function:


<ul>
<li> Imports the <code>marginaleffects.autodiff</code> Python module via <code>reticulate::import()</code>

</li>
<li> Sets the internal jacobian function to use JAX-based automatic differentiation

</li>
<li> Provides faster and more accurate gradient computation for supported models

</li>
<li> Falls back on the default finite difference method for unsupported models and calls.

</li></ul>

Currently supports:


<ul>
<li> Model types: <code>lm</code>, <code>glm</code>, <code>ols</code>

</li>
<li> Functions: <code>predictions()</code> and <code>comparisons()</code>, along with <code>avg_</code> and <code>plot_</code> variants.

</li>
<li> <code>type</code>: &quot;response&quot; or &quot;link&quot;

</li>
<li> <code>by</code>: <code>TRUE</code>, <code>FALSE</code>, or character vector.

</li>
<li> <code>comparison</code>: &quot;difference&quot; and &quot;ratio&quot;

</li></ul>

For unsupported models or options, the function automatically falls back to
the default finite difference method.



### Value

No return value. Called for side effects of enabling/disabling
automatic differentiation.



### Python Configuration

By default, no manual configuration of Python should be necessary. On most
machines, unless you have explicitly configured <code>reticulate</code>, reticulate
defaults to an automatically managed ephemeral virtual environment with all
Python requirements declared via <code>reticulate::py_require()</code>.

If you prefer to use a manually managed Python installation, you can direct
<code>reticulate</code> and specify which Python executable or environment to use.
<code>reticulate</code> selects a Python installation using its <a href="https://rstudio.github.io/reticulate/articles/versions.html#order-of-discovery">Order of Discovery</a>.
As a convenience <code>autodiff(install=TRUE)</code> will install the <code>marginaleffects</code> Python
package in a self-managed virtual environment.

To specify an alternate Python version:

<div class="sourceCode r"><pre>library(reticulate)
use_python("/usr/local/bin/python")
</pre></div>
To use a virtual environment:

<div class="sourceCode r"><pre>use_virtualenv("myenv")
</pre></div>
These configuration commands should be called before calling <code>autodiff()</code>.



### Examples
```{r, warning=FALSE, message=FALSE, eval=FALSE}
library("marginaleffects")

# Install the Python package (only needed once)
autodiff(install = TRUE)

# Enable automatic differentiation
autodiff(TRUE)

# Fit a model and compute marginal effects
mod <- glm(am ~ hp + wt, data = mtcars, family = binomial)
avg_comparisons(mod) # Will use JAX for faster computation

# Disable automatic differentiation
autodiff(FALSE)




```
