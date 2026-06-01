
## Constructor for marginaleffects_internal class {.unnumbered}


### Description

Constructor for marginaleffects_internal class



### Usage

<pre><code class='language-R'>new_marginaleffects_internal(
  model,
  call,
  by = FALSE,
  byfun = NULL,
  comparison = NULL,
  conf_level = 0.95,
  cross = FALSE,
  df = NULL,
  draws = NULL,
  draws_chains = 0,
  eps = NULL,
  hypothesis = NULL,
  hypothesis_null = NULL,
  hypothesis_direction = NULL,
  jacobian = NULL,
  modeldata = NULL,
  numderiv = list("fdforward"),
  type = NULL,
  variables = list(),
  variable_names_by = character(),
  variable_names_by_hypothesis = character(),
  vcov_model = NULL,
  vcov_type = NULL,
  wts = NULL
)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="model">model</code></td>
<td>
The fitted model object (required)
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="call">call</code></td>
<td>
The original function call (required)
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="df">df</code></td>
<td>
The degrees of freedom
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="type">type</code></td>
<td>
The sanitized type from sanitize_type()
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="vcov_model">vcov_model</code></td>
<td>
The variance-covariance matrix
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="wts">wts</code></td>
<td>
The weights specification
</td></tr>
</table>


### Value

An object of class marginaleffects_internal


