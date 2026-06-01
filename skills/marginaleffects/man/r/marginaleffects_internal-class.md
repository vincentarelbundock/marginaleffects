
## Internal S4 class for marginaleffects {.unnumbered}


### Description

This S4 class is used internally to hold common arguments passed between
functions to simplify the function signatures and reduce redundant argument passing.



### Slots


<dl>
<dt><code>by</code></dt><dd>Aggregation/grouping specification
</dd>
<dt><code>byfun</code></dt><dd>Function for aggregation when using by
</dd>
<dt><code>call</code></dt><dd>The original function call
</dd>
<dt><code>calling_function</code></dt><dd>The name of the calling function (comparisons, predictions, hypotheses)
</dd>
<dt><code>comparison</code></dt><dd>Comparison function specification
</dd>
<dt><code>cross</code></dt><dd>Boolean flag for cross-contrasts
</dd>
<dt><code>df</code></dt><dd>The degrees of freedom
</dd>
<dt><code>eps</code></dt><dd>Epsilon value for numerical derivatives
</dd>
<dt><code>jacobian</code></dt><dd>The jacobian matrix or NULL
</dd>
<dt><code>model</code></dt><dd>The fitted model object
</dd>
<dt><code>modeldata</code></dt><dd>The model data frame
</dd>
<dt><code>newdata</code></dt><dd>The new data frame for predictions
</dd>
<dt><code>type</code></dt><dd>The sanitized type from sanitize_type()
</dd>
<dt><code>vcov_model</code></dt><dd>The variance-covariance matrix
</dd>
<dt><code>wts</code></dt><dd>The weights specification
</dd>
</dl>

