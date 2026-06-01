## Data grids {.unnumbered}


### Description

Generate a data grid of user-specified values for use in the <code>newdata</code> argument of the <code>predictions()</code>, <code>comparisons()</code>, and <code>slopes()</code> functions. This is useful to define where in the predictor space we want to evaluate the quantities of interest. Ex: the predicted outcome or slope for a 37 year old college graduate.



### Usage

<pre><code class='language-R'>datagrid(
  ...,
  model = NULL,
  newdata = NULL,
  by = NULL,
  grid_type = "mean_or_mode",
  response = FALSE,
  FUN = NULL,
  FUN_character = NULL,
  FUN_factor = NULL,
  FUN_logical = NULL,
  FUN_numeric = NULL,
  FUN_integer = NULL,
  FUN_binary = NULL,
  FUN_other = NULL
)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="...">...</code></td>
<td>
named arguments with vectors of values or functions for user-specified variables.


<ul>
<li> Functions are applied to the variable in the <code>model</code> dataset or <code>newdata</code>, and must return a vector of the appropriate type.

</li>
<li> Character vectors are automatically transformed to factors if necessary.

</li>
<li> The output will include all combinations of these variables (see Examples below.)

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="model">model</code></td>
<td>
Model object
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="newdata">newdata</code></td>
<td>
data.frame (one and only one of the <code>model</code> and <code>newdata</code> arguments can be used.)
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="by">by</code></td>
<td>
character vector with grouping variables within which <code style="white-space: pre;">FUN_*</code> functions are applied to create &quot;sub-grids&quot; with unspecified variables.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="grid_type">grid_type</code></td>
<td>
character. Determines the functions to apply to each variable. The defaults can be overridden by defining individual variables explicitly in <code>...</code>, or by supplying a function to one of the <code style="white-space: pre;">FUN_*</code> arguments.


<ul>
<li> &quot;mean_or_mode&quot;: Character, factor, logical, and binary variables are set to their modes. Numeric, integer, and other variables are set to their means.

</li>
<li> &quot;balanced&quot;: Each unique level of character, factor, logical, and binary variables are preserved. Numeric, integer, and other variables are set to their means. Warning: When there are many variables and many levels per variable, a balanced grid can be very large. In those cases, it is better to use <code>grid_type="mean_or_mode"</code> and to specify the unique levels of a subset of named variables explicitly.

</li>
<li> &quot;dataframe&quot;: Similar to &quot;mean_or_mode&quot; but creates a data frame by binding columns element-wise rather than taking the cross-product. All explicitly specified vectors must have the same length (or length 1), and the result has as many rows as the longest vector. This differs from other grid types which use <code>expand.grid()</code> or <code>data.table::CJ()</code> to create all combinations.

</li>
<li> &quot;counterfactual&quot;: the entire dataset is duplicated for each combination of the variable values specified in <code>...</code>. Variables not explicitly supplied to <code>datagrid()</code> are set to their observed values in the original dataset.

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="response">response</code></td>
<td>
Logical should the response variable be included in the grid, even if it is not specified explicitly.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="FUN">FUN</code></td>
<td>
a function to be applied to all variables in the grid. This is useful when you want to apply the same function to all variables, such as <code>mean</code> or <code>median</code>. If you specify <code>FUN</code>, it will override the <code>grid_type</code> defaults, but not other <code style="white-space: pre;">FUN_*</code> arguments below.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="FUN_character">FUN_character</code></td>
<td>
the function to be applied to character variables.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="FUN_factor">FUN_factor</code></td>
<td>
the function to be applied to factor variables. This only applies if the variable in the original data is a factor. For variables converted to factor in a model-fitting formula, for example, <code>FUN_character</code> is used.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="FUN_logical">FUN_logical</code></td>
<td>
the function to be applied to logical variables.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="FUN_numeric">FUN_numeric</code></td>
<td>
the function to be applied to numeric variables.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="FUN_integer">FUN_integer</code></td>
<td>
the function to be applied to integer-ish variables (including columns without decimal places).
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="FUN_binary">FUN_binary</code></td>
<td>
the function to be applied to binary variables.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="FUN_other">FUN_other</code></td>
<td>
the function to be applied to other variable types.
</td></tr>
</table>


### Details

If <code>datagrid</code> is used in a <code>predictions()</code>, <code>comparisons()</code>, or <code>slopes()</code> call as the
<code>newdata</code> argument, the model is automatically inserted in the <code>model</code> argument of <code>datagrid()</code>
call, and users do not need to specify either the <code>model</code> or <code>newdata</code> arguments. The same behavior will occur when the value supplied to <code style="white-space: pre;">newdata=</code> is a function call which starts with &quot;datagrid&quot;. This is intended to allow users to create convenience shortcuts like:

<strong>Warning about hierarchical grouping variables:</strong> When using the default <code>grid_type = "mean_or_mode"</code> with hierarchical models (such as mixed models with nested grouping factors), <code>datagrid()</code> may create invalid combinations of grouping variables. For example, if you have students nested within schools, or countries nested within regions, the modal values of each grouping variable may not correspond to valid nested relationships in the data. This can cause prediction errors. To avoid this issue, explicitly specify valid combinations of hierarchical grouping variables in the <code>datagrid()</code> call, or use <code>grid_type = "counterfactual"</code> to preserve the original data structure.

<pre>
mod &lt;- lm(mpg ~ am + vs + factor(cyl) + hp, mtcars)
datagrid_bal &lt;- function(...) datagrid(..., grid_type = "balanced")
predictions(model, newdata = datagrid_bal(cyl = 4))
</pre>
If users supply a model, the data used to fit that model is retrieved using
the <code>insight::get_data</code> function.



### Value

A <code>data.frame</code> in which each row corresponds to one combination of the named
predictors supplied by the user via the <code>...</code> dots. Variables which are not
explicitly defined are held at their mean or mode.



### Examples
```{r, warning=FALSE, message=FALSE, eval=TRUE}
library("marginaleffects")

# The output only has 2 rows, and all the variables except `hp` are at their
# mean or mode.
datagrid(newdata = mtcars, hp = c(100, 110))

# We get the same result by feeding a model instead of a data.frame
mod <- lm(mpg ~ hp, mtcars)
datagrid(model = mod, hp = c(100, 110))

# Use in `marginaleffects` to compute "Typical Marginal Effects". When used
# in `slopes()` or `predictions()` we do not need to specify the
# `model` or `newdata` arguments.
slopes(mod, newdata = datagrid(hp = c(100, 110)))

# datagrid accepts functions
datagrid(hp = range, cyl = unique, newdata = mtcars)
comparisons(mod, newdata = datagrid(hp = fivenum))

# The full dataset is duplicated with each observation given counterfactual
# values of 100 and 110 for the `hp` variable. The original `mtcars` includes
# 32 rows, so the resulting dataset includes 64 rows.
dg <- datagrid(newdata = mtcars, hp = c(100, 110), grid_type = "counterfactual")
nrow(dg)

# We get the same result by feeding a model instead of a data.frame
mod <- lm(mpg ~ hp, mtcars)
dg <- datagrid(model = mod, hp = c(100, 110), grid_type = "counterfactual")
nrow(dg)

# Use `by` to hold variables at group-specific values
mod2 <- lm(mpg ~ hp + cyl, mtcars)
datagrid(model = mod2, hp = mean, by = "cyl")

# Use `FUN` to apply function to all variables
datagrid(model = mod2, FUN = median)

# Use `grid_type="dataframe"` for column-wise binding instead of cross-product
datagrid(model = mod2, hp = c(100, 200), cyl = c(4, 6), grid_type = "dataframe")


```
