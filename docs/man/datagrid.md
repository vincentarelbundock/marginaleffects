
# datagrid

Data grids

## Description

Generate a data grid of user-specified values for use in the
<code>newdata</code> argument of the <code>predictions()</code>,
<code>comparisons()</code>, and <code>slopes()</code> functions. This is
useful to define where in the predictor space we want to evaluate the
quantities of interest. Ex: the predicted outcome or slope for a 37 year
old college graduate.

<ul>
<li>

<code>datagrid()</code> generates data frames with combinations of
"typical" or user-supplied predictor values.

</li>
<li>

<code>datagridcf()</code> generates "counter-factual" data frames, by
replicating the entire dataset once for every combination of predictor
values supplied by the user.

</li>
</ul>

## Usage

<pre><code class='language-R'>datagrid(
  ...,
  model = NULL,
  newdata = NULL,
  by = NULL,
  FUN_character = get_mode,
  FUN_factor = get_mode,
  FUN_logical = get_mode,
  FUN_numeric = function(x) mean(x, na.rm = TRUE),
  FUN_integer = function(x) round(mean(x, na.rm = TRUE)),
  FUN_other = function(x) mean(x, na.rm = TRUE),
  grid_type = "typical"
)

datagridcf(..., model = NULL, newdata = NULL)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="datagrid_:_...">…</code>
</td>
<td>

named arguments with vectors of values or functions for user-specified
variables.

<ul>
<li>

Functions are applied to the variable in the <code>model</code> dataset
or <code>newdata</code>, and must return a vector of the appropriate
type.

</li>
<li>

Character vectors are automatically transformed to factors if necessary.
+The output will include all combinations of these variables (see
Examples below.)

</li>
</ul>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="datagrid_:_model">model</code>
</td>
<td>
Model object
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="datagrid_:_newdata">newdata</code>
</td>
<td>
data.frame (one and only one of the <code>model</code> and
<code>newdata</code> arguments can be used.)
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="datagrid_:_by">by</code>
</td>
<td>
character vector with grouping variables within which
<code style="white-space: pre;">⁠FUN\_\*⁠</code> functions are applied to
create "sub-grids" with unspecified variables.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="datagrid_:_FUN_character">FUN_character</code>
</td>
<td>
the function to be applied to character variables.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="datagrid_:_FUN_factor">FUN_factor</code>
</td>
<td>
the function to be applied to factor variables.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="datagrid_:_FUN_logical">FUN_logical</code>
</td>
<td>
the function to be applied to logical variables.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="datagrid_:_FUN_numeric">FUN_numeric</code>
</td>
<td>
the function to be applied to numeric variables.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="datagrid_:_FUN_integer">FUN_integer</code>
</td>
<td>
the function to be applied to integer variables.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="datagrid_:_FUN_other">FUN_other</code>
</td>
<td>
the function to be applied to other variable types.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="datagrid_:_grid_type">grid_type</code>
</td>
<td>

character

<ul>
<li>

"typical": variables whose values are not explicitly specified by the
user in <code>…</code> are set to their mean or mode, or to the output
of the functions supplied to <code>FUN_type</code> arguments.

</li>
<li>

"counterfactual": the entire dataset is duplicated for each combination
of the variable values specified in <code>…</code>. Variables not
explicitly supplied to <code>datagrid()</code> are set to their observed
values in the original dataset.

</li>
</ul>
</td>
</tr>
</table>

## Details

If <code>datagrid</code> is used in a <code>predictions()</code>,
<code>comparisons()</code>, or <code>slopes()</code> call as the
<code>newdata</code> argument, the model is automatically inserted in
the <code>model</code> argument of <code>datagrid()</code> call, and
users do not need to specify either the <code>model</code> or
<code>newdata</code> arguments.

If users supply a model, the data used to fit that model is retrieved
using the <code>insight::get_data</code> function.

## Value

A <code>data.frame</code> in which each row corresponds to one
combination of the named predictors supplied by the user via the
<code>…</code> dots. Variables which are not explicitly defined are held
at their mean or mode.

## Functions

<ul>
<li>

<code>datagridcf()</code>: Counterfactual data grid

</li>
</ul>

## Examples

``` r
library(marginaleffects)

# The output only has 2 rows, and all the variables except `hp` are at their
# mean or mode.
datagrid(newdata = mtcars, hp = c(100, 110))
```

           mpg    cyl     disp     drat      wt     qsec     vs      am   gear
    1 20.09062 6.1875 230.7219 3.596563 3.21725 17.84875 0.4375 0.40625 3.6875
    2 20.09062 6.1875 230.7219 3.596563 3.21725 17.84875 0.4375 0.40625 3.6875
        carb  hp
    1 2.8125 100
    2 2.8125 110

``` r
# We get the same result by feeding a model instead of a data.frame
mod <- lm(mpg ~ hp, mtcars)
datagrid(model = mod, hp = c(100, 110))
```

           mpg  hp
    1 20.09062 100
    2 20.09062 110

``` r
# Use in `marginaleffects` to compute "Typical Marginal Effects". When used
# in `slopes()` or `predictions()` we do not need to specify the
#`model` or `newdata` arguments.
slopes(mod, newdata = datagrid(hp = c(100, 110)))
```


     Term  hp Estimate Std. Error     z Pr(>|z|)    S   2.5 %  97.5 %
       hp 100  -0.0682     0.0101 -6.74   <0.001 35.9 -0.0881 -0.0484
       hp 110  -0.0682     0.0101 -6.74   <0.001 35.9 -0.0881 -0.0484

    Columns: rowid, term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, hp, predicted_lo, predicted_hi, predicted, mpg 
    Type:  response 

``` r
# datagrid accepts functions
datagrid(hp = range, cyl = unique, newdata = mtcars)
```

           mpg     disp     drat      wt     qsec     vs      am   gear   carb  hp
    1 20.09062 230.7219 3.596563 3.21725 17.84875 0.4375 0.40625 3.6875 2.8125  52
    2 20.09062 230.7219 3.596563 3.21725 17.84875 0.4375 0.40625 3.6875 2.8125  52
    3 20.09062 230.7219 3.596563 3.21725 17.84875 0.4375 0.40625 3.6875 2.8125  52
    4 20.09062 230.7219 3.596563 3.21725 17.84875 0.4375 0.40625 3.6875 2.8125 335
    5 20.09062 230.7219 3.596563 3.21725 17.84875 0.4375 0.40625 3.6875 2.8125 335
    6 20.09062 230.7219 3.596563 3.21725 17.84875 0.4375 0.40625 3.6875 2.8125 335
      cyl
    1   6
    2   4
    3   8
    4   6
    5   4
    6   8

``` r
comparisons(mod, newdata = datagrid(hp = fivenum))
```


     Term Contrast  hp Estimate Std. Error     z Pr(>|z|)    S   2.5 %  97.5 %
       hp       +1  52  -0.0682     0.0101 -6.74   <0.001 35.9 -0.0881 -0.0484
       hp       +1  96  -0.0682     0.0101 -6.74   <0.001 35.9 -0.0881 -0.0484
       hp       +1 123  -0.0682     0.0101 -6.74   <0.001 35.9 -0.0881 -0.0484
       hp       +1 180  -0.0682     0.0101 -6.74   <0.001 35.9 -0.0881 -0.0484
       hp       +1 335  -0.0682     0.0101 -6.74   <0.001 35.9 -0.0881 -0.0484

    Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, hp, predicted_lo, predicted_hi, predicted, mpg 
    Type:  response 

``` r
# The full dataset is duplicated with each observation given counterfactual
# values of 100 and 110 for the `hp` variable. The original `mtcars` includes
# 32 rows, so the resulting dataset includes 64 rows.
dg <- datagrid(newdata = mtcars, hp = c(100, 110), grid_type = "counterfactual")
nrow(dg)
```

    [1] 64

``` r
# We get the same result by feeding a model instead of a data.frame
mod <- lm(mpg ~ hp, mtcars)
dg <- datagrid(model = mod, hp = c(100, 110), grid_type = "counterfactual")
nrow(dg)
```

    [1] 64
