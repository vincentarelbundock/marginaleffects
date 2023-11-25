
# print.marginaleffects

Print <code>marginaleffects</code> objects

## Description

This function controls the text which is printed to the console when one
of the core <code>marginalefffects</code> functions is called and the
object is returned: <code>predictions()</code>,
<code>comparisons()</code>, <code>slopes()</code>,
<code>marginal_means()</code>, <code>hypotheses()</code>,
<code>avg_predictions()</code>, <code>avg_comparisons()</code>,
<code>avg_slopes()</code>.

All of those functions return standard data frames. Columns can be
extracted by name, <code>predictions(model)$estimate</code>, and all the
usual data manipulation functions work out-of-the-box:
<code>colnames()</code>, <code>head()</code>, <code>subset()</code>,
<code>dplyr::filter()</code>, <code>dplyr::arrange()</code>, etc.

Some of the data columns are not printed by default. You can disable
pretty printing and print the full results as a standard data frame
using the <code>style</code> argument or by applying
<code>as.data.frame()</code> on the object. See examples below.

## Usage

<pre><code class='language-R'>## S3 method for class 'marginaleffects'
print(
  x,
  digits = getOption("marginaleffects_print_digits", default = 3),
  p_eps = getOption("marginaleffects_print_p_eps", default = 0.001),
  topn = getOption("marginaleffects_print_topn", default = 5),
  nrows = getOption("marginaleffects_print_nrows", default = 30),
  ncols = getOption("marginaleffects_print_ncols", default = 30),
  style = getOption("marginaleffects_print_style", default = "summary"),
  type = getOption("marginaleffects_print_type", default = TRUE),
  column_names = getOption("marginaleffects_print_column_names", default = TRUE),
  ...
)
</code></pre>

## Arguments

<table>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="print.marginaleffects_:_x">x</code>
</td>
<td>
An object produced by one of the <code>marginaleffects</code> package
functions.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="print.marginaleffects_:_digits">digits</code>
</td>
<td>
The number of digits to display.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="print.marginaleffects_:_p_eps">p_eps</code>
</td>
<td>
p values smaller than this number are printed in "\<0.001" style.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="print.marginaleffects_:_topn">topn</code>
</td>
<td>
The number of rows to be printed from the beginning and end of tables
with more than <code>nrows</code> rows.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="print.marginaleffects_:_nrows">nrows</code>
</td>
<td>
The number of rows which will be printed before truncation.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="print.marginaleffects_:_ncols">ncols</code>
</td>
<td>
The maximum number of column names to display at the bottom of the
printed output.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="print.marginaleffects_:_style">style</code>
</td>
<td>
"summary" or "data.frame"
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="print.marginaleffects_:_type">type</code>
</td>
<td>
boolean: should the type be printed?
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="print.marginaleffects_:_column_names">column_names</code>
</td>
<td>
boolean: should the column names be printed?
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="print.marginaleffects_:_...">…</code>
</td>
<td>
Other arguments are currently ignored.
</td>
</tr>
</table>

## Examples

``` r
library(marginaleffects)

library(marginaleffects)
mod <- lm(mpg ~ hp + am + factor(gear), data = mtcars)
p <- predictions(mod, by = c("am", "gear"))
p
```


     am gear Estimate Std. Error    z Pr(>|z|)     S 2.5 % 97.5 %
      0    3     16.1      0.759 21.2   <0.001 329.6  14.6   17.6
      0    4     21.0      1.470 14.3   <0.001 152.1  18.2   23.9
      1    4     26.3      1.039 25.3   <0.001 466.1  24.2   28.3
      1    5     21.4      1.315 16.3   <0.001 195.2  18.8   24.0

    Columns: am, gear, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
    Type:  response 

``` r
subset(p, am == 1)
```


     Estimate Std. Error    z Pr(>|z|)     S CI low CI high
         26.3       1.04 25.3   <0.001 466.1   24.2    28.3
         21.4       1.31 16.3   <0.001 195.2   18.8    24.0

    Columns: am, gear, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 

``` r
print(p, style = "data.frame")
```

      am gear estimate std.error statistic       p.value  s.value conf.low
    1  0    3 16.10667 0.7589789  21.22150 6.046968e-100 329.5966 14.61910
    2  0    4 21.05000 1.4697545  14.32212  1.591933e-46 152.1379 18.16933
    3  1    4 26.27500 1.0392747  25.28205 5.032443e-141 466.0606 24.23806
    4  1    5 21.38000 1.3145900  16.26363  1.788354e-59 195.1551 18.80345
      conf.high
    1  17.59424
    2  23.93067
    3  28.31194
    4  23.95655

``` r
data.frame(p)
```

      am gear estimate std.error statistic       p.value  s.value conf.low
    1  0    3 16.10667 0.7589789  21.22150 6.046968e-100 329.5966 14.61910
    2  0    4 21.05000 1.4697545  14.32212  1.591933e-46 152.1379 18.16933
    3  1    4 26.27500 1.0392747  25.28205 5.032443e-141 466.0606 24.23806
    4  1    5 21.38000 1.3145900  16.26363  1.788354e-59 195.1551 18.80345
      conf.high
    1  17.59424
    2  23.93067
    3  28.31194
    4  23.95655
