## Print <code>marginaleffects</code> objects {.unnumbered}


### Description

This function controls the text which is printed to the console when one of the core <code>marginalefffects</code> functions is called and the object is returned: <code>predictions()</code>, <code>comparisons()</code>, <code>slopes()</code>, <code>hypotheses()</code>, <code>avg_predictions()</code>, <code>avg_comparisons()</code>, <code>avg_slopes()</code>.

All of those functions return standard data frames. Columns can be extracted by name, <code>predictions(model)\$estimate</code>, and all the usual data manipulation functions work out-of-the-box:  <code>colnames()</code>, <code>head()</code>, <code>subset()</code>, <code>dplyr::filter()</code>, <code>dplyr::arrange()</code>, etc.

Some of the data columns are not printed by default. You can disable pretty printing and print the full results as a standard data frame using the <code>style</code> argument or by applying <code>as.data.frame()</code> on the object. See examples below.



### Usage

<pre><code class='language-R'>## S3 method for class 'marginaleffects'
print(
  x,
  style = getOption("marginaleffects_print_style", default = "summary"),
  digits = getOption("marginaleffects_print_digits", default = 3),
  p_eps = getOption("marginaleffects_print_p_eps", default = 0.001),
  topn = getOption("marginaleffects_print_topn", default = 5),
  nrows = getOption("marginaleffects_print_nrows", default = 30),
  ncols = getOption("marginaleffects_print_ncols", default = 30),
  type = getOption("marginaleffects_print_type", default = TRUE),
  column_names = getOption("marginaleffects_print_column_names", default = FALSE),
  ...
)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="x">x</code></td>
<td>
An object produced by one of the <code>marginaleffects</code> package functions.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="style">style</code></td>
<td>
&quot;summary&quot;, &quot;data.frame&quot;, or &quot;tinytable&quot;
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="digits">digits</code></td>
<td>
The number of digits to display.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="p_eps">p_eps</code></td>
<td>
p values smaller than this number are printed in &quot;&lt;0.001&quot; style.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="topn">topn</code></td>
<td>
The number of rows to be printed from the beginning and end of tables with more than <code>nrows</code> rows.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="nrows">nrows</code></td>
<td>
The number of rows which will be printed before truncation.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="ncols">ncols</code></td>
<td>
The maximum number of column names to display at the bottom of the printed output.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="type">type</code></td>
<td>
boolean: should the type be printed?
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="column_names">column_names</code></td>
<td>
boolean: should the column names be printed?
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="...">...</code></td>
<td>
Other arguments are currently ignored.
</td></tr>
</table>


### Examples
```{r, warning=FALSE, message=FALSE, eval=TRUE}
library("marginaleffects")

library(marginaleffects)
mod <- lm(mpg ~ hp + am + factor(gear), data = mtcars)
p <- predictions(mod, by = c("am", "gear"))
p

subset(p, am == 1)

print(p, style = "data.frame")

data.frame(p)



```
