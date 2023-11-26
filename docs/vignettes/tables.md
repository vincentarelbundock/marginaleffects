
# Tables

## Marginal effects

We can summarize the results of the `comparisons()` or `slopes()`
functions using [the `modelsummary`
package.](https://github.com/vincentarelbundock/modelsummary)

``` r
library(modelsummary)
library(marginaleffects)
options(modelsummary_factory_default = "gt")

mod <- glm(am ~ wt + drat, family = binomial, data = mtcars)
mfx <- slopes(mod)

modelsummary(mfx)
```

<div id="tltmqnjpnc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#tltmqnjpnc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#tltmqnjpnc thead, #tltmqnjpnc tbody, #tltmqnjpnc tfoot, #tltmqnjpnc tr, #tltmqnjpnc td, #tltmqnjpnc th {
  border-style: none;
}

#tltmqnjpnc p {
  margin: 0;
  padding: 0;
}

#tltmqnjpnc .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#tltmqnjpnc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#tltmqnjpnc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#tltmqnjpnc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#tltmqnjpnc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tltmqnjpnc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tltmqnjpnc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tltmqnjpnc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#tltmqnjpnc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#tltmqnjpnc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tltmqnjpnc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tltmqnjpnc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#tltmqnjpnc .gt_spanner_row {
  border-bottom-style: hidden;
}

#tltmqnjpnc .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#tltmqnjpnc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#tltmqnjpnc .gt_from_md > :first-child {
  margin-top: 0;
}

#tltmqnjpnc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tltmqnjpnc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#tltmqnjpnc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#tltmqnjpnc .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#tltmqnjpnc .gt_row_group_first td {
  border-top-width: 2px;
}

#tltmqnjpnc .gt_row_group_first th {
  border-top-width: 2px;
}

#tltmqnjpnc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tltmqnjpnc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#tltmqnjpnc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#tltmqnjpnc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tltmqnjpnc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tltmqnjpnc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tltmqnjpnc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#tltmqnjpnc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tltmqnjpnc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tltmqnjpnc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tltmqnjpnc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tltmqnjpnc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tltmqnjpnc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tltmqnjpnc .gt_left {
  text-align: left;
}

#tltmqnjpnc .gt_center {
  text-align: center;
}

#tltmqnjpnc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tltmqnjpnc .gt_font_normal {
  font-weight: normal;
}

#tltmqnjpnc .gt_font_bold {
  font-weight: bold;
}

#tltmqnjpnc .gt_font_italic {
  font-style: italic;
}

#tltmqnjpnc .gt_super {
  font-size: 65%;
}

#tltmqnjpnc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#tltmqnjpnc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#tltmqnjpnc .gt_indent_1 {
  text-indent: 5px;
}

#tltmqnjpnc .gt_indent_2 {
  text-indent: 10px;
}

#tltmqnjpnc .gt_indent_3 {
  text-indent: 15px;
}

#tltmqnjpnc .gt_indent_4 {
  text-indent: 20px;
}

#tltmqnjpnc .gt_indent_5 {
  text-indent: 25px;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="header gt_col_headings">
<th id=" " class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col"></th>
<th id="(1)" class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">(1)</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="odd">
<td class="gt_row gt_left" headers="">drat</td>
<td class="gt_row gt_center" headers="(1)">0.278</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">(0.168)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">wt</td>
<td class="gt_row gt_center" headers="(1)">-0.217</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000"></td>
<td class="gt_row gt_center" headers="(1)"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000">(0.080)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">Num.Obs.</td>
<td class="gt_row gt_center" headers="(1)">32</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">AIC</td>
<td class="gt_row gt_center" headers="(1)">22.0</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">BIC</td>
<td class="gt_row gt_center" headers="(1)">26.4</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">Log.Lik.</td>
<td class="gt_row gt_center" headers="(1)">-8.011</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">F</td>
<td class="gt_row gt_center" headers="(1)">3.430</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">RMSE</td>
<td class="gt_row gt_center" headers="(1)">0.28</td>
</tr>
</tbody>
</table>

</div>

The same results can be visualized with `modelplot()`:

``` r
modelplot(mfx)
```

<img
src="../tables.markdown_strict_files/figure-markdown_strict/unnamed-chunk-3-1.png"
style="width:100.0%" />

## Contrasts

When using the `comparisons()` function (or the `slopes()` function with
categorical variables), the output will include two columns to uniquely
identify the quantities of interest: `term` and `contrast`.

``` r
dat <- mtcars
dat$gear <- as.factor(dat$gear)
mod <- glm(vs ~ gear + mpg, data = dat, family = binomial)

cmp <- comparisons(mod)
get_estimates(cmp)
#> # A tibble: 3 × 8
#>   term  contrast          estimate std.error statistic    p.value conf.low conf.high
#>   <chr> <chr>                <dbl>     <dbl>     <dbl>      <dbl>    <dbl>     <dbl>
#> 1 gear  mean(4) - mean(3)   0.0372    0.137      0.272 0.785       -0.230     0.305 
#> 2 gear  mean(5) - mean(3)  -0.340     0.0988    -3.44  0.000588    -0.533    -0.146 
#> 3 mpg   mean(+1)            0.0609    0.0128     4.78  0.00000178   0.0359    0.0859
```

We can use the `shape` argument of the `modelsummary` function to
structure the table properly:

``` r
modelsummary(cmp, shape = term + contrast ~ model)
```

<div id="hqiagecqcy" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hqiagecqcy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hqiagecqcy thead, #hqiagecqcy tbody, #hqiagecqcy tfoot, #hqiagecqcy tr, #hqiagecqcy td, #hqiagecqcy th {
  border-style: none;
}

#hqiagecqcy p {
  margin: 0;
  padding: 0;
}

#hqiagecqcy .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hqiagecqcy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hqiagecqcy .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hqiagecqcy .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hqiagecqcy .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hqiagecqcy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqiagecqcy .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hqiagecqcy .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hqiagecqcy .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hqiagecqcy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hqiagecqcy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hqiagecqcy .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hqiagecqcy .gt_spanner_row {
  border-bottom-style: hidden;
}

#hqiagecqcy .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hqiagecqcy .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hqiagecqcy .gt_from_md > :first-child {
  margin-top: 0;
}

#hqiagecqcy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hqiagecqcy .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hqiagecqcy .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hqiagecqcy .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hqiagecqcy .gt_row_group_first td {
  border-top-width: 2px;
}

#hqiagecqcy .gt_row_group_first th {
  border-top-width: 2px;
}

#hqiagecqcy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqiagecqcy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hqiagecqcy .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hqiagecqcy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqiagecqcy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqiagecqcy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hqiagecqcy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hqiagecqcy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hqiagecqcy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqiagecqcy .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hqiagecqcy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqiagecqcy .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hqiagecqcy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqiagecqcy .gt_left {
  text-align: left;
}

#hqiagecqcy .gt_center {
  text-align: center;
}

#hqiagecqcy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hqiagecqcy .gt_font_normal {
  font-weight: normal;
}

#hqiagecqcy .gt_font_bold {
  font-weight: bold;
}

#hqiagecqcy .gt_font_italic {
  font-style: italic;
}

#hqiagecqcy .gt_super {
  font-size: 65%;
}

#hqiagecqcy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hqiagecqcy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hqiagecqcy .gt_indent_1 {
  text-indent: 5px;
}

#hqiagecqcy .gt_indent_2 {
  text-indent: 10px;
}

#hqiagecqcy .gt_indent_3 {
  text-indent: 15px;
}

#hqiagecqcy .gt_indent_4 {
  text-indent: 20px;
}

#hqiagecqcy .gt_indent_5 {
  text-indent: 25px;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="header gt_col_headings">
<th id=" " class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col"></th>
<th id="  " class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col"></th>
<th id="(1)" class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">(1)</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="odd">
<td class="gt_row gt_left" headers="">gear</td>
<td class="gt_row gt_left" headers="">mean(4) - mean(3)</td>
<td class="gt_row gt_center" headers="(1)">0.037</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">(0.137)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="">mean(5) - mean(3)</td>
<td class="gt_row gt_center" headers="(1)">-0.340</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">(0.099)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">mpg</td>
<td class="gt_row gt_left" headers="">mean(+1)</td>
<td class="gt_row gt_center" headers="(1)">0.061</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000"></td>
<td class="gt_row gt_left" headers=""
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000"></td>
<td class="gt_row gt_center" headers="(1)"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000">(0.013)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">Num.Obs.</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">32</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">AIC</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">26.2</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">BIC</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">32.1</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">Log.Lik.</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">-9.101</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">F</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">2.389</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">RMSE</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">0.31</td>
</tr>
</tbody>
</table>

</div>

Cross-contrasts can be a bit trickier, since there are multiple
simultaneous groups. Consider this example:

``` r
mod <- lm(mpg ~ factor(cyl) + factor(gear), data = mtcars)
cmp <- comparisons(
  mod,
  variables = c("gear", "cyl"),
  cross = TRUE)
get_estimates(cmp)
#> # A tibble: 4 × 9
#>   term  contrast_cyl      contrast_gear     estimate std.error statistic p.value conf.low conf.high
#>   <chr> <chr>             <chr>                <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
#> 1 cross mean(6) - mean(4) mean(4) - mean(3)    -5.33      2.77     -1.93 0.0542     -10.8  0.0953  
#> 2 cross mean(6) - mean(4) mean(5) - mean(3)    -5.16      2.63     -1.96 0.0500     -10.3  0.000165
#> 3 cross mean(8) - mean(4) mean(4) - mean(3)    -9.22      3.62     -2.55 0.0108     -16.3 -2.13    
#> 4 cross mean(8) - mean(4) mean(5) - mean(3)    -9.04      3.19     -2.84 0.00453    -15.3 -2.80
```

As we can see above, there are two relevant grouping columns:
`contrast_gear` and `contrast_cyl`. We can simply plug those names in
the `shape` argument:

``` r
modelsummary(
  cmp,
  shape = contrast_gear + contrast_cyl ~ model)
```

<div id="dqnpfrttxj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dqnpfrttxj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#dqnpfrttxj thead, #dqnpfrttxj tbody, #dqnpfrttxj tfoot, #dqnpfrttxj tr, #dqnpfrttxj td, #dqnpfrttxj th {
  border-style: none;
}

#dqnpfrttxj p {
  margin: 0;
  padding: 0;
}

#dqnpfrttxj .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#dqnpfrttxj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#dqnpfrttxj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#dqnpfrttxj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#dqnpfrttxj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dqnpfrttxj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dqnpfrttxj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dqnpfrttxj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#dqnpfrttxj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#dqnpfrttxj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dqnpfrttxj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dqnpfrttxj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#dqnpfrttxj .gt_spanner_row {
  border-bottom-style: hidden;
}

#dqnpfrttxj .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#dqnpfrttxj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#dqnpfrttxj .gt_from_md > :first-child {
  margin-top: 0;
}

#dqnpfrttxj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dqnpfrttxj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#dqnpfrttxj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#dqnpfrttxj .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#dqnpfrttxj .gt_row_group_first td {
  border-top-width: 2px;
}

#dqnpfrttxj .gt_row_group_first th {
  border-top-width: 2px;
}

#dqnpfrttxj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dqnpfrttxj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#dqnpfrttxj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#dqnpfrttxj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dqnpfrttxj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dqnpfrttxj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dqnpfrttxj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#dqnpfrttxj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dqnpfrttxj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dqnpfrttxj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dqnpfrttxj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dqnpfrttxj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dqnpfrttxj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dqnpfrttxj .gt_left {
  text-align: left;
}

#dqnpfrttxj .gt_center {
  text-align: center;
}

#dqnpfrttxj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dqnpfrttxj .gt_font_normal {
  font-weight: normal;
}

#dqnpfrttxj .gt_font_bold {
  font-weight: bold;
}

#dqnpfrttxj .gt_font_italic {
  font-style: italic;
}

#dqnpfrttxj .gt_super {
  font-size: 65%;
}

#dqnpfrttxj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#dqnpfrttxj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#dqnpfrttxj .gt_indent_1 {
  text-indent: 5px;
}

#dqnpfrttxj .gt_indent_2 {
  text-indent: 10px;
}

#dqnpfrttxj .gt_indent_3 {
  text-indent: 15px;
}

#dqnpfrttxj .gt_indent_4 {
  text-indent: 20px;
}

#dqnpfrttxj .gt_indent_5 {
  text-indent: 25px;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="header gt_col_headings">
<th id="gear" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">gear</th>
<th id="cyl" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">cyl</th>
<th id="(1)" class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">(1)</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="odd">
<td class="gt_row gt_left" headers="gear">mean(4) - mean(3)</td>
<td class="gt_row gt_left" headers="cyl">mean(6) - mean(4)</td>
<td class="gt_row gt_center" headers="(1)">-5.332</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="gear"></td>
<td class="gt_row gt_left" headers="cyl"></td>
<td class="gt_row gt_center" headers="(1)">(2.769)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="gear"></td>
<td class="gt_row gt_left" headers="cyl">mean(8) - mean(4)</td>
<td class="gt_row gt_center" headers="(1)">-9.218</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="gear"></td>
<td class="gt_row gt_left" headers="cyl"></td>
<td class="gt_row gt_center" headers="(1)">(3.618)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="gear">mean(5) - mean(3)</td>
<td class="gt_row gt_left" headers="cyl">mean(6) - mean(4)</td>
<td class="gt_row gt_center" headers="(1)">-5.156</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="gear"></td>
<td class="gt_row gt_left" headers="cyl"></td>
<td class="gt_row gt_center" headers="(1)">(2.631)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="gear"></td>
<td class="gt_row gt_left" headers="cyl">mean(8) - mean(4)</td>
<td class="gt_row gt_center" headers="(1)">-9.042</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="gear"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000"></td>
<td class="gt_row gt_left" headers="cyl"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000"></td>
<td class="gt_row gt_center" headers="(1)"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000">(3.185)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="gear">Num.Obs.</td>
<td class="gt_row gt_left" headers="cyl"></td>
<td class="gt_row gt_center" headers="(1)">32</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="gear">R2</td>
<td class="gt_row gt_left" headers="cyl"></td>
<td class="gt_row gt_center" headers="(1)">0.740</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="gear">R2 Adj.</td>
<td class="gt_row gt_left" headers="cyl"></td>
<td class="gt_row gt_center" headers="(1)">0.701</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="gear">AIC</td>
<td class="gt_row gt_left" headers="cyl"></td>
<td class="gt_row gt_center" headers="(1)">173.7</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="gear">BIC</td>
<td class="gt_row gt_left" headers="cyl"></td>
<td class="gt_row gt_center" headers="(1)">182.5</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="gear">Log.Lik.</td>
<td class="gt_row gt_left" headers="cyl"></td>
<td class="gt_row gt_center" headers="(1)">-80.838</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="gear">F</td>
<td class="gt_row gt_left" headers="cyl"></td>
<td class="gt_row gt_center" headers="(1)">19.190</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="gear">RMSE</td>
<td class="gt_row gt_left" headers="cyl"></td>
<td class="gt_row gt_center" headers="(1)">3.03</td>
</tr>
</tbody>
</table>

</div>

## Marginal means

``` r
library("marginaleffects")
library("modelsummary")

dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- lm(mpg ~ hp + cyl + am, data = dat)
mm <- marginal_means(mod)

modelsummary(mm,
             title = "Estimated Marginal Means",
             estimate = "{estimate} ({std.error}){stars}",
             statistic = NULL,
             group = term + value ~ model)
```

<div id="ybrgoxwcdd" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ybrgoxwcdd table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ybrgoxwcdd thead, #ybrgoxwcdd tbody, #ybrgoxwcdd tfoot, #ybrgoxwcdd tr, #ybrgoxwcdd td, #ybrgoxwcdd th {
  border-style: none;
}

#ybrgoxwcdd p {
  margin: 0;
  padding: 0;
}

#ybrgoxwcdd .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ybrgoxwcdd .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ybrgoxwcdd .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ybrgoxwcdd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ybrgoxwcdd .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ybrgoxwcdd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ybrgoxwcdd .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ybrgoxwcdd .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ybrgoxwcdd .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ybrgoxwcdd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ybrgoxwcdd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ybrgoxwcdd .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ybrgoxwcdd .gt_spanner_row {
  border-bottom-style: hidden;
}

#ybrgoxwcdd .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ybrgoxwcdd .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ybrgoxwcdd .gt_from_md > :first-child {
  margin-top: 0;
}

#ybrgoxwcdd .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ybrgoxwcdd .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ybrgoxwcdd .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ybrgoxwcdd .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ybrgoxwcdd .gt_row_group_first td {
  border-top-width: 2px;
}

#ybrgoxwcdd .gt_row_group_first th {
  border-top-width: 2px;
}

#ybrgoxwcdd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ybrgoxwcdd .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ybrgoxwcdd .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ybrgoxwcdd .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ybrgoxwcdd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ybrgoxwcdd .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ybrgoxwcdd .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ybrgoxwcdd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ybrgoxwcdd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ybrgoxwcdd .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ybrgoxwcdd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ybrgoxwcdd .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ybrgoxwcdd .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ybrgoxwcdd .gt_left {
  text-align: left;
}

#ybrgoxwcdd .gt_center {
  text-align: center;
}

#ybrgoxwcdd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ybrgoxwcdd .gt_font_normal {
  font-weight: normal;
}

#ybrgoxwcdd .gt_font_bold {
  font-weight: bold;
}

#ybrgoxwcdd .gt_font_italic {
  font-style: italic;
}

#ybrgoxwcdd .gt_super {
  font-size: 65%;
}

#ybrgoxwcdd .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ybrgoxwcdd .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ybrgoxwcdd .gt_indent_1 {
  text-indent: 5px;
}

#ybrgoxwcdd .gt_indent_2 {
  text-indent: 10px;
}

#ybrgoxwcdd .gt_indent_3 {
  text-indent: 15px;
}

#ybrgoxwcdd .gt_indent_4 {
  text-indent: 20px;
}

#ybrgoxwcdd .gt_indent_5 {
  text-indent: 25px;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<caption>Estimated Marginal Means</caption>
<thead>
<tr class="header gt_col_headings">
<th id=" " class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col"></th>
<th id="  " class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col"></th>
<th id="(1)" class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">(1)</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="odd">
<td class="gt_row gt_left" headers="">cyl</td>
<td class="gt_row gt_left" headers="">4</td>
<td class="gt_row gt_center" headers="(1)">22.885 (1.357)***</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="">6</td>
<td class="gt_row gt_center" headers="(1)">18.960 (1.073)***</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="">8</td>
<td class="gt_row gt_center" headers="(1)">19.351 (1.377)***</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">am</td>
<td class="gt_row gt_left" headers="">FALSE</td>
<td class="gt_row gt_center" headers="(1)">18.320 (0.785)***</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers=""
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000"></td>
<td class="gt_row gt_left" headers=""
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000">TRUE</td>
<td class="gt_row gt_center" headers="(1)"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000">22.478
(0.834)***</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">Num.Obs.</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">32</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">R2</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">0.825</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">R2 Adj.</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">0.799</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">AIC</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">161.0</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">BIC</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">169.8</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">Log.Lik.</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">-74.502</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">F</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">31.794</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">RMSE</td>
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">2.48</td>
</tr>
</tbody>
</table>

Estimated Marginal Means

</div>
