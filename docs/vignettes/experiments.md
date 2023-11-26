
# Experiments

## 2x2 Experiments

A 2×2 factorial design is a type of experimental design that allows
researchers to understand the effects of two independent variables (each
with two levels) on a single dependent variable. The design is popular
among academic researchers as well as in industry when running A/B
tests.

In this notebook, we illustrate how to analyze these designs with [the
`marginaleffects` package for `R`.](https://marginaleffects.com) As we
will see, `marginaleffects` includes many convenient functions for
analyzing both experimental and observational data, and for plotting our
results.

### Fitting a Model

We will use the `mtcars` dataset. We’ll analyze fuel efficiency, `mpg`
(miles per gallon), as a function of `am` (transmission type) and `vs`
(engine shape).

`vs` is an indicator variable for if the car has a straight engine (1 =
straight engine, 0 = V-shaped). `am` is an indicator variable for if the
car has manual transmission (1 = manual transmission, 0=automatic
transmission). There are then four types of cars (1 type for each of the
four combinations of binary indicators).

Let’s start by creating a model for fuel efficiency. For simplicity,
we’ll use linear regression and model the interaction between `vs` and
`am`.

``` r
library(tidyverse)
library(marginaleffects)
library(modelsummary)

## See ?mtcars for variable definitions
fit <- lm(mpg ~ vs + am + vs:am, data=mtcars) # equivalent to ~ vs*am
```

We can plot the predictions from the model using the `plot_predictions`
function. From the plot below, we can see a few things:

-   Straight engines (`vs=1`) are estimated to have better expected fuel
    efficiency than V-shaped engines (`vs=0`).
-   Manual transmissions (`am=1`) are estimated to have better fuel
    efficiency for both V-shaped and straight engines.
-   For straight engines, the effect of manual transmissions on fuel
    efficiency seems to increase.

``` r
plot_predictions(fit, by = c("vs", "am"))
```

<img
src="../experiments.markdown_strict_files/figure-markdown_strict/unnamed-chunk-3-1.png"
style="width:100.0%" />

### Evaluating Effects From The Model Summary

Since this model is fairly simple the estimated differences between any
of the four possible combinations of `vs` and `am` can be read from the
regression table, which we create using the `modelsummary` package:

``` r
modelsummary(fit, gof_map = c("r.squared", "nobs"))
```

<div id="tjjzrudull" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#tjjzrudull table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#tjjzrudull thead, #tjjzrudull tbody, #tjjzrudull tfoot, #tjjzrudull tr, #tjjzrudull td, #tjjzrudull th {
  border-style: none;
}

#tjjzrudull p {
  margin: 0;
  padding: 0;
}

#tjjzrudull .gt_table {
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

#tjjzrudull .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#tjjzrudull .gt_title {
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

#tjjzrudull .gt_subtitle {
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

#tjjzrudull .gt_heading {
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

#tjjzrudull .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tjjzrudull .gt_col_headings {
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

#tjjzrudull .gt_col_heading {
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

#tjjzrudull .gt_column_spanner_outer {
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

#tjjzrudull .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tjjzrudull .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tjjzrudull .gt_column_spanner {
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

#tjjzrudull .gt_spanner_row {
  border-bottom-style: hidden;
}

#tjjzrudull .gt_group_heading {
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

#tjjzrudull .gt_empty_group_heading {
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

#tjjzrudull .gt_from_md > :first-child {
  margin-top: 0;
}

#tjjzrudull .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tjjzrudull .gt_row {
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

#tjjzrudull .gt_stub {
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

#tjjzrudull .gt_stub_row_group {
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

#tjjzrudull .gt_row_group_first td {
  border-top-width: 2px;
}

#tjjzrudull .gt_row_group_first th {
  border-top-width: 2px;
}

#tjjzrudull .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tjjzrudull .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#tjjzrudull .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#tjjzrudull .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tjjzrudull .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tjjzrudull .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tjjzrudull .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#tjjzrudull .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tjjzrudull .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tjjzrudull .gt_footnotes {
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

#tjjzrudull .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tjjzrudull .gt_sourcenotes {
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

#tjjzrudull .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tjjzrudull .gt_left {
  text-align: left;
}

#tjjzrudull .gt_center {
  text-align: center;
}

#tjjzrudull .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tjjzrudull .gt_font_normal {
  font-weight: normal;
}

#tjjzrudull .gt_font_bold {
  font-weight: bold;
}

#tjjzrudull .gt_font_italic {
  font-style: italic;
}

#tjjzrudull .gt_super {
  font-size: 65%;
}

#tjjzrudull .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#tjjzrudull .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#tjjzrudull .gt_indent_1 {
  text-indent: 5px;
}

#tjjzrudull .gt_indent_2 {
  text-indent: 10px;
}

#tjjzrudull .gt_indent_3 {
  text-indent: 15px;
}

#tjjzrudull .gt_indent_4 {
  text-indent: 20px;
}

#tjjzrudull .gt_indent_5 {
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
<td class="gt_row gt_left" headers="">(Intercept)</td>
<td class="gt_row gt_center" headers="(1)">15.050</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">(1.002)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">vs</td>
<td class="gt_row gt_center" headers="(1)">5.693</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">(1.651)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">am</td>
<td class="gt_row gt_center" headers="(1)">4.700</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_center" headers="(1)">(1.736)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">vs × am</td>
<td class="gt_row gt_center" headers="(1)">2.929</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000"></td>
<td class="gt_row gt_center" headers="(1)"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000">(2.541)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">R2</td>
<td class="gt_row gt_center" headers="(1)">0.700</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">Num.Obs.</td>
<td class="gt_row gt_center" headers="(1)">32</td>
</tr>
</tbody>
</table>

</div>

We can express the same results in the form of a linear equation:

mpg = 15.050 + 5.693 ⋅ vs + 4.700 ⋅ am + 2.929 ⋅ vs ⋅ am.

With a little arithmetic, we can compute estimated differences in fuel
efficiency between different groups:

-   4.700 mpg between `am=1` and `am=0`, when `vs=0`.
-   5.693 mpg between `vs=1` and `vs=0`, when `am=0`.
-   7.629 mpg between `am=1` and `am=0`, when `vs=1`.
-   8.621 mpg between `vs=1` and `vs=0`, when `am=1`.
-   13.322 mpg between a car with `am=1` and `vs=1`, and a car with
    `am=0` and `vs=0`.

Reading off these differences from the model summary is relatively
straightforward in very simple cases like this one. However, it becomes
more difficult as more variables are added to the model, not to mention
obtaining estimated standard errors becomes nightmarish. To make the
process easier, we can leverage the `avg_comparisons()` function from
the `marginaleffects` package to compute the appropriate quantities and
standard errors.

### Using `avg_comparisons` To Estimate All Differences

The grey rectangle in the graph below is the estimated fuel efficiency
when `vs=0` and `am=0`, that is, for an automatic transmission car with
V-shaped engine.

<img
src="../experiments.markdown_strict_files/figure-markdown_strict/unnamed-chunk-5-1.png"
style="width:100.0%" />

Let’s use `avg_comparisons` to get the difference between straight
engines and V-shaped engines when the car has automatic transmission. In
this call, the `variables` argument indicates that we want to estimate
the effect of a change of 1 unit in the `vs` variable. The
`newdata=datagrid(am=0)` determines the values of the covariates at
which we want to evaluate the contrast.

``` r
avg_comparisons(fit,
  variables = "vs",
  newdata = datagrid(am = 0))
#> 
#>  Term Contrast Estimate Std. Error    z Pr(>|z|)    S 2.5 % 97.5 %
#>    vs    1 - 0     5.69       1.65 3.45   <0.001 10.8  2.46   8.93
#> 
#> Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted 
#> Type:  response
```

As expected, the results produced by `avg_comparisons()` are exactly the
same as those which we read from the model summary table. The contrast
that we just computed corresponds to the change illustrasted by the
arrow in this plot:

<img
src="../experiments.markdown_strict_files/figure-markdown_strict/unnamed-chunk-7-1.png"
style="width:100.0%" />

The next difference that we compute is between manual transmissions and
automatic transmissions when the car has a V-shaped engine. Again, the
call to `avg_comparisons` is shown below, and the corresponding contrast
is indicated in the plot below using an arrow.

``` r
avg_comparisons(fit,
  variables = "am",
  newdata = datagrid(vs = 0))
#> 
#>  Term Contrast Estimate Std. Error    z Pr(>|z|)   S 2.5 % 97.5 %
#>    am    1 - 0      4.7       1.74 2.71  0.00678 7.2   1.3    8.1
#> 
#> Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted 
#> Type:  response
```

<img
src="../experiments.markdown_strict_files/figure-markdown_strict/unnamed-chunk-9-1.png"
style="width:100.0%" />

The third difference we estimated was between manual transmissions and
automatic transmissions when the car has a straight engine. The model
call and contrast are:

``` r
avg_comparisons(fit,
  variables = "am",
  newdata = datagrid(vs = 1))
#> 
#>  Term Contrast Estimate Std. Error    z Pr(>|z|)    S 2.5 % 97.5 %
#>    am    1 - 0     7.63       1.86 4.11   <0.001 14.6  3.99   11.3
#> 
#> Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted 
#> Type:  response
```

<img
src="../experiments.markdown_strict_files/figure-markdown_strict/unnamed-chunk-11-1.png"
style="width:100.0%" />

The last difference and contrast between manual transmissions with
straight engines and automatic transmissions with V-shaped engines. We
call this a “cross-contrast” because we are measuring the difference
between two groups that differ on two explanatory variables at the same
time. To compute this contrast, we use the `cross` argument of
`avg_comparisons`:

``` r
avg_comparisons(fit,
  variables = c("am", "vs"),
  cross = TRUE)
#> 
#>  Estimate Std. Error    z Pr(>|z|)    S 2.5 % 97.5 % C: am C: vs
#>      13.3       1.65 8.07   <0.001 50.3  10.1   16.6 1 - 0 1 - 0
#> 
#> Columns: term, contrast_am, contrast_vs, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

<img
src="../experiments.markdown_strict_files/figure-markdown_strict/unnamed-chunk-13-1.png"
style="width:100.0%" />

### Conclusion

The 2x2 design is a very popular design, and when using a linear model,
the estimated differences between groups can be directly read off from
the model summary, if not with a little arithmetic. However, when using
models with a non-identity link function, or when seeking to obtain the
standard errors for estimated differences, things become considerably
more difficult. This vignette showed how to use `avg_comparisons` to
specify contrasts of interests and obtain standard errors for those
differences. The approach used applies to all generalized linear models
and effects can be further stratified using the `by` argument (although
this is not shown in this vignette.)

## Regression adjustment

Many analysts who conduct and analyze experiments wish to use regression
adjustment with a linear regression model to improve the precision of
their estimate of the treatment effect. Unfortunately, regression
adjustment can introduce small-sample bias and other undesirable
properties (Freedman 2008). Lin (2013) proposes a simple strategy to fix
these problems in sufficiently large samples:

1.  Center all predictors by subtracting each of their means.
2.  Estimate a linear model in which the treatment is interacted with
    each of the covariates.

The `estimatr` package includes a convenient function to implement this
strategy:

``` r
library(estimatr)
library(marginaleffects)
lalonde <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/MatchIt/lalonde.csv")

mod <- lm_lin(
    re78 ~ treat,
    covariates = ~ age + educ + race,
    data = lalonde,
    se_type = "HC3")
summary(mod)
#> 
#> Call:
#> lm_lin(formula = re78 ~ treat, covariates = ~age + educ + race, 
#>     data = lalonde, se_type = "HC3")
#> 
#> Standard error type:  HC3 
#> 
#> Coefficients:
#>                    Estimate Std. Error t value  Pr(>|t|) CI Lower CI Upper  DF
#> (Intercept)         6488.05     356.71 18.1885 2.809e-59  5787.50   7188.6 604
#> treat                489.73     878.52  0.5574 5.774e-01 -1235.59   2215.0 604
#> age_c                 85.88      35.42  2.4248 1.561e-02    16.32    155.4 604
#> educ_c               464.04     131.51  3.5286 4.495e-04   205.77    722.3 604
#> racehispan_c        2775.47    1155.40  2.4022 1.660e-02   506.38   5044.6 604
#> racewhite_c         2291.67     793.30  2.8888 4.006e-03   733.71   3849.6 604
#> treat:age_c           17.23      76.37  0.2256 8.216e-01  -132.75    167.2 604
#> treat:educ_c         226.71     308.43  0.7350 4.626e-01  -379.02    832.4 604
#> treat:racehispan_c -1057.84    2652.42 -0.3988 6.902e-01 -6266.92   4151.2 604
#> treat:racewhite_c  -1205.68    1805.21 -0.6679 5.045e-01 -4750.92   2339.6 604
#> 
#> Multiple R-squared:  0.05722 ,   Adjusted R-squared:  0.04317 
#> F-statistic: 4.238 on 9 and 604 DF,  p-value: 2.424e-05
```

We can obtain the same results by fitting a model with the standard `lm`
function and using the `comparisons()` function:

``` r
mod <- lm(re78 ~ treat * (age + educ + race), data = lalonde)
avg_comparisons(
    mod,
    variables = "treat",
    vcov = "HC3")
#> 
#>   Term Contrast Estimate Std. Error     z Pr(>|z|)   S 2.5 % 97.5 %
#>  treat    1 - 0      490        879 0.557    0.577 0.8 -1232   2212
#> 
#> Columns: term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

Notice that the `treat` coefficient and associate standard error in the
`lm_lin` regression are exactly the same as the estimates produced by
the `comparisons()` function.

### References

-   Freedman, David A. “On Regression Adjustments to Experimental Data.”
    Advances in Applied Mathematics 40, no. 2 (February 2008): 180–93.
-   Lin, Winston. “Agnostic Notes on Regression Adjustments to
    Experimental Data: Reexamining Freedman’s Critique.” Annals of
    Applied Statistics 7, no. 1 (March 2013): 295–318.
    https://doi.org/10.1214/12-AOAS583.
