
# Logit

This vignette replicates some of the analyses in this excellent blog
post by Frank Harrell: [Avoiding One-Number Summaries of Treatment
Effects for RCTs with Binary
Outcomes](https://www.fharrell.com/post/rdist/). Here, we show how
one-number summaries and the entire distribution unit-level contrasts
can be easily computed with `comparisons()`.

Dr. Harrell discusses summaries from logistic regression models in the
blog post above. He focuses on a context in which one is interested in
comparing two groups, such as in randomized controlled trials. He
highlights potential pitfalls of presenting “one-number summaries”,
e.g., odds ratio and mean proportion difference. Finally, he recommends
focusing on the entire distribution of proportion difference between
groups.

For clarification, we use the following terms interchangeably in the
context of logistic regression where the covariate of interest is
categorical:

-   Contrast
-   Proportion difference
-   Risk difference
-   Absolute risk reduction

## Data

We focus on subset data from the [GUSTO-I
study](https://www.nejm.org/doi/full/10.1056/NEJM199309023291001), where
patients were randomly assigned to accelerated tissue plasminogen
activator (tPA) or streptokinase (SK).

Load libraries, data and fit a covariate-adjusted logistic regression
model.

``` r
library(marginaleffects)
library(modelsummary)
library(ggplot2)
library(rms)

load(url(
"https://github.com/vincentarelbundock/modelarchive/raw/main/data-raw/gusto.rda"
))

gusto <- subset(gusto, tx %in% c("tPA", "SK"))
gusto$tx <- factor(gusto$tx, levels = c("tPA", "SK"))

mod <- glm(
    day30 ~ tx + rcs(age, 4) + Killip + pmin(sysbp, 120) + lsp(pulse, 50) +
    pmi + miloc + sex, family = "binomial",
    data = gusto)
```

### One-Number Summaries

As usual, we can produce a one-number summary of the relationship of
interest by exponentiating the coefficients, which yields an Odds Ratio
(OR):

``` r
modelsummary(mod, exponentiate = TRUE, coef_omit = "^(?!txSK)") 
```

<div id="ycsbdhmvsm" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ycsbdhmvsm table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ycsbdhmvsm thead, #ycsbdhmvsm tbody, #ycsbdhmvsm tfoot, #ycsbdhmvsm tr, #ycsbdhmvsm td, #ycsbdhmvsm th {
  border-style: none;
}

#ycsbdhmvsm p {
  margin: 0;
  padding: 0;
}

#ycsbdhmvsm .gt_table {
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

#ycsbdhmvsm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ycsbdhmvsm .gt_title {
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

#ycsbdhmvsm .gt_subtitle {
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

#ycsbdhmvsm .gt_heading {
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

#ycsbdhmvsm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ycsbdhmvsm .gt_col_headings {
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

#ycsbdhmvsm .gt_col_heading {
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

#ycsbdhmvsm .gt_column_spanner_outer {
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

#ycsbdhmvsm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ycsbdhmvsm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ycsbdhmvsm .gt_column_spanner {
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

#ycsbdhmvsm .gt_spanner_row {
  border-bottom-style: hidden;
}

#ycsbdhmvsm .gt_group_heading {
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

#ycsbdhmvsm .gt_empty_group_heading {
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

#ycsbdhmvsm .gt_from_md > :first-child {
  margin-top: 0;
}

#ycsbdhmvsm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ycsbdhmvsm .gt_row {
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

#ycsbdhmvsm .gt_stub {
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

#ycsbdhmvsm .gt_stub_row_group {
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

#ycsbdhmvsm .gt_row_group_first td {
  border-top-width: 2px;
}

#ycsbdhmvsm .gt_row_group_first th {
  border-top-width: 2px;
}

#ycsbdhmvsm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ycsbdhmvsm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ycsbdhmvsm .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ycsbdhmvsm .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ycsbdhmvsm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ycsbdhmvsm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ycsbdhmvsm .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ycsbdhmvsm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ycsbdhmvsm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ycsbdhmvsm .gt_footnotes {
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

#ycsbdhmvsm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ycsbdhmvsm .gt_sourcenotes {
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

#ycsbdhmvsm .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ycsbdhmvsm .gt_left {
  text-align: left;
}

#ycsbdhmvsm .gt_center {
  text-align: center;
}

#ycsbdhmvsm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ycsbdhmvsm .gt_font_normal {
  font-weight: normal;
}

#ycsbdhmvsm .gt_font_bold {
  font-weight: bold;
}

#ycsbdhmvsm .gt_font_italic {
  font-style: italic;
}

#ycsbdhmvsm .gt_super {
  font-size: 65%;
}

#ycsbdhmvsm .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ycsbdhmvsm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ycsbdhmvsm .gt_indent_1 {
  text-indent: 5px;
}

#ycsbdhmvsm .gt_indent_2 {
  text-indent: 10px;
}

#ycsbdhmvsm .gt_indent_3 {
  text-indent: 15px;
}

#ycsbdhmvsm .gt_indent_4 {
  text-indent: 20px;
}

#ycsbdhmvsm .gt_indent_5 {
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
<td class="gt_row gt_left" headers="">txSK</td>
<td class="gt_row gt_center" headers="(1)">1.230</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000"></td>
<td class="gt_row gt_center" headers="(1)"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000">(0.065)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">Num.Obs.</td>
<td class="gt_row gt_center" headers="(1)">30510</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">AIC</td>
<td class="gt_row gt_center" headers="(1)">12428.6</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">BIC</td>
<td class="gt_row gt_center" headers="(1)">12553.5</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">Log.Lik.</td>
<td class="gt_row gt_center" headers="(1)">-6199.317</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">F</td>
<td class="gt_row gt_center" headers="(1)">173.216</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">RMSE</td>
<td class="gt_row gt_center" headers="(1)">0.24</td>
</tr>
</tbody>
</table>

</div>

Unlike ORs, adjusted risk differences vary from individual to individual
based on the values of the control variables. The `comparisons()`
function can compute adjusted risk differences for every individual.
Here, we display only the first 6 of them:

``` r
comparisons(
    mod,
    variables = "tx")
#> 
#>  Term Contrast Estimate Std. Error    z Pr(>|z|)   S    2.5 %  97.5 %
#>    tx SK - tPA 0.001074   0.000497 2.16  0.03060 5.0 0.000100 0.00205
#>    tx SK - tPA 0.000857   0.000380 2.26  0.02410 5.4 0.000112 0.00160
#>    tx SK - tPA 0.001780   0.000779 2.29  0.02229 5.5 0.000253 0.00331
#>    tx SK - tPA 0.001137   0.000500 2.27  0.02302 5.4 0.000157 0.00212
#>    tx SK - tPA 0.001366   0.000594 2.30  0.02143 5.5 0.000202 0.00253
#> --- 30500 rows omitted. See ?avg_comparisons and ?print.marginaleffects --- 
#>    tx SK - tPA 0.002429   0.000808 3.00  0.00266  8.6 0.000844 0.00401
#>    tx SK - tPA 0.012130   0.003900 3.11  0.00187  9.1 0.004486 0.01977
#>    tx SK - tPA 0.036812   0.010361 3.55  < 0.001 11.4 0.016505 0.05712
#>    tx SK - tPA 0.022969   0.006975 3.29  < 0.001 10.0 0.009298 0.03664
#>    tx SK - tPA 0.049707   0.012843 3.87  < 0.001 13.2 0.024535 0.07488
#> Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted, day30, tx, age, Killip, sysbp, pulse, pmi, miloc, sex 
#> Type:  response
```

Population-averaged (aka “marginal”) adjusted risk difference ([see this
vignette](gcomputation.html)) can be obtained using the `avg_*()`
functions or using the `by` argument:

``` r
avg_comparisons(mod, variables = "tx")
#> 
#>  Term Contrast Estimate Std. Error    z Pr(>|z|)    S   2.5 % 97.5 %
#>    tx SK - tPA   0.0111    0.00277 4.01   <0.001 14.0 0.00566 0.0165
#> 
#> Columns: term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```

The `comparisons()` function above computed the predicted probability of
mortality (`day30==1`) for each observed row of the data in two
counterfactual cases: when `tx` is “SK”, and when `tx` is “tPA”. Then,
it computed the differences between these two sets of predictions.
Finally, it computed the population-average of risk differences.

Instead of risk *differences*, we could compute population-averaged
(marginal) adjusted risk *ratios*:

``` r
avg_comparisons(
    mod,
    variables = "tx",
    comparison = "lnratioavg",
    transform = exp)
#> 
#>  Term                 Contrast Estimate Pr(>|z|)    S 2.5 % 97.5 %
#>    tx ln(mean(SK) / mean(tPA))     1.18   <0.001 13.3  1.08   1.28
#> 
#> Columns: term, contrast, estimate, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted 
#> Type:  response
```

Population-averaged (marginal) odds ratios:

``` r
avg_comparisons(
    mod,
    variables = "tx",
    comparison = "lnoravg",
    transform = "exp")
#> 
#>  Term                 Contrast Estimate Pr(>|z|)    S 2.5 % 97.5 %
#>    tx ln(odds(SK) / odds(tPA))     1.19   <0.001 13.4  1.09    1.3
#> 
#> Columns: term, contrast, estimate, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted 
#> Type:  response
```

### Unit-level Summaries

Instead of estimating one-number summaries, we can focus on unit-level
proportion differences using `comparisons()`. This function applies the
fitted logistic regression model to predict outcome probabilities for
each patient, i.e., unit-level.

``` r
cmp <- comparisons(mod, variables = "tx")
cmp
#> 
#>  Term Contrast Estimate Std. Error    z Pr(>|z|)   S    2.5 %  97.5 %
#>    tx SK - tPA 0.001074   0.000497 2.16  0.03060 5.0 0.000100 0.00205
#>    tx SK - tPA 0.000857   0.000380 2.26  0.02410 5.4 0.000112 0.00160
#>    tx SK - tPA 0.001780   0.000779 2.29  0.02229 5.5 0.000253 0.00331
#>    tx SK - tPA 0.001137   0.000500 2.27  0.02302 5.4 0.000157 0.00212
#>    tx SK - tPA 0.001366   0.000594 2.30  0.02143 5.5 0.000202 0.00253
#> --- 30500 rows omitted. See ?avg_comparisons and ?print.marginaleffects --- 
#>    tx SK - tPA 0.002429   0.000808 3.00  0.00266  8.6 0.000844 0.00401
#>    tx SK - tPA 0.012130   0.003900 3.11  0.00187  9.1 0.004486 0.01977
#>    tx SK - tPA 0.036812   0.010361 3.55  < 0.001 11.4 0.016505 0.05712
#>    tx SK - tPA 0.022969   0.006975 3.29  < 0.001 10.0 0.009298 0.03664
#>    tx SK - tPA 0.049707   0.012843 3.87  < 0.001 13.2 0.024535 0.07488
#> Columns: rowid, term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, predicted_lo, predicted_hi, predicted, day30, tx, age, Killip, sysbp, pulse, pmi, miloc, sex 
#> Type:  response
```

Show the predicted probability for individual patients under both
treatment alternatives.

``` r
ggplot(cmp, aes(predicted_hi, predicted_lo)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  coord_fixed() +
  labs(x = "SK", y = "tPA")
```

<img
src="../logit.markdown_strict_files/figure-markdown_strict/unnamed-chunk-9-1.png"
style="width:100.0%" />

We can present the entire distribution of unit-level proportion
differences an a cumulative distribution function:

``` r
ggplot(cmp, aes(estimate)) + stat_ecdf()
```

<img
src="../logit.markdown_strict_files/figure-markdown_strict/unnamed-chunk-10-1.png"
style="width:100.0%" />

Or the same information as a histogram with the mean and median.

``` r
ggplot(cmp, aes(estimate)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = mean(cmp$estimate), color = "orange") +
  geom_vline(xintercept = median(cmp$estimate), color = "darkgreen") +
  labs(x = "SK - TPA", title = "Distribution of unit-level contrasts")
```

<img
src="../logit.markdown_strict_files/figure-markdown_strict/unnamed-chunk-11-1.png"
style="width:100.0%" />

### Appendix

`comparisons()` performed the following calculations under the hood:

``` r
d  <- gusto

d$tx = "SK"
predicted_hi <- predict(mod, newdata = d, type = "response")

d$tx = "tPA"
predicted_lo <- predict(mod, newdata = d, type = "response")

comparison <- predicted_hi - predicted_lo
```

The original dataset contains 30510 patients, thus `comparisons()`
generates an output with same amount of rows.

``` r
nrow(gusto)
#> [1] 30510
```

``` r
nrow(cmp)
#> [1] 30510
```
