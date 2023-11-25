
# Missing Data

The `marginaleffects` package offers convenience functions to compute
and display predictions, contrasts, and marginal effects from models
with multiple imputation from the `mice` and `Amelia` packages. The
workflow follows Rubin’s rules (Rubin, 1987, p. 76), via the following
steps:

1.  Impute *M* data sets.
2.  Fit a model in each of the *M* imputed data sets.
3.  Compute marginal effects in each of the *M* data sets.
4.  Pool results.

To highlight the workflow, we consider a simple linear regression model,
although the same workflow should work with any model type that is fit
using a formula interface and a `data` argument.

`marginaleffects` directly supports the `mice` and `Amelia` imputation
packages, as well as any other package that can return a list of imputed
data frames. This is demonstrated below using the `iris` dataset, in
which we insert missing observations randomly and then impute missing
values using several packages.

``` r
library(marginaleffects)
set.seed(1024)

dat <- iris
dat$Sepal.Length[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Sepal.Width[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Species[sample(seq_len(nrow(iris)), 40)] <- NA
```

## `mice`

First, we impute the dataset using the `mice` package:

``` r
library(mice)

dat_mice <- mice(dat, m = 20, printFlag = FALSE, .Random.seed = 1024)
```

Then, we use the standard `mice` syntax to produce an object of class
`mira` with all the models:

``` r
mod_mice <- with(dat_mice, lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species))
```

Finally, we feed the `mira` object to a `marginaleffects` function:

``` r
mfx_mice <- avg_slopes(mod_mice, by = "Species")
mfx_mice
#> 
#>          Term                        Contrast    Species Estimate Std. Error      t Pr(>|t|)     S   2.5 % 97.5 %    Df
#>  Sepal.Length mean(dY/dX)                     setosa       0.0684     0.0560  1.222  0.22413   2.2 -0.0424  0.179 120.0
#>  Sepal.Length mean(dY/dX)                     versicolor   0.0540     0.0557  0.968  0.33543   1.6 -0.0567  0.165  93.5
#>  Sepal.Length mean(dY/dX)                     virginica    0.0582     0.0512  1.137  0.25816   2.0 -0.0433  0.160 101.2
#>  Sepal.Width  mean(dY/dX)                     setosa       0.1890     0.0836  2.260  0.02435   5.4  0.0246  0.353 400.4
#>  Sepal.Width  mean(dY/dX)                     versicolor   0.2092     0.0772  2.710  0.00807   7.0  0.0558  0.363  89.0
#>  Sepal.Width  mean(dY/dX)                     virginica    0.2242     0.1041  2.154  0.03511   4.8  0.0162  0.432  61.9
#>  Species      mean(versicolor) - mean(setosa) setosa       1.1399     0.0977 11.668  < 0.001  68.1  0.9464  1.333 114.8
#>  Species      mean(versicolor) - mean(setosa) versicolor   1.1399     0.0977 11.668  < 0.001  68.1  0.9464  1.333 114.8
#>  Species      mean(versicolor) - mean(setosa) virginica    1.1399     0.0977 11.668  < 0.001  68.1  0.9464  1.333 114.8
#>  Species      mean(virginica) - mean(setosa)  setosa       1.7408     0.1108 15.709  < 0.001 100.7  1.5214  1.960 121.6
#>  Species      mean(virginica) - mean(setosa)  versicolor   1.7408     0.1108 15.709  < 0.001 100.7  1.5214  1.960 121.6
#>  Species      mean(virginica) - mean(setosa)  virginica    1.7408     0.1108 15.709  < 0.001 100.7  1.5214  1.960 121.6
#> 
#> Columns: term, contrast, Species, estimate, std.error, s.value, predicted_lo, predicted_hi, predicted, df, statistic, p.value, conf.low, conf.high 
#> Type:  response
```

## `Amelia`

With `Amelia`, the workflow is essentially the same. First, we impute
using `Amelia`:

``` r
library(Amelia)

dat_amelia <- amelia(dat, m = 20, noms = "Species", p2s = 0)
```

Then, we use `Amelia` syntax to produce an object of class `amest` with
all the models:

``` r
mod_amelia <- with(dat_amelia, lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species))
```

Finally, we feed the `amest` object to a `marginaleffects` function:

``` r
mfx_amelia <- avg_slopes(mod_amelia, by = "Species")
mfx_amelia
#> 
#>          Term                        Contrast    Species Estimate Std. Error      t Pr(>|t|)    S  2.5 % 97.5 %   Df
#>  Sepal.Length mean(dY/dX)                     setosa       0.3878     0.0907  4.278  < 0.001 13.3  0.205 0.5705 44.0
#>  Sepal.Length mean(dY/dX)                     versicolor   0.3231     0.0802  4.030  < 0.001 12.5  0.163 0.4838 55.9
#>  Sepal.Length mean(dY/dX)                     virginica    0.3467     0.0799  4.340  < 0.001 13.6  0.186 0.5077 44.7
#>  Sepal.Width  mean(dY/dX)                     setosa      -0.2079     0.1491 -1.395  0.16877  2.6 -0.507 0.0909 55.0
#>  Sepal.Width  mean(dY/dX)                     versicolor  -0.1157     0.1168 -0.991  0.32646  1.6 -0.350 0.1187 51.8
#>  Sepal.Width  mean(dY/dX)                     virginica   -0.0452     0.1272 -0.355  0.72320  0.5 -0.298 0.2078 82.1
#>  Species      mean(versicolor) - mean(setosa) setosa       0.6127     0.1731  3.541  0.00111  9.8  0.262 0.9635 36.7
#>  Species      mean(versicolor) - mean(setosa) versicolor   0.6127     0.1731  3.541  0.00111  9.8  0.262 0.9635 36.7
#>  Species      mean(versicolor) - mean(setosa) virginica    0.6127     0.1731  3.541  0.00111  9.8  0.262 0.9635 36.7
#>  Species      mean(virginica) - mean(setosa)  setosa       1.0364     0.2004  5.171  < 0.001 16.6  0.629 1.4436 34.2
#>  Species      mean(virginica) - mean(setosa)  versicolor   1.0364     0.2004  5.171  < 0.001 16.6  0.629 1.4436 34.2
#>  Species      mean(virginica) - mean(setosa)  virginica    1.0364     0.2004  5.171  < 0.001 16.6  0.629 1.4436 34.2
#> 
#> Columns: term, contrast, Species, estimate, std.error, s.value, predicted_lo, predicted_hi, predicted, df, statistic, p.value, conf.low, conf.high 
#> Type:  response
```

## Other imputation packages: `missRanger`, or lists of imputed data frames.

Several `R` packages can impute missing data. Indeed, [the
`Missing Data CRAN View`](https://cran.r-project.org/web/views/MissingData.html)
lists at least a dozen alternatives. Since user interfaces change a lot
from package to package, `marginaleffects` supports a single workflow
that can be used, with some adaptation, with all imputation packages:

1.  Use an external package to create a list of imputed data frames.
2.  Apply the `datalist2mids()` function from the `miceadds` package to
    convert the list of imputed data frames to a `mids` object.
3.  Use the `with()` function to fit models to create `mira` object, as
    illustrated in the `mice` and `Amelia` sections above.
4.  Pass the `mira` object to a `marginaleffects` function.

Consider the imputation package `missRanger`, which generates a list of
imputed datasets:

``` r
library(miceadds)
library(missRanger)

## convert lists of imputed datasets to `mids` objects
dat_missRanger <- replicate(20, missRanger(dat, verbose = 0), simplify = FALSE)
mids_missRanger <- datlist2mids(dat_missRanger)

## fit models
mod_missRanger <- with(mids_missRanger, lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species))

## `missRanger` slopes
mfx_missRanger <- avg_slopes(mod_missRanger, by = "Species")
mfx_missRanger
#> 
#>          Term                        Contrast    Species Estimate Std. Error     t Pr(>|t|)     S    2.5 % 97.5 %      Df
#>  Sepal.Length mean(dY/dX)                     setosa       0.0586     0.0414  1.42  0.15671   2.7 -0.02249  0.140 2780641
#>  Sepal.Length mean(dY/dX)                     versicolor   0.0675     0.0392  1.72  0.08516   3.6 -0.00935  0.144  721514
#>  Sepal.Length mean(dY/dX)                     virginica    0.0643     0.0367  1.75  0.07986   3.6 -0.00766  0.136 1020580
#>  Sepal.Width  mean(dY/dX)                     setosa       0.2314     0.0690  3.35  < 0.001  10.3  0.09613  0.367 1911278
#>  Sepal.Width  mean(dY/dX)                     versicolor   0.2186     0.0551  3.97  < 0.001  13.8  0.11066  0.327  246676
#>  Sepal.Width  mean(dY/dX)                     virginica    0.2089     0.0687  3.04  0.00237   8.7  0.07419  0.344  194958
#>  Species      mean(versicolor) - mean(setosa) setosa       1.1589     0.0704 16.46  < 0.001 199.8  1.02091  1.297 1115135
#>  Species      mean(versicolor) - mean(setosa) versicolor   1.1589     0.0704 16.46  < 0.001 199.8  1.02091  1.297 1115135
#>  Species      mean(versicolor) - mean(setosa) virginica    1.1589     0.0704 16.46  < 0.001 199.8  1.02091  1.297 1115135
#>  Species      mean(virginica) - mean(setosa)  setosa       1.7781     0.0822 21.64  < 0.001 342.5  1.61703  1.939 1547086
#>  Species      mean(virginica) - mean(setosa)  versicolor   1.7781     0.0822 21.64  < 0.001 342.5  1.61703  1.939 1547086
#>  Species      mean(virginica) - mean(setosa)  virginica    1.7781     0.0822 21.64  < 0.001 342.5  1.61703  1.939 1547086
#> 
#> Columns: term, contrast, Species, estimate, std.error, s.value, predicted_lo, predicted_hi, predicted, df, statistic, p.value, conf.low, conf.high 
#> Type:  response
```

## Comparing results with different imputation software

We can use the `modelsummary` package to compare the results with
listwise deletion to the results using different imputations software:

``` r
library(modelsummary)

## listwise deletion slopes
mod_lwd <- lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species, data = dat)
mfx_lwd <- avg_slopes(mod_lwd, by = "Species")

## regression table
models <- list(
    "LWD" = mfx_lwd,
    "mice" = mfx_mice,
    "Amelia" = mfx_amelia,
    "missRanger" = mfx_missRanger)
modelsummary(models, shape = term : contrast + Species ~ model)
```

<div id="edltsovpql" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#edltsovpql table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#edltsovpql thead, #edltsovpql tbody, #edltsovpql tfoot, #edltsovpql tr, #edltsovpql td, #edltsovpql th {
  border-style: none;
}

#edltsovpql p {
  margin: 0;
  padding: 0;
}

#edltsovpql .gt_table {
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

#edltsovpql .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#edltsovpql .gt_title {
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

#edltsovpql .gt_subtitle {
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

#edltsovpql .gt_heading {
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

#edltsovpql .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#edltsovpql .gt_col_headings {
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

#edltsovpql .gt_col_heading {
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

#edltsovpql .gt_column_spanner_outer {
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

#edltsovpql .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#edltsovpql .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#edltsovpql .gt_column_spanner {
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

#edltsovpql .gt_spanner_row {
  border-bottom-style: hidden;
}

#edltsovpql .gt_group_heading {
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

#edltsovpql .gt_empty_group_heading {
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

#edltsovpql .gt_from_md > :first-child {
  margin-top: 0;
}

#edltsovpql .gt_from_md > :last-child {
  margin-bottom: 0;
}

#edltsovpql .gt_row {
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

#edltsovpql .gt_stub {
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

#edltsovpql .gt_stub_row_group {
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

#edltsovpql .gt_row_group_first td {
  border-top-width: 2px;
}

#edltsovpql .gt_row_group_first th {
  border-top-width: 2px;
}

#edltsovpql .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#edltsovpql .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#edltsovpql .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#edltsovpql .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#edltsovpql .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#edltsovpql .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#edltsovpql .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#edltsovpql .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#edltsovpql .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#edltsovpql .gt_footnotes {
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

#edltsovpql .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#edltsovpql .gt_sourcenotes {
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

#edltsovpql .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#edltsovpql .gt_left {
  text-align: left;
}

#edltsovpql .gt_center {
  text-align: center;
}

#edltsovpql .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#edltsovpql .gt_font_normal {
  font-weight: normal;
}

#edltsovpql .gt_font_bold {
  font-weight: bold;
}

#edltsovpql .gt_font_italic {
  font-style: italic;
}

#edltsovpql .gt_super {
  font-size: 65%;
}

#edltsovpql .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#edltsovpql .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#edltsovpql .gt_indent_1 {
  text-indent: 5px;
}

#edltsovpql .gt_indent_2 {
  text-indent: 10px;
}

#edltsovpql .gt_indent_3 {
  text-indent: 15px;
}

#edltsovpql .gt_indent_4 {
  text-indent: 20px;
}

#edltsovpql .gt_indent_5 {
  text-indent: 25px;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="header gt_col_headings">
<th id=" " class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col"></th>
<th id="Species" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Species</th>
<th id="LWD" class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">LWD</th>
<th id="mice" class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">mice</th>
<th id="Amelia"
class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">Amelia</th>
<th id="missRanger"
class="gt_col_heading gt_columns_bottom_border gt_center"
data-quarto-table-cell-role="th" scope="col">missRanger</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="odd">
<td class="gt_row gt_left" headers="">Sepal.Length mean(dY/dX)</td>
<td class="gt_row gt_left" headers="Species">setosa</td>
<td class="gt_row gt_center" headers="LWD">0.033</td>
<td class="gt_row gt_center" headers="mice">0.068</td>
<td class="gt_row gt_center" headers="Amelia">0.388</td>
<td class="gt_row gt_center" headers="missRanger">0.059</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">(0.061)</td>
<td class="gt_row gt_center" headers="mice">(0.056)</td>
<td class="gt_row gt_center" headers="Amelia">(0.091)</td>
<td class="gt_row gt_center" headers="missRanger">(0.041)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species">versicolor</td>
<td class="gt_row gt_center" headers="LWD">0.050</td>
<td class="gt_row gt_center" headers="mice">0.054</td>
<td class="gt_row gt_center" headers="Amelia">0.323</td>
<td class="gt_row gt_center" headers="missRanger">0.067</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">(0.061)</td>
<td class="gt_row gt_center" headers="mice">(0.056)</td>
<td class="gt_row gt_center" headers="Amelia">(0.080)</td>
<td class="gt_row gt_center" headers="missRanger">(0.039)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species">virginica</td>
<td class="gt_row gt_center" headers="LWD">0.043</td>
<td class="gt_row gt_center" headers="mice">0.058</td>
<td class="gt_row gt_center" headers="Amelia">0.347</td>
<td class="gt_row gt_center" headers="missRanger">0.064</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">(0.058)</td>
<td class="gt_row gt_center" headers="mice">(0.051)</td>
<td class="gt_row gt_center" headers="Amelia">(0.080)</td>
<td class="gt_row gt_center" headers="missRanger">(0.037)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">Sepal.Width mean(dY/dX)</td>
<td class="gt_row gt_left" headers="Species">setosa</td>
<td class="gt_row gt_center" headers="LWD">0.274</td>
<td class="gt_row gt_center" headers="mice">0.189</td>
<td class="gt_row gt_center" headers="Amelia">-0.208</td>
<td class="gt_row gt_center" headers="missRanger">0.231</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">(0.091)</td>
<td class="gt_row gt_center" headers="mice">(0.084)</td>
<td class="gt_row gt_center" headers="Amelia">(0.149)</td>
<td class="gt_row gt_center" headers="missRanger">(0.069)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species">versicolor</td>
<td class="gt_row gt_center" headers="LWD">0.255</td>
<td class="gt_row gt_center" headers="mice">0.209</td>
<td class="gt_row gt_center" headers="Amelia">-0.116</td>
<td class="gt_row gt_center" headers="missRanger">0.219</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">(0.074)</td>
<td class="gt_row gt_center" headers="mice">(0.077)</td>
<td class="gt_row gt_center" headers="Amelia">(0.117)</td>
<td class="gt_row gt_center" headers="missRanger">(0.055)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species">virginica</td>
<td class="gt_row gt_center" headers="LWD">0.234</td>
<td class="gt_row gt_center" headers="mice">0.224</td>
<td class="gt_row gt_center" headers="Amelia">-0.045</td>
<td class="gt_row gt_center" headers="missRanger">0.209</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">(0.083)</td>
<td class="gt_row gt_center" headers="mice">(0.104)</td>
<td class="gt_row gt_center" headers="Amelia">(0.127)</td>
<td class="gt_row gt_center" headers="missRanger">(0.069)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">Species mean(versicolor) -
mean(setosa)</td>
<td class="gt_row gt_left" headers="Species">setosa</td>
<td class="gt_row gt_center" headers="LWD">1.157</td>
<td class="gt_row gt_center" headers="mice">1.140</td>
<td class="gt_row gt_center" headers="Amelia">0.613</td>
<td class="gt_row gt_center" headers="missRanger">1.159</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">(0.097)</td>
<td class="gt_row gt_center" headers="mice">(0.098)</td>
<td class="gt_row gt_center" headers="Amelia">(0.173)</td>
<td class="gt_row gt_center" headers="missRanger">(0.070)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species">versicolor</td>
<td class="gt_row gt_center" headers="LWD">1.157</td>
<td class="gt_row gt_center" headers="mice">1.140</td>
<td class="gt_row gt_center" headers="Amelia">0.613</td>
<td class="gt_row gt_center" headers="missRanger">1.159</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">(0.097)</td>
<td class="gt_row gt_center" headers="mice">(0.098)</td>
<td class="gt_row gt_center" headers="Amelia">(0.173)</td>
<td class="gt_row gt_center" headers="missRanger">(0.070)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species">virginica</td>
<td class="gt_row gt_center" headers="LWD">1.157</td>
<td class="gt_row gt_center" headers="mice">1.140</td>
<td class="gt_row gt_center" headers="Amelia">0.613</td>
<td class="gt_row gt_center" headers="missRanger">1.159</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">(0.097)</td>
<td class="gt_row gt_center" headers="mice">(0.098)</td>
<td class="gt_row gt_center" headers="Amelia">(0.173)</td>
<td class="gt_row gt_center" headers="missRanger">(0.070)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">Species mean(virginica) -
mean(setosa)</td>
<td class="gt_row gt_left" headers="Species">setosa</td>
<td class="gt_row gt_center" headers="LWD">1.839</td>
<td class="gt_row gt_center" headers="mice">1.741</td>
<td class="gt_row gt_center" headers="Amelia">1.036</td>
<td class="gt_row gt_center" headers="missRanger">1.778</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">(0.123)</td>
<td class="gt_row gt_center" headers="mice">(0.111)</td>
<td class="gt_row gt_center" headers="Amelia">(0.200)</td>
<td class="gt_row gt_center" headers="missRanger">(0.082)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species">versicolor</td>
<td class="gt_row gt_center" headers="LWD">1.839</td>
<td class="gt_row gt_center" headers="mice">1.741</td>
<td class="gt_row gt_center" headers="Amelia">1.036</td>
<td class="gt_row gt_center" headers="missRanger">1.778</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">(0.123)</td>
<td class="gt_row gt_center" headers="mice">(0.111)</td>
<td class="gt_row gt_center" headers="Amelia">(0.200)</td>
<td class="gt_row gt_center" headers="missRanger">(0.082)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers=""></td>
<td class="gt_row gt_left" headers="Species">virginica</td>
<td class="gt_row gt_center" headers="LWD">1.839</td>
<td class="gt_row gt_center" headers="mice">1.741</td>
<td class="gt_row gt_center" headers="Amelia">1.036</td>
<td class="gt_row gt_center" headers="missRanger">1.778</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers=""
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000"></td>
<td class="gt_row gt_left" headers="Species"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000"></td>
<td class="gt_row gt_center" headers="LWD"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000">(0.123)</td>
<td class="gt_row gt_center" headers="mice"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000">(0.111)</td>
<td class="gt_row gt_center" headers="Amelia"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000">(0.200)</td>
<td class="gt_row gt_center" headers="missRanger"
style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #000000">(0.082)</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">Num.Obs.</td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">60</td>
<td class="gt_row gt_center" headers="mice">150</td>
<td class="gt_row gt_center" headers="Amelia">150</td>
<td class="gt_row gt_center" headers="missRanger">150</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">Num.Imp.</td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD"></td>
<td class="gt_row gt_center" headers="mice">20</td>
<td class="gt_row gt_center" headers="Amelia">20</td>
<td class="gt_row gt_center" headers="missRanger">20</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">R2</td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">0.953</td>
<td class="gt_row gt_center" headers="mice">0.930</td>
<td class="gt_row gt_center" headers="Amelia">0.853</td>
<td class="gt_row gt_center" headers="missRanger">0.947</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">R2 Adj.</td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">0.949</td>
<td class="gt_row gt_center" headers="mice">0.928</td>
<td class="gt_row gt_center" headers="Amelia">0.848</td>
<td class="gt_row gt_center" headers="missRanger">0.945</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">AIC</td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">-34.0</td>
<td class="gt_row gt_center" headers="mice"></td>
<td class="gt_row gt_center" headers="Amelia"></td>
<td class="gt_row gt_center" headers="missRanger"></td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">BIC</td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">-19.3</td>
<td class="gt_row gt_center" headers="mice"></td>
<td class="gt_row gt_center" headers="Amelia"></td>
<td class="gt_row gt_center" headers="missRanger"></td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">Log.Lik.</td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">23.997</td>
<td class="gt_row gt_center" headers="mice"></td>
<td class="gt_row gt_center" headers="Amelia"></td>
<td class="gt_row gt_center" headers="missRanger"></td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="">F</td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">220.780</td>
<td class="gt_row gt_center" headers="mice"></td>
<td class="gt_row gt_center" headers="Amelia"></td>
<td class="gt_row gt_center" headers="missRanger"></td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="">RMSE</td>
<td class="gt_row gt_left" headers="Species"></td>
<td class="gt_row gt_center" headers="LWD">0.16</td>
<td class="gt_row gt_center" headers="mice"></td>
<td class="gt_row gt_center" headers="Amelia"></td>
<td class="gt_row gt_center" headers="missRanger"></td>
</tr>
</tbody>
</table>

</div>

## Passing new data arguments: `newdata`, `wts`, `by`, etc.

Sometimes we want to pass arguments changing or specifying the data on
which we will do our analysis using `marginaleffects`. This can be for
reasons such as wanting to specify the values or weights at which we
evaluate e.g. `avg_slopes()`, or due to the underlying models not
robustly preserving all the original data columns (such as `fixest`
objects not saving their data in the fit object making it potentially
challenging to retrieve, and even if retrievable it will not include the
weights used during fitting as a column as `wts` expects when given a
string).

If we are not using multiple imputation, or if we want to just pass a
single dataset to the several fitted models after multiple imputation,
we can pass a single dataset to the `newdata` argument. However, if we
wish to supply each model in our list resulting after multiple
imputation with a /different/ dataset on which to calculate results, we
cannot use `newdata`. Instead, in this case it can be useful to revert
to a more manual (but still very easy) approach. Here is an example
calculating `avg_slopes` using a different set of weights for each of
the `fixest` models which we fit after multiple imputation.

``` r
set.seed(1024)
library(mice)
library(fixest)
library(marginaleffects)

dat <- mtcars

## insert missing values
dat$hp[sample(seq_len(nrow(mtcars)), 10)] <- NA
dat$mpg[sample(seq_len(nrow(mtcars)), 10)] <- NA
dat$gear[sample(seq_len(nrow(mtcars)), 10)] <- NA

## multiple imputation
dat <- mice(dat, m = 5, method = "sample", printFlag = FALSE)
dat <- complete(dat, action = "all")

## fit models
mod <- lapply(dat, \(x) 
    feglm(am ~ mpg * cyl + hp,
        weight = ~gear,
        family = binomial,
        data = x))

## slopes without weights
lapply(seq_along(mod), \(i) 
    avg_slopes(mod[[i]], newdata = dat[[i]])) |>
    mice::pool()
#> Class: mipo    m = 5 
#>   term m     estimate         ubar            b            t dfcom       df      riv    lambda       fmi
#> 1  cyl 5 -0.134280454 7.093304e-04 2.347331e-03 3.526128e-03    29 2.920745 3.971065 0.7988359 0.8667882
#> 2   hp 5  0.001649773 5.708603e-07 1.375452e-06 2.221403e-06    29 3.556844 2.891325 0.7430181 0.8214040
#> 3  mpg 5  0.006082804 1.080454e-04 2.722234e-04 4.347135e-04    29 3.458294 3.023434 0.7514561 0.8284250

## slopes with weights
lapply(seq_along(mod), \(i) 
    avg_slopes(mod[[i]], newdata = dat[[i]], wts = "gear")) |>
    mice::pool()
#> Class: mipo    m = 5 
#>   term m     estimate         ubar            b            t dfcom       df      riv    lambda       fmi
#> 1  cyl 5 -0.135839444 7.276189e-04 2.481021e-03 0.0037048435    29 2.867574 4.091736 0.8036033 0.8705464
#> 2   hp 5  0.001671173 5.697224e-07 1.424648e-06 0.0000022793    29 3.474698 3.000721 0.7500450 0.8272548
#> 3  mpg 5  0.006251144 1.055910e-04 2.705239e-04 0.0004302197    29 3.422256 3.074397 0.7545649 0.8309976
```
