
# MrP

Data analysts often want to learn about a population using samples that
are not representative of that population. Consider a few examples:

-   *Market research:* A supermarket chain wants to assess consumer
    preferences in each of the markets where it operates, but it would
    be too expensive to collect distinct representative samples for many
    cities.
-   *Political polling:* A newspaper conducts a nationally
    representative survey in the lead up to a Presidential election, and
    wishes to compute state-by-state estimates of voting intentions.
-   *Online surveys:* A researcher conducts a poll online, but the
    respondents are younger and more highly educated than the general
    population.

This notebook introduces [Multilevel Regression with Poststratification
(MrP)](https://en.wikipedia.org/wiki/Multilevel_regression_with_poststratification),
a popular strategy which can be used to limit the distortions in
unrepresentative data, or to estimate subgroup characteristics on the
basis of data gathered at a different level of aggregation. MrP can be
deployed in a wide range of contexts (see this
[paper](https://www.monicaalexander.com/pdf/mrp_chapter.pdf) and this
[blog post](https://www.monicaalexander.com/posts/2019-08-07-mrp/) by
the always excellent [Monica
Alexander](https://www.monicaalexander.com/)).

As we will see below, MrP is super easy to implement using the
`marginaleffects` package for `R`. `marginaleffects` also offers
tremendous benefits to analysts, including a consistent user interface
to over 100 classes of statistical models, as well as many
post-estimation and hypothesis testing tools. To illustrate these
benefits, we will consider a hypothetical example with simulated
data.[1]

<figure>
<img src="fig/mrt.jpg" alt="MrP, not Mister T." />
<figcaption aria-hidden="true">MrP, not Mister T.</figcaption>
</figure>

## MrP for surveys and market research

Imagine that a national supermarket chain plans to introduce a line of
meat substitutes in some of its stores. To guide marketing and
distribution efforts, the company would like to know the share of the
population in each city that is interested in tasting meat substitutes.

The company conducts a telephone survey of 1000 randomly selected adults
from 40 large American cities. For each survey respondent, we record the
city of residence, age, level of education, and whether they are
interested in tasting meat substitutes. The variable we focus on is
“interest in meat substitutes,” measured on a scale of 1 to 7 where 7
means “very interested” and 1 means “not at all interested”. Our
ultimate goal is to estimate the average of this 7 point scale for each
city.

The (simulated) data that we will use is stored in a `R` data frame
called `survey`. We can use the `nrow()` function to confirm the sample
size, and the `datasummary_df()` function from the `modelsummary`
package to display the first few rows of data:

``` r
library(marginaleffects)
library(modelsummary)
library(tidyverse)
library(ggridges)
library(brms)

nrow(survey)
#> [1] 1000

datasummary_df(survey[1:5, ])
```

<div id="xidcfhmhmy" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#xidcfhmhmy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#xidcfhmhmy thead, #xidcfhmhmy tbody, #xidcfhmhmy tfoot, #xidcfhmhmy tr, #xidcfhmhmy td, #xidcfhmhmy th {
  border-style: none;
}

#xidcfhmhmy p {
  margin: 0;
  padding: 0;
}

#xidcfhmhmy .gt_table {
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

#xidcfhmhmy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#xidcfhmhmy .gt_title {
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

#xidcfhmhmy .gt_subtitle {
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

#xidcfhmhmy .gt_heading {
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

#xidcfhmhmy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xidcfhmhmy .gt_col_headings {
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

#xidcfhmhmy .gt_col_heading {
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

#xidcfhmhmy .gt_column_spanner_outer {
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

#xidcfhmhmy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xidcfhmhmy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xidcfhmhmy .gt_column_spanner {
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

#xidcfhmhmy .gt_spanner_row {
  border-bottom-style: hidden;
}

#xidcfhmhmy .gt_group_heading {
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

#xidcfhmhmy .gt_empty_group_heading {
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

#xidcfhmhmy .gt_from_md > :first-child {
  margin-top: 0;
}

#xidcfhmhmy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xidcfhmhmy .gt_row {
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

#xidcfhmhmy .gt_stub {
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

#xidcfhmhmy .gt_stub_row_group {
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

#xidcfhmhmy .gt_row_group_first td {
  border-top-width: 2px;
}

#xidcfhmhmy .gt_row_group_first th {
  border-top-width: 2px;
}

#xidcfhmhmy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xidcfhmhmy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#xidcfhmhmy .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#xidcfhmhmy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xidcfhmhmy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xidcfhmhmy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xidcfhmhmy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#xidcfhmhmy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xidcfhmhmy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xidcfhmhmy .gt_footnotes {
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

#xidcfhmhmy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xidcfhmhmy .gt_sourcenotes {
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

#xidcfhmhmy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xidcfhmhmy .gt_left {
  text-align: left;
}

#xidcfhmhmy .gt_center {
  text-align: center;
}

#xidcfhmhmy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xidcfhmhmy .gt_font_normal {
  font-weight: normal;
}

#xidcfhmhmy .gt_font_bold {
  font-weight: bold;
}

#xidcfhmhmy .gt_font_italic {
  font-style: italic;
}

#xidcfhmhmy .gt_super {
  font-size: 65%;
}

#xidcfhmhmy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#xidcfhmhmy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#xidcfhmhmy .gt_indent_1 {
  text-indent: 5px;
}

#xidcfhmhmy .gt_indent_2 {
  text-indent: 10px;
}

#xidcfhmhmy .gt_indent_3 {
  text-indent: 15px;
}

#xidcfhmhmy .gt_indent_4 {
  text-indent: 20px;
}

#xidcfhmhmy .gt_indent_5 {
  text-indent: 25px;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="header gt_col_headings">
<th id="respondent"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">respondent</th>
<th id="city" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">city</th>
<th id="age" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">age</th>
<th id="education"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">education</th>
<th id="meat_substitute"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">meat_substitute</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="odd">
<td class="gt_row gt_left" headers="respondent">0001</td>
<td class="gt_row gt_left" headers="city">Tucson, AZ</td>
<td class="gt_row gt_left" headers="age">18-54</td>
<td class="gt_row gt_left" headers="education">High school or less</td>
<td class="gt_row gt_left" headers="meat_substitute">1.00</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="respondent">0002</td>
<td class="gt_row gt_left" headers="city">Miami, FL</td>
<td class="gt_row gt_left" headers="age">55+</td>
<td class="gt_row gt_left" headers="education">High school or less</td>
<td class="gt_row gt_left" headers="meat_substitute">6.00</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="respondent">0003</td>
<td class="gt_row gt_left" headers="city">Austin, TX</td>
<td class="gt_row gt_left" headers="age">55+</td>
<td class="gt_row gt_left" headers="education">Post-secondary</td>
<td class="gt_row gt_left" headers="meat_substitute">3.00</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="respondent">0004</td>
<td class="gt_row gt_left" headers="city">Atlanta, GA</td>
<td class="gt_row gt_left" headers="age">18-54</td>
<td class="gt_row gt_left" headers="education">Post-secondary</td>
<td class="gt_row gt_left" headers="meat_substitute">5.00</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="respondent">0005</td>
<td class="gt_row gt_left" headers="city">Milwaukee, WI</td>
<td class="gt_row gt_left" headers="age">18-54</td>
<td class="gt_row gt_left" headers="education">High school or less</td>
<td class="gt_row gt_left" headers="meat_substitute">5.00</td>
</tr>
</tbody>
</table>

</div>

This dataset includes 1000 observations: one per survey respondent.
Unfortunately, it is not straightforward to compute precise city-by-city
estimates, because although the number of respondents is large overall,
the number of respondents within each of the 40 cities is relatively
small. Moreover, the company’s sampling strategy does not guarantee that
subsamples are representative of each city’s population. MrP can help us
circumvent these problems in two steps:

1.  *Multilevel regression (Mr)*: Estimate a multilevel regression at
    the individual level, with random intercepts for cities.
2.  *Poststratification (P)*: Adjust the predictions of the model based
    on the demographic characteristics of each city.

In the rest of this notebook, we show that the `marginaleffects` package
makes it very easy to apply these steps.

## “Mr” for “Multilevel regression”

The first step of MrP is to use individual-level data to estimate a
model that predicts the value of the variable of interest. One of the
great benefits of using `marginaleffects` for MrP, is that this package
is agnostic to the type of model used to make predictions. Analysts can
use almost any model they like, and the workflow described below would
remain the same.

One popular choice for MrP is to estimate a multilevel regression model
with random intercepts for each of the geographic regions of interest.
To do so, analysts could use many different `R` packages, including
[`lme4`](https://cran.r-project.org/package=lme4),
[`glmmTMB`](https://cran.r-project.org/package=glmmTMB), or
[`brms`](https://cran.r-project.org/package=brms). In this notebook, we
use the `brms::brm()` function to estimate a bayesian multilevel model,
with the `age` and `education` variables as fixed components, and random
intercepts at the city level:

``` r
mod <- brm(meat_substitute ~ age + education + (1 | city), data = survey)
```

We can visualize the model estimates using the `modelplot()` function
from the `modelsummary` package:

``` r
modelplot(mod)
```

<img
src="../mrp.markdown_strict_files/figure-markdown_strict/unnamed-chunk-5-1.png"
style="width:100.0%" />

We are now ready for the second MrP step: poststratification.

## “P” for “Poststratification”

In the second MrP step, we use data from the US Census (or a similar
source) to create a “poststratification table.” This table includes one
row for each combination of the predictor values in our model. In our
model, the `age` variable has 2 categories (“18-54” and “54+”); the
`education` variables has 2 categories (“High school or less” and
“Post-secondary”); and the `city` variable has 40 unique entries.
Therefore, the poststratification table must have 2 × 2 × 40 = 160
entries.

Crucially, the poststratification dataset must also include a column
with the population share of each combination of predictor values.
Consider the table used by our hypothetical supermarket chain. This
table includes 160 rows:

``` r
nrow(stratification)
#> [1] 160
```

And here are the entries for the city of Tucson, AZ:

``` r
tucson <- subset(stratification, city == "Tucson, AZ")
datasummary_df(tucson)
```

<div id="lndljavghq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lndljavghq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#lndljavghq thead, #lndljavghq tbody, #lndljavghq tfoot, #lndljavghq tr, #lndljavghq td, #lndljavghq th {
  border-style: none;
}

#lndljavghq p {
  margin: 0;
  padding: 0;
}

#lndljavghq .gt_table {
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

#lndljavghq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#lndljavghq .gt_title {
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

#lndljavghq .gt_subtitle {
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

#lndljavghq .gt_heading {
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

#lndljavghq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lndljavghq .gt_col_headings {
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

#lndljavghq .gt_col_heading {
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

#lndljavghq .gt_column_spanner_outer {
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

#lndljavghq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lndljavghq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lndljavghq .gt_column_spanner {
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

#lndljavghq .gt_spanner_row {
  border-bottom-style: hidden;
}

#lndljavghq .gt_group_heading {
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

#lndljavghq .gt_empty_group_heading {
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

#lndljavghq .gt_from_md > :first-child {
  margin-top: 0;
}

#lndljavghq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lndljavghq .gt_row {
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

#lndljavghq .gt_stub {
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

#lndljavghq .gt_stub_row_group {
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

#lndljavghq .gt_row_group_first td {
  border-top-width: 2px;
}

#lndljavghq .gt_row_group_first th {
  border-top-width: 2px;
}

#lndljavghq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lndljavghq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lndljavghq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lndljavghq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lndljavghq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lndljavghq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lndljavghq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#lndljavghq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lndljavghq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lndljavghq .gt_footnotes {
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

#lndljavghq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lndljavghq .gt_sourcenotes {
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

#lndljavghq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lndljavghq .gt_left {
  text-align: left;
}

#lndljavghq .gt_center {
  text-align: center;
}

#lndljavghq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lndljavghq .gt_font_normal {
  font-weight: normal;
}

#lndljavghq .gt_font_bold {
  font-weight: bold;
}

#lndljavghq .gt_font_italic {
  font-style: italic;
}

#lndljavghq .gt_super {
  font-size: 65%;
}

#lndljavghq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#lndljavghq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lndljavghq .gt_indent_1 {
  text-indent: 5px;
}

#lndljavghq .gt_indent_2 {
  text-indent: 10px;
}

#lndljavghq .gt_indent_3 {
  text-indent: 15px;
}

#lndljavghq .gt_indent_4 {
  text-indent: 20px;
}

#lndljavghq .gt_indent_5 {
  text-indent: 25px;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="header gt_col_headings">
<th id="city" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">city</th>
<th id="education"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">education</th>
<th id="age" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">age</th>
<th id="population_share"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">population_share</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="odd">
<td class="gt_row gt_left" headers="city">Tucson, AZ</td>
<td class="gt_row gt_left" headers="education">High school or less</td>
<td class="gt_row gt_left" headers="age">18-54</td>
<td class="gt_row gt_left" headers="population_share">0.16</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="city">Tucson, AZ</td>
<td class="gt_row gt_left" headers="education">Post-secondary</td>
<td class="gt_row gt_left" headers="age">18-54</td>
<td class="gt_row gt_left" headers="population_share">0.38</td>
</tr>
<tr class="odd">
<td class="gt_row gt_left" headers="city">Tucson, AZ</td>
<td class="gt_row gt_left" headers="education">High school or less</td>
<td class="gt_row gt_left" headers="age">55+</td>
<td class="gt_row gt_left" headers="population_share">0.20</td>
</tr>
<tr class="even">
<td class="gt_row gt_left" headers="city">Tucson, AZ</td>
<td class="gt_row gt_left" headers="education">Post-secondary</td>
<td class="gt_row gt_left" headers="age">55+</td>
<td class="gt_row gt_left" headers="population_share">0.25</td>
</tr>
</tbody>
</table>

</div>

According to these (simulated) data, the share of Tucson residents who
are between 18 and 54 year old and have a High School degree or less is
about 16%.

We can use the `predictions()` function from the `marginaleffects`
package to predict the value of the `meat_substitute` variable for each
of the four categories of residents in Tucson:

``` r
predictions(mod, newdata = tucson)
#> 
#>  Estimate 2.5 % 97.5 %
#>      1.81  1.48   2.13
#>      3.03  2.71   3.34
#>      3.01  2.69   3.33
#>      4.23  3.90   4.54
#> 
#> Columns: rowid, estimate, conf.low, conf.high, city, education, age, population_share, meat_substitute 
#> Type:  response
```

The MrP estimate for this city is simply the weighted average of
predicted values, where weights are the population shares of each
category of residents. In this context, we have:

0.16 × 1.81 + 0.38 × 3.03 + 0.20 × 3.01 + 0.25 × 4.23 = 3.13

Instead of computing estimates by hand for each city, we can use the
`by` and `wts` arguments of the `predictions()` function to do
everything everywhere all at once:

``` r
p <- predictions(              # Compute predictions,
    model = mod,               # using the multilevel regression model `mod`, 
    newdata = stratification,  # for each row of the `stratification` table.
    by = "city",               # Then, take the weighted average of predictions by city,
    wts = "population_share")  # using demographic weights.
p
#> 
#>               city Estimate 2.5 % 97.5 %
#>  Washington, DC        4.31  3.99   4.64
#>  Tucson, AZ            3.13  2.81   3.44
#>  Seattle, WA           5.06  4.73   5.39
#>  San Jose, CA          3.91  3.59   4.25
#>  San Francisco, CA     5.07  4.72   5.40
#> --- 30 rows omitted. See ?print.marginaleffects --- 
#>  Boston, MA            3.65  3.37   3.93
#>  Baltimore, MD         2.93  2.56   3.31
#>  Austin, TX            2.88  2.56   3.19
#>  Atlanta, GA           4.59  4.27   4.94
#>  Albuquerque, NM       2.67  2.35   2.99
#> Columns: city, estimate, conf.low, conf.high 
#> Type:  response
```

Since we estimated a bayesian model in the “Mr” step, we can now use the
`posterior_draws()` function to extract draws from the posterior
distribution of the MrP estimates. This allows us to compute credible
intervals for each city, and draw many fancy plots like this one:

``` r
p |> 
    # extract draws from the posterior distribution
    posterior_draws() |>
    # sort cities by interest in meat substitutes
    arrange(estimate) |>
    mutate(city = factor(city, levels = rev(unique(city)))) |>
    # plot the results
    ggplot(aes(x = draw, y = city)) +
    geom_density_ridges() +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    labs(
        x = "Average interest in meat substitutes",
        y = NULL,
        title = "Estimated interest in meat substitutes by city",
        subtitle = "Multilevel Regression and Poststratification",
        caption = "Source: Simulated data")
```

<img
src="../mrp.markdown_strict_files/figure-markdown_strict/unnamed-chunk-11-1.png"
style="width:100.0%" />

## Data simulation

All the data used on this page were simulated using this code:

``` r
library(marginaleffects)
library(countrycode)
library(tidyverse)
library(modelsummary)
library(gt)
library(brms)
set.seed(1024)

cities <- c("New York City, NY", "Los Angeles, CA", "Chicago, IL", "Houston, TX", "Phoenix, AZ", "Philadelphia, PA", "San Antonio, TX", "San Diego, CA", "Dallas, TX", "San Jose, CA", "Austin, TX", "Jacksonville, FL", "Fort Worth, TX", "Columbus, OH", "San Francisco, CA", "Charlotte, NC", "Indianapolis, IN", "Seattle, WA", "Denver, CO", "Washington, DC", "Boston, MA", "Nashville, TN", "El Paso, TX", "Detroit, MI", "Memphis, TN", "Portland, OR", "Oklahoma City, OK", "Las Vegas, NV", "Louisville, KY", "Baltimore, MD", "Milwaukee, WI", "Albuquerque, NM", "Tucson, AZ", "Fresno, CA", "Sacramento, CA", "Mesa, AZ", "Atlanta, GA", "Kansas City, MO", "Colorado Springs, CO", "Miami, FL")
cities <- rev(sort(cities))
education <- c("High school or less", "Post-secondary")
age <- c("18-54", "55+")
stratification <- expand.grid(
    city = cities,
    education = education,
    age = age) |>
    mutate(
        population_share = runif(n(), min = .25, max = .75),
        population_share = population_share / sum(population_share),
        .by = "city",) |>
    arrange(city)
N <- 1000
survey <- data.frame(
    city = sample(cities, N, replace = TRUE),
    age = sample(age, N, replace = TRUE),
    education = sample(education, N, replace = TRUE)
)
survey <- data.frame(
    respondent = sprintf("%04d", 1:N),
    survey)
M <- model.matrix(~., survey)
b <- runif(ncol(M))
survey$meat_substitute <- as.numeric(cut(M %*% b, breaks = 7))
```

[1] See the bottom of this page for the simulation code.
