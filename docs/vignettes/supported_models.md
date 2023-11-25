
<script src="supported_models_files/libs/kePrint-0.0.1/kePrint.js"></script>
<link href="supported_models_files/libs/lightable-0.0.1/lightable.css" rel="stylesheet" />


# Supported Models

`marginaleffects` effects supports 90 model types directly, and dozens
more via the `tidymodels` and `mlr3` frameworks. This table shows the
list of directly supported model types. There are three main alternative
software packages to compute such slopes (1) `Stata`’s `margins`
command, (2) `R`’s `margins::margins()` function, and (3) `R`’s
`emmeans::emtrends()` function. The [test suite hosted on
Github](https://github.com/vincentarelbundock/marginaleffects/tree/main/inst/tinytest)
compares the numerical equivalence of results produced by
`marginaleffects::slopes()` to those produced by all 3 alternative
software packages:

-   ✓: a green check means that the results of at least one model are
    equal to a reasonable tolerance.
-   ✖: a red cross means that the results are *not* identical; extra
    caution is warranted.
-   U: a grey U means that computing slopes for a model type is
    *unsupported* by alternative packages, but supported by
    `marginaleffects`.
-   An empty cell means means that no comparison has been made yet.

I am eager to add support for new models. Feel free to file a request or
submit code on Github.

<table class="table" data-quarto-postprocess="true"
style="margin-left: auto; margin-right: auto;">
<colgroup>
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr class="header">
<th colspan="2" data-quarto-table-cell-role="th"
style="text-align: left; empty-cells: hide; border-bottom: hidden;"></th>
<th colspan="6" data-quarto-table-cell-role="th"
style="text-align: center; border-bottom: hidden; padding-bottom: 0; padding-left: 3px; padding-right: 3px;"><div
style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">
Numerical equivalence
</div></th>
</tr>
<tr class="odd">
<th colspan="2" data-quarto-table-cell-role="th"
style="text-align: center; border-bottom: hidden; padding-bottom: 0; padding-left: 3px; padding-right: 3px;"><div
style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">
Supported by marginaleffects
</div></th>
<th colspan="2" data-quarto-table-cell-role="th"
style="text-align: center; border-bottom: hidden; padding-bottom: 0; padding-left: 3px; padding-right: 3px;"><div
style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">
Stata
</div></th>
<th colspan="2" data-quarto-table-cell-role="th"
style="text-align: center; border-bottom: hidden; padding-bottom: 0; padding-left: 3px; padding-right: 3px;"><div
style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">
margins
</div></th>
<th colspan="2" data-quarto-table-cell-role="th"
style="text-align: center; border-bottom: hidden; padding-bottom: 0; padding-left: 3px; padding-right: 3px;"><div
style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">
emtrends
</div></th>
</tr>
<tr class="header">
<th style="text-align: left;"
data-quarto-table-cell-role="th">Package</th>
<th style="text-align: left;"
data-quarto-table-cell-role="th">Function</th>
<th style="text-align: center;"
data-quarto-table-cell-role="th">dY/dX</th>
<th style="text-align: center;" data-quarto-table-cell-role="th">SE</th>
<th style="text-align: center;"
data-quarto-table-cell-role="th">dY/dX</th>
<th style="text-align: center;" data-quarto-table-cell-role="th">SE</th>
<th style="text-align: center;"
data-quarto-table-cell-role="th">dY/dX</th>
<th style="text-align: center;" data-quarto-table-cell-role="th">SE</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">stats</td>
<td style="text-align: left;">lm</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">glm</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">nls</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">loess</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">AER</td>
<td style="text-align: left;">ivreg</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">tobit</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;">afex</td>
<td style="text-align: left;">afex_aov</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;">aod</td>
<td style="text-align: left;">betabin</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">betareg</td>
<td style="text-align: left;">betareg</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;">bife</td>
<td style="text-align: left;">bife</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">biglm</td>
<td style="text-align: left;">biglm</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">bigglm</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">blme</td>
<td style="text-align: left;">blmer</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">bglmer</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;">brglm2</td>
<td style="text-align: left;">bracl</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">brglmFit</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">brnb</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">brmultinom</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">brms</td>
<td style="text-align: left;">brm</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;">crch</td>
<td style="text-align: left;">crch</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">hxlr</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;">DCchoice</td>
<td style="text-align: left;">oohbchoice</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;">estimatr</td>
<td style="text-align: left;">lm_lin</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">lm_robust</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">iv_robust</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;">fixest</td>
<td style="text-align: left;">feols</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">feglm</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">fenegbin</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">fepois</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;">gam</td>
<td style="text-align: left;">gam</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;">gamlss</td>
<td style="text-align: left;">gamlss</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;">geepack</td>
<td style="text-align: left;">geeglm</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;">glmmTMB</td>
<td style="text-align: left;">glmmTMB</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;">glmx</td>
<td style="text-align: left;">glmx</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">ivreg</td>
<td style="text-align: left;">ivreg</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;">mlr3</td>
<td style="text-align: left;">Learner</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;">lme4</td>
<td style="text-align: left;">lmer</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">glmer</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">glmer.nb</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;">lmerTest</td>
<td style="text-align: left;">lmer</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;">logistf</td>
<td style="text-align: left;">logistf</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">flic</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">flac</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;">MASS</td>
<td style="text-align: left;">glmmPQL</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">glm.nb</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">polr</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: red !important;">✖</td>
<td style="text-align: center; color: red !important;">✖</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">rlm</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;">mclogit</td>
<td style="text-align: left;">mblogit</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">mclogit</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;">MCMCglmm</td>
<td style="text-align: left;">MCMCglmm</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">mgcv</td>
<td style="text-align: left;">gam</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">bam</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: red !important;">✖</td>
</tr>
<tr class="odd">
<td style="text-align: left;">mhurdle</td>
<td style="text-align: left;">mhurdle</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;">mlogit</td>
<td style="text-align: left;">mlogit</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">mvgam</td>
<td style="text-align: left;">mvgam</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;">nlme</td>
<td style="text-align: left;">gls</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">lme</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;">nnet</td>
<td style="text-align: left;">multinom</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">ordbetareg</td>
<td style="text-align: left;">ordbetareg</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;">ordinal</td>
<td style="text-align: left;">clm</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">plm</td>
<td style="text-align: left;">plm</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;">phylolm</td>
<td style="text-align: left;">phylolm</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">phyloglm</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;">pscl</td>
<td style="text-align: left;">hurdle</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: red !important;">✖</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">hurdle</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: red !important;">✖</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">zeroinfl</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;">quantreg</td>
<td style="text-align: left;">rq</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;">Rchoice</td>
<td style="text-align: left;">hetprob</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">ivpml</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;">rms</td>
<td style="text-align: left;">ols</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">lrm</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">orm</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">Gls</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;">robust</td>
<td style="text-align: left;">lmRob</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">robustbase</td>
<td style="text-align: left;">glmrob</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">lmrob</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">robustlmm</td>
<td style="text-align: left;">rlmer</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;">rstanarm</td>
<td style="text-align: left;">stan_glm</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: red !important;">✖</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;">sampleSelection</td>
<td style="text-align: left;">selection</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">heckit</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;">scam</td>
<td style="text-align: left;">scam</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;">speedglm</td>
<td style="text-align: left;">speedglm</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">speedlm</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;">survey</td>
<td style="text-align: left;">svyglm</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">svyolr</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="even">
<td style="text-align: left;">survival</td>
<td style="text-align: left;">clogit</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;"></td>
<td style="text-align: left;">coxph</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
</tr>
<tr class="even">
<td style="text-align: left;"></td>
<td style="text-align: left;">survreg</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
</tr>
<tr class="odd">
<td style="text-align: left;">tobit1</td>
<td style="text-align: left;">tobit1</td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: gray !important;"></td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
<tr class="even">
<td style="text-align: left;">truncreg</td>
<td style="text-align: left;">truncreg</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: green !important;">✓</td>
<td style="text-align: center; color: gray !important;">U</td>
<td style="text-align: center; color: gray !important;">U</td>
</tr>
</tbody>
</table>
