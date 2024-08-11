

<center>
<!-- <a href = "https://marginaleffects.com" target = "_blank"><img src="man/figures/marginaleffects_logo_word.svg" width = "100%"></a> -->
<a href = "https://marginaleffects.com" target = "_blank"><img src="man/figures/zoo_header.png" width = "100%"></a>
<br>
<center>
<h3>
How to interpret statistical models in R and Python
</h3>
</center>
<br>
<a href = "https://github.com/vincentarelbundock/marginaleffects/blob/main/LICENSE.md" target = "_blank"><img src="https://img.shields.io/badge/license-GPLv3-blue"></a>
<a href = "https://marginaleffects.com" target = "_blank"><img src="https://img.shields.io/static/v1?label=Website&message=Visit&color=blue"></a>
<a href = "https://marginaleffects.com" target = "_blank"><img src="https://cranlogs.r-pkg.org/badges/grand-total/marginaleffects"></a>
<br><br>
</center>

The parameters of a statistical model can sometimes be difficult to
interpret substantively, especially when that model includes non-linear
components, interactions, or transformations. Analysts who fit such
complex models often seek to transform raw parameter estimates into
quantities that are easier for domain experts and stakeholders to
understand, such as predictions, contrasts, risk differences, ratios,
odds, lift, slopes, and so on.

Unfortunately, computing these quantities—along with associated standard
errors—can be a tedious and error-prone task. This problem is compounded
by the fact that modeling packages in `R` and `Python` produce objects
with varied structures, which hold different information. This means
that end-users often have to write customized code to interpret the
estimates obtained by fitting Linear, GLM, GAM, Bayesian, Mixed Effects,
and other model types. This can lead to wasted effort, confusion, and
mistakes, and it can hinder the implementation of best practices.

## Book

[This free online book](https://marginaleffects.com/) introduces a
conceptual framework to clearly define statistical quantities of
interest, and shows how to estimate those quantities using the
`marginaleffects` package for `R` and `Python`. The techniques
introduced herein can enhance the interpretability of [over 100 classes
of statistical and machine learning
models](https://marginaleffects.com/vignettes/supported_models.html),
including linear, GLM, GAM, mixed-effects, bayesian, categorical
outcomes, XGBoost, and more. With a single unified interface, users can
compute and plot many estimands, including:

-   Predictions (aka fitted values or adjusted predictions)
-   Comparisons such as contrasts, risk differences, risk ratios, odds,
    etc.
-   Slopes (aka marginal effects or partial derivatives)
-   Marginal means
-   Linear and non-linear hypothesis tests
-   Equivalence tests
-   Uncertainty estimates using the delta method, bootstrapping,
    simulation, or conformal inference.
-   Much more!

[The Marginal Effects Zoo](https://marginaleffects.com/) book includes
over 30 chapters of tutorials, case studies, and technical notes. It
covers a wide range of topics, including how the `marginaleffects`
package can facilitate the analysis of:

-   Experiments
-   Observational data
-   Causal inference with G-Computation
-   Machine learning models
-   Bayesian modeling
-   Multilevel regression with post-stratification (MRP)
-   Missing data
-   Matching
-   Inverse probability weighting
-   Conformal prediction

[Get started by clicking
here!](https://marginaleffects.com/vignettes/get_started.html)

## Article

Our article on `marginaleffects` is provisionally accepted for
publication by the *Journal of Statistical Software*. You can read [the
preprint
here.](https://marginaleffects.com/files/marginaleffects_arel-bundock_greifer_heiss_jss5115.pdf)

To cite `marginaleffects` in publications please use:

Arel-Bundock V, Greifer N, Heiss A (Forthcoming). “How to Interpret
Statistical Models Using marginaleffects in R and Python.” *Journal of
Statistical Software*.

A BibTeX entry for LaTeX users is:

``` {latex}
@Article{,
    title = {How to Interpret Statistical Models Using {marginaleffects} in {R} and {Python}},
    author = {Vincent Arel-Bundock and Noah Greifer and Andrew Heiss},
    year = {Forthcoming},
    journal = {Journal of Statistical Software},
}
```

## Software

The `marginaleffects` package for `R` and `Python` offers a single point
of entry to easily interpret the results of [over 100 classes of
models,](https://marginaleffects.com/vignettes/supported_models.html)
using a simple and consistent user interface. Its benefits include:

-   *Powerful:* It can compute and plot predictions; comparisons
    (contrasts, risk ratios, etc.); slopes; and conduct hypothesis and
    equivalence tests for over 100 different classes of models in `R`.
-   *Simple:* All functions share a simple and unified interface.
-   *Documented*: Each function is thoroughly documented with abundant
    examples. The Marginal Effects Zoo website includes 20,000+ words of
    vignettes and case studies.
-   *Efficient:* [Some
    operations](https://marginaleffects.com/vignettes/performance.html)
    can be up to 1000 times faster and use 30 times less memory than
    with the `margins` package.  
-   *Valid:* When possible, [numerical results are
    checked](https://marginaleffects.com/vignettes/supported_models.html)
    against alternative software like `Stata` or other `R` packages.
-   *Thin:* The `R` package requires relatively few dependencies.
-   *Standards-compliant:* `marginaleffects` follows “tidy” principles
    and returns simple data frames that work with all standard `R`
    functions. The outputs are easy to program with and feed to other
    packages like
    [`ggplot2`](https://marginaleffects.com/vignettes/plot.html) or
    [`modelsummary`.](https://marginaleffects.com/vignettes/tables.html)
-   *Extensible:* Adding support for new models is very easy, often
    requiring less than 10 lines of new code. Please submit [feature
    requests on
    Github.](https://github.com/vincentarelbundock/marginaleffects/issues)
-   *Active development*: Bugs are fixed promptly.
