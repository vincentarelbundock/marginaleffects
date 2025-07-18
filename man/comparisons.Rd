% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparisons.R
\name{comparisons}
\alias{comparisons}
\alias{avg_comparisons}
\title{Comparisons Between Predictions Made With Different Regressor Values}
\usage{
comparisons(
  model,
  newdata = NULL,
  variables = NULL,
  comparison = "difference",
  type = NULL,
  vcov = TRUE,
  by = FALSE,
  conf_level = 0.95,
  transform = NULL,
  cross = FALSE,
  wts = FALSE,
  hypothesis = NULL,
  equivalence = NULL,
  df = Inf,
  eps = NULL,
  numderiv = "fdforward",
  ...
)

avg_comparisons(
  model,
  newdata = NULL,
  variables = NULL,
  type = NULL,
  vcov = TRUE,
  by = TRUE,
  conf_level = 0.95,
  comparison = "difference",
  transform = NULL,
  cross = FALSE,
  wts = FALSE,
  hypothesis = NULL,
  equivalence = NULL,
  df = Inf,
  eps = NULL,
  numderiv = "fdforward",
  ...
)
}
\arguments{
\item{model}{Model object}

\item{newdata}{Grid of predictor values at which we evaluate the comparisons.
\itemize{
\item Warning: Avoid modifying your dataset between fitting the model and calling a \code{marginaleffects} function. This can sometimes lead to unexpected results.
\item \code{NULL} (default): Unit-level contrasts for each observed value in the dataset (empirical distribution). The dataset is retrieved using \code{\link[insight:get_data]{insight::get_data()}}, which tries to extract data from the environment. This may produce unexpected results if the original data frame has been altered since fitting the model.
\item data frame: Unit-level contrasts for each row of the \code{newdata} data frame.
\item string:
\itemize{
\item "mean": Contrasts at the Mean. Contrasts when each predictor is held at its mean or mode.
\item "median": Contrasts at the Median. Contrasts when each predictor is held at its median or mode.
\item "balanced": Contrasts evaluated on a balanced grid with every combination of categories and numeric variables held at their means.
\item "tukey": Contrasts at Tukey's 5 numbers.
\item "grid": Contrasts on a grid of representative numbers (Tukey's 5 numbers and unique values of categorical predictors).
}
\item \code{\link[=datagrid]{datagrid()}} call to specify a custom grid of regressors. For example:
\itemize{
\item \code{newdata = datagrid(cyl = c(4, 6))}: \code{cyl} variable equal to 4 and 6 and other regressors fixed at their means or modes.
\item \code{newdata = datagrid(mpg = fivenum)}: \code{mpg} variable held at Tukey's five numbers (using the \code{fivenum} function), and other regressors fixed at their means or modes.
\item See the Examples section and the \link{datagrid} documentation.
}
\item \code{\link[=subset]{subset()}} call with a single argument to select a subset of the dataset used to fit the model, ex: \code{newdata = subset(treatment == 1)}
\item \code{\link[dplyr:filter]{dplyr::filter()}} call with a single argument to select a subset of the dataset used to fit the model, ex: \code{newdata = filter(treatment == 1)}
}}

\item{variables}{Focal variables
\itemize{
\item \code{NULL}: compute comparisons for all the variables in the model object (can be slow).
\item Character vector: subset of variables (usually faster).
\item Named list: names identify the subset of variables of interest, and values define the type of contrast to compute. Acceptable values depend on the variable type:
\itemize{
\item Factor or character variables:
\itemize{
\item "reference": Each factor level is compared to the factor reference (base) level
\item "all": All combinations of observed levels
\item "sequential": Each factor level is compared to the previous factor level
\item "pairwise": Each factor level is compared to all other levels
\item "minmax": The highest and lowest levels of a factor.
\item "revpairwise", "revreference", "revsequential": inverse of the corresponding hypotheses.
\item Vector of length 2 with the two values to compare.
\item Data frame with the same number of rows as \code{newdata}, with two columns of "lo" and "hi" values to compare.
\item Function that accepts a vector and returns a data frame with two columns of "lo" and "hi" values to compare. See examples below.
}
\item Logical variables:
\itemize{
\item NULL: contrast between TRUE and FALSE
\item Data frame with the same number of rows as \code{newdata}, with two columns of "lo" and "hi" values to compare.
\item Function that accepts a vector and returns a data frame with two columns of "lo" and "hi" values to compare. See examples below.
}
\item Numeric variables:
\itemize{
\item Numeric of length 1: Forward contrast for a gap of \code{x}, computed between the observed value and the observed value plus \code{x}. Users can set a global option to get a "center" or "backward" contrast instead: \code{options(marginaleffects_contrast_direction="center")}
\item Numeric vector of length 2: Contrast between the largest and the smallest elements of the \code{x} vector.
\item Data frame with the same number of rows as \code{newdata}, with two columns of "lo" and "hi" values to compare.
\item Function that accepts a vector and returns a data frame with two columns of "lo" and "hi" values to compare. See examples below.
\item "iqr": Contrast across the interquartile range of the regressor.
\item "sd": Contrast across one standard deviation around the regressor mean.
\item "2sd": Contrast across two standard deviations around the regressor mean.
\item "minmax": Contrast between the maximum and the minimum values of the regressor.
}
\item Examples:
\itemize{
\item \code{variables = list(gear = "pairwise", hp = 10)}
\item \code{variables = list(gear = "sequential", hp = c(100, 120))}
\item \code{variables = list(hp = function(x) data.frame(low = x - 5, high = x + 10))}
\item See the Examples section below for more.
}
}
}}

\item{comparison}{How should pairs of predictions be compared? Difference, ratio, odds ratio, or user-defined functions.
\itemize{
\item string: shortcuts to common contrast functions.
\itemize{
\item Supported shortcuts strings: difference, differenceavg, differenceavgwts, dydx, eyex, eydx, dyex, dydxavg, eyexavg, eydxavg, dyexavg, dydxavgwts, eyexavgwts, eydxavgwts, dyexavgwts, ratio, ratioavg, ratioavgwts, lnratio, lnratioavg, lnratioavgwts, lnor, lnoravg, lnoravgwts, lift, liftavg, liftavgwts, expdydx, expdydxavg, expdydxavgwts
\item See the Comparisons section below for definitions of each transformation.
}
\item function: accept two equal-length numeric vectors of adjusted predictions (\code{hi} and \code{lo}) and returns a vector of contrasts of the same length, or a unique numeric value.
\itemize{
\item See the Transformations section below for examples of valid functions.
}
}}

\item{type}{string indicates the type (scale) of the predictions used to
compute contrasts or slopes. This can differ based on the model
type, but will typically be a string such as: "response", "link", "probs",
or "zero". When an unsupported string is entered, the model-specific list of
acceptable values is returned in an error message. When \code{type} is \code{NULL}, the
first entry in the error message is used by default.}

\item{vcov}{Type of uncertainty estimates to report (e.g., for robust standard errors). Acceptable values:
\itemize{
\item FALSE: Do not compute standard errors. This can speed up computation considerably.
\item TRUE: Unit-level standard errors using the default \code{vcov(model)} variance-covariance matrix.
\item String which indicates the kind of uncertainty estimates to return.
\itemize{
\item Heteroskedasticity-consistent: \code{"HC"}, \code{"HC0"}, \code{"HC1"}, \code{"HC2"}, \code{"HC3"}, \code{"HC4"}, \code{"HC4m"}, \code{"HC5"}. See \code{?sandwich::vcovHC}
\item Heteroskedasticity and autocorrelation consistent: \code{"HAC"}
\item Mixed-Models degrees of freedom: "satterthwaite", "kenward-roger"
\item Other: \code{"NeweyWest"}, \code{"KernHAC"}, \code{"OPG"}. See the \code{sandwich} package documentation.
\item "rsample", "boot", "fwb", and "simulation" are passed to the \code{method} argument of the \code{inferences()} function. To customize the bootstrap or simulation process, call \code{inferences()} directly.
}
\item One-sided formula which indicates the name of cluster variables (e.g., \code{~unit_id}). This formula is passed to the \code{cluster} argument of the \code{sandwich::vcovCL} function.
\item Square covariance matrix
\item Function which returns a covariance matrix (e.g., \code{stats::vcov(model)})
}}

\item{by}{Aggregate unit-level estimates (aka, marginalize, average over). Valid inputs:
\itemize{
\item \code{FALSE}: return the original unit-level estimates.
\item \code{TRUE}: aggregate estimates for each term.
\item Character vector of column names in \code{newdata} or in the data frame produced by calling the function without the \code{by} argument.
\item Data frame with a \code{by} column of group labels, and merging columns shared by \code{newdata} or the data frame produced by calling the same function without the \code{by} argument.
\item See examples below.
\item For more complex aggregations, you can use the \code{FUN} argument of the \code{hypotheses()} function. See that function's documentation and the Hypothesis Test vignettes on the \code{marginaleffects} website.
}}

\item{conf_level}{numeric value between 0 and 1. Confidence level to use to build a confidence interval.}

\item{transform}{string or function. Transformation applied to unit-level estimates and confidence intervals just before the function returns results. Functions must accept a vector and return a vector of the same length. Support string shortcuts: "exp", "ln"}

\item{cross}{\itemize{
\item \code{FALSE}: Contrasts represent the change in adjusted predictions when one predictor changes and all other variables are held constant.
\item \code{TRUE}: Contrasts represent the changes in adjusted predictions when all the predictors specified in the \code{variables} argument are manipulated simultaneously (a "cross-contrast").
}}

\item{wts}{logical, string or numeric: weights to use when computing average predictions, contrasts or slopes. These weights only affect the averaging in \verb{avg_*()} or with the \code{by} argument, and not unit-level estimates. See \code{?weighted.mean}
\itemize{
\item string: column name of the weights variable in \code{newdata}. When supplying a column name to \code{wts}, it is recommended to supply the original data (including the weights variable) explicitly to \code{newdata}.
\item numeric: vector of length equal to the number of rows in the original data or in \code{newdata} (if supplied).
\item FALSE: Equal weights.
\item TRUE: Extract weights from the fitted object with \code{insight::find_weights()} and use them when taking weighted averages of estimates. Warning: \code{newdata=datagrid()} returns a single average weight, which is equivalent to using \code{wts=FALSE}
}}

\item{hypothesis}{specify a hypothesis test or custom contrast using a number , formula, string equation, vector, matrix, or function.
\itemize{
\item Number: The null hypothesis used in the computation of Z and p (before applying \code{transform}).
\item String: Equation to specify linear or non-linear hypothesis tests. Two-tailed tests must include an equal \code{=} sign. One-tailed tests must start with \code{<} or \code{>}. If the terms in \code{coef(object)} uniquely identify estimates, they can be used in the formula. Otherwise, use \code{b1}, \code{b2}, etc. to identify the position of each parameter. The \verb{b*} wildcard can be used to test hypotheses on all estimates. When the hypothesis string represents a two-sided equation, the \code{estimate} column holds the value of the left side minus the right side of the equation. If a named vector is used, the names are used as labels in the output. Examples:
\itemize{
\item \code{hp = drat}
\item \code{hp + drat = 12}
\item \code{b1 + b2 + b3 = 0}
\item \verb{b* / b1 = 1}
\item \verb{<= 0}
\item \verb{>= -3.5}
\item \code{b1 >= 10}
}
\item Formula: \code{lhs ~ rhs | group}
\itemize{
\item \code{lhs}
\itemize{
\item \code{ratio} (null = 1)
\item \code{difference} (null = 0)
\item Leave empty for default value
}
\item \code{rhs}
\itemize{
\item \code{pairwise} and \code{revpairwise}: pairwise differences between estimates in each row.
\item \code{reference}: differences between the estimates in each row and the estimate in the first row.
\item \code{sequential}: difference between an estimate and the estimate in the next row.
\item \code{meandev}: difference between an estimate and the mean of all estimates.
\item `meanotherdev: difference between an estimate and the mean of all other estimates, excluding the current one.
\item \code{poly}: polynomial contrasts, as computed by the \code{stats::contr.poly()} function.
\item \code{helmert}: Helmert contrasts, as computed by the \code{stats::contr.helmert()} function. Contrast 2nd level to the first, 3rd to the average of the first two, and so on.
\item \code{trt_vs_ctrl}: difference between the mean of estimates (except the first) and the first estimate.
\item \code{I(fun(x))}: custom function to manipulate the vector of estimates \code{x}. The function \code{fun()} can return multiple (potentially named) estimates.
}
\item \code{group} (optional)
\itemize{
\item Column name of \code{newdata}. Conduct hypothesis tests withing subsets of the data.
}
\item Examples:
\itemize{
\item \code{~ poly}
\item \code{~ sequential | groupid}
\item \code{~ reference}
\item \code{ratio ~ pairwise}
\item \code{difference ~ pairwise | groupid}
\item \code{~ I(x - mean(x)) | groupid}
\item \verb{~ I(\\(x) c(a = x[1], b = mean(x[2:3]))) | groupid}
}
}
\item Matrix or Vector: Each column is a vector of weights. The the output is the dot product between these vectors of weights and the vector of estimates. The matrix can have column names to label the estimates.
\item Function:
\itemize{
\item Accepts an argument \code{x}: object produced by a \code{marginaleffects} function or a data frame with column \code{rowid} and \code{estimate}
\item Returns a data frame with columns \code{term} and \code{estimate} (mandatory) and \code{rowid} (optional).
\item The function can also accept optional input arguments: \code{newdata}, \code{by}, \code{draws}.
\item This function approach will not work for Bayesian models or with bootstrapping. In those cases, it is easy to use \code{get_draws()} to extract and manipulate the draws directly.
}
\item See the Examples section below and the vignette: \url{https://marginaleffects.com/chapters/hypothesis.html}
\item Warning: When calling \code{predictions()} with \code{type="invlink(link)"} (the default in some models), \code{hypothesis} is tested and p values are computed on the link scale.
}}

\item{equivalence}{Numeric vector of length 2: bounds used for the two-one-sided test (TOST) of equivalence, and for the non-inferiority and non-superiority tests. See Details section below.}

\item{df}{Degrees of freedom used to compute p values and confidence intervals.
\itemize{
\item A single numeric value between 1 and \code{Inf}, or a numeric vector with length equal to the number of rows in the output. When \code{df} is \code{Inf}, the normal distribution is used. When \code{df} is finite, the \code{t} distribution is used.
\item "residual": Calls \link[insight:get_df]{insight::get_df} to extract degrees of freedom from the model automatically.
\item "satterthwaite" or "kenward-roger": Use the Satterthwaite or Kenward-Roger approximation to compute degrees of freedom in mixed effects models.
}}

\item{eps}{NULL or numeric value which determines the step size to use when
calculating numerical derivatives: (f(x+eps)-f(x))/eps. When \code{eps} is
\code{NULL}, the step size is 0.0001 multiplied by the difference between
the maximum and minimum values of the variable with respect to which we
are taking the derivative. Changing \code{eps} may be necessary to avoid
numerical problems in certain models.}

\item{numderiv}{string or list of strings indicating the method to use to for the numeric differentiation used in to compute delta method standard errors.
\itemize{
\item "fdforward": finite difference method with forward differences
\item "fdcenter": finite difference method with central differences (default)
\item "richardson": Richardson extrapolation method
\item Extra arguments can be specified by passing a list to the \code{numDeriv} argument, with the name of the method first and named arguments following, ex: \code{numderiv=list("fdcenter", eps = 1e-5)}. When an unknown argument is used, \code{marginaleffects} prints the list of valid arguments for each method.
}}

\item{...}{Additional arguments are passed to the \code{predict()} method
supplied by the modeling package.These arguments are particularly useful
for mixed-effects or bayesian models (see the online vignettes on the
\code{marginaleffects} website). Available arguments can vary from model to
model, depending on the range of supported arguments by each modeling
package. See the "Model-Specific Arguments" section of the
\code{?slopes} documentation for a non-exhaustive list of available
arguments.}
}
\value{
A \code{data.frame} with one row per estimate. This data frame is pretty-printed by default, but users can interact with it as a regular data frame, with functions like \code{nrow()}, \code{head()}, \code{colnames()}, etc. Values can be extracted using standard \verb{[,]} or \code{$} operators, and manipulated using external packages like \code{dplyr} or \code{data.table}.

Columns may include:
\itemize{
\item \code{rowid}: row number of the \code{newdata} data frame
\item \code{group}: (optional) value of the grouped outcome (e.g., categorical outcome models)
\item \code{term}: the focal variable.
\item \code{estimate}: an estimate of the prediction, counterfactual comparison, or slope.
\item \code{std.error}: standard errors computed via the delta method.
\item \code{p.value}: p value associated to the \code{estimate} column. The null is determined by the \code{hypothesis} argument (0 by default).
\item \code{s.value}: Shannon information transforms of p values. See the S values vignette at \url{https://marginaleffects.com} the marginaleffects website.
\item \code{conf.low}: lower bound of the confidence (or credible) interval defined by the \code{conf_level} argument.
\item \code{conf.high}: upper bound of the confidence (or credible) interval defined by the \code{conf_level} argument.
\item \code{predicted_lo}: predicted outcome for the "low" value of the focal predictor in a counterfactual comparison.
\item \code{predicted_hi}: predicted outcome for the "high" value of the focal predictor in a counterfactual comparison.
}

See \code{?print.marginaleffects} for printing options.

The \code{data.frame}s produced by \code{marginaleffects} come with several attributes to hold the original model, data, and much other information that can be used for post-processing. These attributes are not considered part of the public API of the package. Their names and contents can change without warning or notice. Users should not rely on them.

Depending on the model, this information can use up a substantial amount of memory. To avoid saving these attributes, users can set a global option. Note that setting this option may result in a loss of functionality, such as the ability to call \code{hypotheses()} to post-process estimates.

\code{options(marginaleffects_lean = TRUE)}
}
\description{
Predict the outcome variable at different regressor values (e.g., college
graduates vs. others), and compare those predictions by computing a difference,
ratio, or some other function. \code{comparisons()} can return many quantities of
interest, such as contrasts, differences, risk ratios, changes in log odds, lift,
slopes, elasticities, etc.
\itemize{
\item \code{comparisons()}: unit-level (conditional) estimates.
\item \code{avg_comparisons()}: average (marginal) estimates.
}

\code{variables} identifies the focal regressors whose "effect" we are interested in. \code{comparison} determines how predictions with different regressor values are compared (difference, ratio, odds, etc.). The \code{newdata} argument and the \code{datagrid()} function control where statistics are evaluated in the predictor space: "at observed values", "at the mean", "at representative values", etc.

See the comparisons chapter on the package website for worked examples and case studies:
\itemize{
\item \url{https://marginaleffects.com/chapters/comparisons.html}
\item \url{https://marginaleffects.com/}
}
}
\section{Functions}{
\itemize{
\item \code{avg_comparisons()}: Average comparisons

}}
\section{Standard errors using the delta method}{


Standard errors for all quantities estimated by \code{marginaleffects} can be obtained via the delta method. This requires differentiating a function with respect to the coefficients in the model using a finite difference approach. In some models, the delta method standard errors can be sensitive to various aspects of the numeric differentiation strategy, including the step size. By default, the step size is set to \code{1e-8}, or to \code{1e-4} times the smallest absolute model coefficient, whichever is largest.

\code{marginaleffects} can delegate numeric differentiation to the \code{numDeriv} package, which allows more flexibility. To do this, users can pass arguments to the \code{numDeriv::jacobian} function through a global option. For example:
\itemize{
\item \code{options(marginaleffects_numDeriv = list(method = "simple", method.args = list(eps = 1e-6)))}
\item \code{options(marginaleffects_numDeriv = list(method = "Richardson", method.args = list(eps = 1e-5)))}
\item \code{options(marginaleffects_numDeriv = NULL)}
}

See the "Uncertainty" chapter on the \code{marginaleffects} website for more details on the computation of standard errors, bootstrapping, and more:

https://marginaleffects.com/chapters/uncertainty.html
}

\section{Model-Specific Arguments}{


Some model types allow model-specific arguments to modify the nature of
marginal effects, predictions, marginal means, and contrasts. Please report
other package-specific \code{predict()} arguments on Github so we can add them to
the table below.

https://github.com/vincentarelbundock/marginaleffects/issues\tabular{llll}{
   Package \tab Class \tab Argument \tab Documentation \cr
   \code{brms} \tab \code{brmsfit} \tab \code{ndraws} \tab \link[brms:posterior_predict.brmsfit]{brms::posterior_predict} \cr
    \tab  \tab \code{re_formula} \tab \link[brms:posterior_predict.brmsfit]{brms::posterior_predict} \cr
   \code{lme4} \tab \code{merMod} \tab \code{re.form} \tab \link[lme4:predict.merMod]{lme4::predict.merMod} \cr
    \tab  \tab \code{allow.new.levels} \tab \link[lme4:predict.merMod]{lme4::predict.merMod} \cr
   \code{glmmTMB} \tab \code{glmmTMB} \tab \code{re.form} \tab \link[glmmTMB:predict.glmmTMB]{glmmTMB::predict.glmmTMB} \cr
    \tab  \tab \code{allow.new.levels} \tab \link[glmmTMB:predict.glmmTMB]{glmmTMB::predict.glmmTMB} \cr
    \tab  \tab \code{zitype} \tab \link[glmmTMB:predict.glmmTMB]{glmmTMB::predict.glmmTMB} \cr
   \code{mgcv} \tab \code{bam} \tab \code{exclude} \tab \link[mgcv:predict.bam]{mgcv::predict.bam} \cr
    \tab \code{gam} \tab \code{exclude} \tab \link[mgcv:predict.gam]{mgcv::predict.gam} \cr
   \code{robustlmm} \tab \code{rlmerMod} \tab \code{re.form} \tab \link[robustlmm:rlmerMod-class]{robustlmm::predict.rlmerMod} \cr
    \tab  \tab \code{allow.new.levels} \tab \link[robustlmm:rlmerMod-class]{robustlmm::predict.rlmerMod} \cr
   \code{MCMCglmm} \tab \code{MCMCglmm} \tab \code{ndraws} \tab  \cr
   \code{sampleSelection} \tab \code{selection} \tab \code{part} \tab \link[sampleSelection:predict.selection]{sampleSelection::predict.selection} \cr
}
}

\section{comparison argument functions}{


The following transformations can be applied by supplying one of the shortcut strings to the
\code{comparison} argument.
\code{hi} is a vector of adjusted predictions for the "high" side of the
contrast. \code{lo} is a vector of adjusted predictions for the "low" side of the
contrast. \code{y} is a vector of adjusted predictions for the original data. \code{x}
is the predictor in the original data. \code{eps} is the step size to use to
compute derivatives and elasticities.\tabular{ll}{
   Shortcut \tab Function \cr
   difference \tab \(hi, lo) hi - lo \cr
   differenceavg \tab \(hi, lo) mean(hi - lo) \cr
   dydx \tab \(hi, lo, eps) (hi - lo)/eps \cr
   eyex \tab \(hi, lo, eps, y, x) (hi - lo)/eps * (x/y) \cr
   eydx \tab \(hi, lo, eps, y, x) ((hi - lo)/eps)/y \cr
   dyex \tab \(hi, lo, eps, x) ((hi - lo)/eps) * x \cr
   dydxavg \tab \(hi, lo, eps) mean((hi - lo)/eps) \cr
   eyexavg \tab \(hi, lo, eps, y, x) mean((hi - lo)/eps * (x/y)) \cr
   eydxavg \tab \(hi, lo, eps, y, x) mean(((hi - lo)/eps)/y) \cr
   dyexavg \tab \(hi, lo, eps, x) mean(((hi - lo)/eps) * x) \cr
   ratio \tab \(hi, lo) hi/lo \cr
   ratioavg \tab \(hi, lo) mean(hi)/mean(lo) \cr
   lnratio \tab \(hi, lo) log(hi/lo) \cr
   lnratioavg \tab \(hi, lo) log(mean(hi)/mean(lo)) \cr
   lnor \tab \(hi, lo) log((hi/(1 - hi))/(lo/(1 - lo))) \cr
   lnoravg \tab \(hi, lo) log((mean(hi)/(1 - mean(hi)))/(mean(lo)/(1 - mean(lo)))) \cr
   lift \tab \(hi, lo) (hi - lo)/lo \cr
   liftavg \tab \(hi, lo) (mean(hi - lo))/mean(lo) \cr
   expdydx \tab \(hi, lo, eps) ((exp(hi) - exp(lo))/exp(eps))/eps \cr
   expdydxavg \tab \(hi, lo, eps) mean(((exp(hi) - exp(lo))/exp(eps))/eps) \cr
}
}

\section{Bayesian posterior summaries}{


By default, credible intervals in bayesian models are built as equal-tailed
intervals. This can be changed to a highest density interval by setting a global
option:

\code{options("marginaleffects_posterior_interval" = "eti")}

\code{options("marginaleffects_posterior_interval" = "hdi")}

By default, the center of the posterior distribution in bayesian models is
identified by the median. Users can use a different summary function by setting a
global option:

\code{options("marginaleffects_posterior_center" = "mean")}

\code{options("marginaleffects_posterior_center" = "median")}

When estimates are averaged using the \code{by} argument, the \code{tidy()} function, or
the \code{summary()} function, the posterior distribution is marginalized twice over.
First, we take the average \emph{across} units but \emph{within} each iteration of the
MCMC chain, according to what the user requested in \code{by} argument or
\code{tidy()/summary()} functions. Then, we identify the center of the resulting
posterior using the function supplied to the
\code{"marginaleffects_posterior_center"} option (the median by default).
}

\section{Equivalence, Inferiority, Superiority}{


\eqn{\theta} is an estimate, \eqn{\sigma_\theta} its estimated standard error, and \eqn{[a, b]} are the bounds of the interval supplied to the \code{equivalence} argument.

Non-inferiority:
\itemize{
\item \eqn{H_0}{H0}: \eqn{\theta \leq a}{\theta <= a}
\item \eqn{H_1}{H1}: \eqn{\theta > a}
\item \eqn{t=(\theta - a)/\sigma_\theta}{t=(\theta - a)/\sigma_\theta}
\item p: Upper-tail probability
}

Non-superiority:
\itemize{
\item \eqn{H_0}{H0}: \eqn{\theta \geq b}{\theta >= b}
\item \eqn{H_1}{H1}: \eqn{\theta < b}
\item \eqn{t=(\theta - b)/\sigma_\theta}{t=(\theta - b)/\sigma_\theta}
\item p: Lower-tail probability
}

Equivalence: Two One-Sided Tests (TOST)
\itemize{
\item p: Maximum of the non-inferiority and non-superiority p values.
}

Thanks to Russell V. Lenth for the excellent \code{emmeans} package and documentation which inspired this feature.
}

\section{Prediction types}{


The \code{type} argument determines the scale of the predictions used to compute quantities of interest with functions from the \code{marginaleffects} package. Admissible values for \code{type} depend on the model object. When users specify an incorrect value for \code{type}, \code{marginaleffects} will raise an informative error with a list of valid \code{type} values for the specific model object. The first entry in the list in that error message is the default type.

The \code{invlink(link)} is a special type defined by \code{marginaleffects}. It is available for some (but not all) models, and only for the \code{predictions()} function. With this link type, we first compute predictions on the link scale, then we use the inverse link function to backtransform the predictions to the response scale. This is useful for models with non-linear link functions as it can ensure that confidence intervals stay within desirable bounds, ex: 0 to 1 for a logit model. Note that an average of estimates with \code{type="invlink(link)"} will not always be equivalent to the average of estimates with \code{type="response"}. This type is default when calling \code{predictions()}. It is available---but not default---when calling \code{avg_predictions()} or \code{predictions()} with the \code{by} argument.

Some of the most common \code{type} values are:

response, link, E, Ep, average, class, conditional, count, cum.prob, cumhaz, cumprob, density, detection, disp, ev, expected, expvalue, fitted, hazard, invlink(link), latent, latent_N, linear, linear.predictor, linpred, location, lp, mean, numeric, p, ppd, pr, precision, prediction, prob, probability, probs, quantile, risk, rmst, scale, survival, unconditional, utility, variance, xb, zero, zlink, zprob
}

\section{Order of operations}{


Behind the scenes, the arguments of \code{marginaleffects} functions are evaluated in this order:
\enumerate{
\item \code{newdata}
\item \code{variables}
\item \code{comparison} and \code{slope}
\item \code{by}
\item \code{vcov}
\item \code{hypothesis}
\item \code{transform}
}
}

\section{Parallel computation}{


The \code{slopes()} and \code{comparisons()} functions can use parallelism to
speed up computation. Operations are parallelized for the computation of
standard errors, at the model coefficient level. There is always
considerable overhead when using parallel computation, mainly involved
in passing the whole dataset to the different processes. Thus, parallel
computation is most likely to be useful when the model includes many parameters
and the dataset is relatively small.

Warning: In many cases, parallel processing will not be useful at all.

To activate parallel computation, users must load the \code{future.apply} package,
call \code{plan()} function, and set a global option.

\code{options(marginaleffects_parallel = TRUE)}: parallelize delta method computation of standard errors.
\code{options(marginaleffects_parallel_inferences = TRUE)}: parallelize \code{"rsample"} or \code{"fwb"} bootstrap computation in \code{inferences()}.
\code{options(marginaleffects_parallel_packages = TRUE)}: vector of strings with the names of modeling packages used to fit the model, ex: c("survival", "splines")

For example:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{library(future.apply)
plan("multisession", workers = 4)
options(marginaleffects_parallel = FALSE)
options(marginaleffects_parallel_inferences = TRUE)
options(marginaleffects_parallel_packages = c("survival", "splines"))

slopes(model)
}\if{html}{\out{</div>}}

To disable parallelism in \code{marginaleffects} altogether, you can set a global option:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{options(marginaleffects_parallel = FALSE)
}\if{html}{\out{</div>}}
}

\section{Global options}{


The behavior of \code{marginaleffects} functions can be modified by setting global options.

Disable some safety checks and warnings:
\itemize{
\item \code{options(marginaleffects_startup_message = FALSE)}
\itemize{
\item Disable the startup message printed on \code{library(marginaleffects)}.
}
\item \code{options(marginaleffects_safe = FALSE)}
\itemize{
\item Disable safety checks and warnings.
}
\item \code{options(marginaleffects_print_omit = c("p.value", "s.value"))}
\itemize{
\item Omit some columns from the printed output.
}
}

Enforce lean return objects, sans information about the original model and
data, and other ancillary attributes. Note that this will disable some
advanced post-processing features and functions like \link{hypotheses}.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{options(marginaleffects_lean = TRUE)
}\if{html}{\out{</div>}}

Other options:
\itemize{
\item \code{marginaleffects_plot_gray}: logical. If \code{TRUE}, the default color of the plot is gray. Default is \code{FALSE}.
}
}

\examples{
\dontshow{if (interactive() || isTRUE(Sys.getenv("R_DOC_BUILD") == "true")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(marginaleffects)

# Linear model
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am + factor(cyl), tmp)
avg_comparisons(mod, variables = list(cyl = "reference"))
avg_comparisons(mod, variables = list(cyl = "sequential"))
avg_comparisons(mod, variables = list(cyl = "pairwise"))

# GLM with different scale types
mod <- glm(am ~ factor(gear), data = mtcars)
avg_comparisons(mod, type = "response")
avg_comparisons(mod, type = "link")

# Contrasts at the mean
comparisons(mod, newdata = "mean")

# Contrasts between marginal means
comparisons(mod, newdata = "balanced")

# Contrasts at user-specified values
comparisons(mod, newdata = datagrid(am = 0, gear = tmp$gear))
comparisons(mod, newdata = datagrid(am = unique, gear = max))

m <- lm(mpg ~ hp + drat + factor(cyl) + factor(am), data = mtcars)
comparisons(m, variables = "hp", newdata = datagrid(FUN_factor = unique, FUN_numeric = median))

# Numeric contrasts
mod <- lm(mpg ~ hp, data = mtcars)
avg_comparisons(mod, variables = list(hp = 1))
avg_comparisons(mod, variables = list(hp = 5))
avg_comparisons(mod, variables = list(hp = c(90, 100)))
avg_comparisons(mod, variables = list(hp = "iqr"))
avg_comparisons(mod, variables = list(hp = "sd"))
avg_comparisons(mod, variables = list(hp = "minmax"))

# using a function to specify a custom difference in one regressor
dat <- mtcars
dat$new_hp <- 49 * (dat$hp - min(dat$hp)) / (max(dat$hp) - min(dat$hp)) + 1
modlog <- lm(mpg ~ log(new_hp) + factor(cyl), data = dat)
fdiff <- function(x) data.frame(x, x + 10)
avg_comparisons(modlog, variables = list(new_hp = fdiff))

# Adjusted Risk Ratio
mod <- glm(vs ~ mpg, data = mtcars, family = binomial)
avg_comparisons(mod, comparison = "lnratioavg", transform = exp)

# Adjusted Risk Ratio: Manual specification of the `comparison`
avg_comparisons(
  mod,
  comparison = function(hi, lo) log(mean(hi) / mean(lo)),
  transform = exp)
# cross contrasts
mod <- lm(mpg ~ factor(cyl) * factor(gear) + hp, data = mtcars)
avg_comparisons(mod, variables = c("cyl", "gear"), cross = TRUE)

# variable-specific contrasts
avg_comparisons(mod, variables = list(gear = "sequential", hp = 10))

# hypothesis test: is the `hp` marginal effect at the mean equal to the `drat` marginal effect
mod <- lm(mpg ~ wt + drat, data = mtcars)

comparisons(
  mod,
  newdata = "mean",
  hypothesis = "wt = drat")

# same hypothesis test using row indices
comparisons(
  mod,
  newdata = "mean",
  hypothesis = "b1 - b2 = 0")

# same hypothesis test using numeric vector of weights
comparisons(
  mod,
  newdata = "mean",
  hypothesis = c(1, -1))

# two custom contrasts using a matrix of weights
lc <- matrix(
  c(
    1, -1,
    2, 3),
  ncol = 2)
comparisons(
  mod,
  newdata = "mean",
  hypothesis = lc)

# Effect of a 1 group-wise standard deviation change
# First we calculate the SD in each group of `cyl`
# Second, we use that SD as the treatment size in the `variables` argument
library(dplyr)
mod <- lm(mpg ~ hp + factor(cyl), mtcars)
tmp <- mtcars \%>\%
  group_by(cyl) \%>\%
  mutate(hp_sd = sd(hp))
avg_comparisons(mod,
  variables = list(hp = function(x) data.frame(x, x + tmp$hp_sd)),
  by = "cyl")

# `by` argument
mod <- lm(mpg ~ hp * am * vs, data = mtcars)
comparisons(mod, by = TRUE)

mod <- lm(mpg ~ hp * am * vs, data = mtcars)
avg_comparisons(mod, variables = "hp", by = c("vs", "am"))

library(nnet)
mod <- multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)
by <- data.frame(
  group = c("3", "4", "5"),
  by = c("3,4", "3,4", "5"))
comparisons(mod, type = "probs", by = by)
\dontshow{\}) # examplesIf}
}
\references{
\itemize{
\item Arel-Bundock V, Greifer N, Heiss A (2024). “How to Interpret Statistical Models Using marginaleffects for R and Python.” \emph{Journal of Statistical Software}, \emph{111}(9), 1-32. doi:10.18637/jss.v111.i09 \doi{10.18637/jss.v111.i09}
\item Greenland S. 2019. "Valid P-Values Behave Exactly as They Should: Some Misleading Criticisms of P-Values and Their Resolution With S-Values." The American Statistician. 73(S1): 106–114.
\item Cole, Stephen R, Jessie K Edwards, and Sander Greenland. 2020. "Surprise!" American Journal of Epidemiology 190 (2): 191–93. \doi{10.1093/aje/kwaa136}
}
}
