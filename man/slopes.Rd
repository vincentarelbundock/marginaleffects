% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slopes.R
\name{slopes}
\alias{slopes}
\alias{avg_slopes}
\title{Slopes (aka Partial derivatives, Marginal Effects, or Trends)}
\usage{
slopes(
  model,
  newdata = NULL,
  variables = NULL,
  type = NULL,
  by = FALSE,
  vcov = TRUE,
  conf_level = 0.95,
  slope = "dydx",
  wts = FALSE,
  hypothesis = NULL,
  equivalence = NULL,
  df = Inf,
  eps = NULL,
  numderiv = "fdforward",
  ...
)

avg_slopes(
  model,
  newdata = NULL,
  variables = NULL,
  type = NULL,
  by = TRUE,
  vcov = TRUE,
  conf_level = 0.95,
  slope = "dydx",
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

\item{newdata}{Grid of predictor values at which we evaluate the slopes.
\itemize{
\item Warning: Please avoid modifying your dataset between fitting the model and calling a \code{marginaleffects} function. This can sometimes lead to unexpected results.
\item \code{NULL} (default): Unit-level slopes for each observed value in the dataset (empirical distribution). The dataset is retrieved using \code{\link[insight:get_data]{insight::get_data()}}, which tries to extract data from the environment. This may produce unexpected results if the original data frame has been altered since fitting the model.
\item \code{\link[=datagrid]{datagrid()}} call to specify a custom grid of regressors. For example:
\itemize{
\item \code{newdata = datagrid(cyl = c(4, 6))}: \code{cyl} variable equal to 4 and 6 and other regressors fixed at their means or modes.
\item See the Examples section and the \code{\link[=datagrid]{datagrid()}} documentation.
}
\item \code{\link[=subset]{subset()}} call with a single argument to select a subset of the dataset used to fit the model, ex: \code{newdata = subset(treatment == 1)}
\item \code{\link[dplyr:filter]{dplyr::filter()}} call with a single argument to select a subset of the dataset used to fit the model, ex: \code{newdata = filter(treatment == 1)}
\item string:
\itemize{
\item "mean": Slopes evaluated when each predictor is held at its mean or mode.
\item "median": Slopes evaluated when each predictor is held at its median or mode.
\item "balanced": Slopes evaluated on a balanced grid with every combination of categories and numeric variables held at their means.
\item "tukey": Slopes evaluated at Tukey's 5 numbers.
\item "grid": Slopes evaluated on a grid of representative numbers (Tukey's 5 numbers and unique values of categorical predictors).
}
}}

\item{variables}{Focal variables
\itemize{
\item \code{NULL}: compute slopes or comparisons for all the variables in the model object (can be slow).
\item Character vector: subset of variables (usually faster).
}}

\item{type}{string indicates the type (scale) of the predictions used to
compute contrasts or slopes. This can differ based on the model
type, but will typically be a string such as: "response", "link", "probs",
or "zero". When an unsupported string is entered, the model-specific list of
acceptable values is returned in an error message. When \code{type} is \code{NULL}, the
first entry in the error message is used by default.}

\item{by}{Aggregate unit-level estimates (aka, marginalize, average over). Valid inputs:
\itemize{
\item \code{FALSE}: return the original unit-level estimates.
\item \code{TRUE}: aggregate estimates for each term.
\item Character vector of column names in \code{newdata} or in the data frame produced by calling the function without the \code{by} argument.
\item Data frame with a \code{by} column of group labels, and merging columns shared by \code{newdata} or the data frame produced by calling the same function without the \code{by} argument.
\item See examples below.
\item For more complex aggregations, you can use the \code{FUN} argument of the \code{hypotheses()} function. See that function's documentation and the Hypothesis Test vignettes on the \code{marginaleffects} website.
}}

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

\item{conf_level}{numeric value between 0 and 1. Confidence level to use to build a confidence interval.}

\item{slope}{string indicates the type of slope or (semi-)elasticity to compute:
\itemize{
\item "dydx": dY/dX
\item "eyex": dY/dX * Y / X
\item "eydx": dY/dX * Y
\item "dyex": dY/dX / X
\item Y is the predicted value of the outcome; X is the observed value of the predictor.
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
Partial derivative of the regression equation with respect to a regressor of interest.
\itemize{
\item \code{slopes()}: unit-level (conditional) estimates.
\item \code{avg_slopes()}: average (marginal) estimates.
}

The \code{newdata} argument and the \code{datagrid()} function can be used to control where statistics are evaluated in the predictor space: "at observed values", "at the mean", "at representative values", etc.

See the slopes vignette and package website for worked examples and case studies:
\itemize{
\item \url{https://marginaleffects.com/chapters/slopes.html}
\item \url{https://marginaleffects.com/}
}

Warning: Slopes and elasticities can only be calculated for continuous numeric variables. The \code{slopes()} functions will automatically revert to \code{comparisons()} for binary or categorical variables.
}
\details{
A "slope" or "marginal effect" is the partial derivative of the regression equation
with respect to a variable in the model. This function uses automatic
differentiation to compute slopes for a vast array of models,
including non-linear models with transformations (e.g., polynomials).
Uncertainty estimates are computed using the delta method.

Numerical derivatives for the \code{slopes} function are calculated
using a simple epsilon difference approach: \eqn{\partial Y / \partial X = (f(X + \varepsilon/2) - f(X-\varepsilon/2)) / \varepsilon}{dY/dX = (f(X + e/2) - f(X-e/2)) / e},
where f is the \code{predict()} method associated with the model class, and
\eqn{\varepsilon}{e} is determined by the \code{eps} argument.
}
\section{Functions}{
\itemize{
\item \code{avg_slopes()}: Average slopes

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
# Unit-level (conditional) Marginal Effects
mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
mfx <- slopes(mod)
head(mfx)

# Average Marginal Effect (AME)
avg_slopes(mod, by = TRUE)


# Marginal Effect at the Mean (MEM)
slopes(mod, newdata = datagrid())

# Marginal Effect at User-Specified Values
# Variables not explicitly included in `datagrid()` are held at their means
slopes(mod, newdata = datagrid(hp = c(100, 110)))

# Group-Average Marginal Effects (G-AME)
# Calculate marginal effects for each observation, and then take the average
# marginal effect within each subset of observations with different observed
# values for the `cyl` variable:
mod2 <- lm(mpg ~ hp * cyl, data = mtcars)
avg_slopes(mod2, variables = "hp", by = "cyl")

# Marginal Effects at User-Specified Values (counterfactual)
# Variables not explicitly included in `datagrid()` are held at their
# original values, and the whole dataset is duplicated once for each
# combination of the values in `datagrid()`
mfx <- slopes(mod,
    newdata = datagrid(
        hp = c(100, 110),
        grid_type = "counterfactual"))
head(mfx)

# Heteroskedasticity robust standard errors
mfx <- slopes(mod, vcov = sandwich::vcovHC(mod))
head(mfx)

# hypothesis test: is the `hp` marginal effect at the mean equal to the `drat` marginal effect
mod <- lm(mpg ~ wt + drat, data = mtcars)

slopes(
    mod,
    newdata = "mean",
    hypothesis = "wt = drat")

# same hypothesis test using row indices
slopes(
    mod,
    newdata = "mean",
    hypothesis = "b1 - b2 = 0")

# same hypothesis test using numeric vector of weights
slopes(
    mod,
    newdata = "mean",
    hypothesis = c(1, -1))

# two custom contrasts using a matrix of weights
lc <- matrix(
    c(
        1, -1,
        2, 3),
    ncol = 2)
colnames(lc) <- c("Contrast A", "Contrast B")
slopes(
    mod,
    newdata = "mean",
    hypothesis = lc)
\dontshow{\}) # examplesIf}
}
\references{
\itemize{
\item Arel-Bundock V, Greifer N, Heiss A (2024). “How to Interpret Statistical Models Using marginaleffects for R and Python.” \emph{Journal of Statistical Software}, \emph{111}(9), 1-32. doi:10.18637/jss.v111.i09 \doi{10.18637/jss.v111.i09}
\item Greenland S. 2019. "Valid P-Values Behave Exactly as They Should: Some Misleading Criticisms of P-Values and Their Resolution With S-Values." The American Statistician. 73(S1): 106–114.
\item Cole, Stephen R, Jessie K Edwards, and Sander Greenland. 2020. "Surprise!" American Journal of Epidemiology 190 (2): 191–93. \doi{10.1093/aje/kwaa136}
}
}
