% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_coef.R, R/methods_MASS.R,
%   R/methods_WeightIt.R, R/methods_afex.R, R/methods_betareg.R,
%   R/methods_nnet.R, R/methods_brglm2.R, R/methods_brms.R,
%   R/methods_dataframe.R, R/methods_gamlss.R, R/methods_glmmTMB.R,
%   R/methods_glmtoolbox.R, R/methods_lme4.R, R/methods_mclogit.R,
%   R/methods_mgcv.R, R/methods_mlm.R, R/methods_sampleSelection.R,
%   R/methods_scam.R, R/methods_stats.R, R/methods_survey.R,
%   R/methods_systemfit.R, R/methods_tidymodels.R
\name{get_coef}
\alias{get_coef}
\alias{get_coef.default}
\alias{get_coef.polr}
\alias{get_coef.multinom_weightit}
\alias{get_coef.afex_aov}
\alias{get_coef.betareg}
\alias{get_coef.multinom}
\alias{get_coef.brmultinom}
\alias{get_coef.bracl}
\alias{get_coef.brmsfit}
\alias{get_coef.data.frame}
\alias{get_coef.gamlss}
\alias{get_coef.glmmTMB}
\alias{get_coef.glmgee}
\alias{get_coef.merMod}
\alias{get_coef.lmerModLmerTest}
\alias{get_coef.lmerMod}
\alias{get_coef.mblogit}
\alias{get_coef.gam}
\alias{get_coef.mlm}
\alias{get_coef.selection}
\alias{get_coef.scam}
\alias{get_coef.nls}
\alias{get_coef.svyolr}
\alias{get_coef.systemfit}
\alias{get_coef.workflow}
\title{Get a named vector of coefficients from a model object}
\usage{
get_coef(model, ...)

\method{get_coef}{default}(model, ...)

\method{get_coef}{polr}(model, ...)

\method{get_coef}{multinom_weightit}(model, ...)

\method{get_coef}{afex_aov}(model, ...)

\method{get_coef}{betareg}(model, ...)

\method{get_coef}{multinom}(model, ...)

\method{get_coef}{brmultinom}(model, ...)

\method{get_coef}{bracl}(model, ...)

\method{get_coef}{brmsfit}(model, ...)

\method{get_coef}{data.frame}(model, ...)

\method{get_coef}{gamlss}(model, ...)

\method{get_coef}{glmmTMB}(model, ...)

\method{get_coef}{glmgee}(model, ...)

\method{get_coef}{merMod}(model, ...)

\method{get_coef}{lmerModLmerTest}(model, ...)

\method{get_coef}{lmerMod}(model, ...)

\method{get_coef}{mblogit}(model, ...)

\method{get_coef}{gam}(model, ...)

\method{get_coef}{mlm}(model, ...)

\method{get_coef}{selection}(model, ...)

\method{get_coef}{scam}(model, ...)

\method{get_coef}{nls}(model, ...)

\method{get_coef}{svyolr}(model, ...)

\method{get_coef}{systemfit}(model, ...)

\method{get_coef}{workflow}(model, ...)
}
\arguments{
\item{model}{Model object}

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
A named vector of coefficients. The names must match those of the variance matrix.
}
\description{
Mostly for internal use, but can be useful because the output is consistent across model classes.
}
\keyword{internal}
