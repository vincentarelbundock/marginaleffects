#' List of model types supported by the `marginaleffects` package
#'
#' A list of model types supported by the `marginaleffects` package
#'
#' @format data.frame
#' \describe{
#'   \item{Package}{R package which produces the model object.}
#'   \item{Function}{R function which produces the model object.}
#'   \item{dydx}{Can `marginaleffects` retrieve marginal effects?}
#'   \item{se}{Can `marginaleffects` retrieve standard errors?}
#'   \item{stata_dydx}{Has the validity of numerical results for marginal effects been checked against Stata's margins command?}
#'   \item{stata_se}{Has the validity of numerical results for standard errors been checked against Stata's margins command?}
#'   \item{stata_dydx}{Has the validity of numerical results for marginal effects been checked against the margins package for R??}
#'   \item{stata_se}{Has the validity of numerical results for standard errors been checked against the margins package for R??}
#' }
"supported_models"
