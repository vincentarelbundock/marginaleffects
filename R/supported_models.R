#' List of model types supported by the `marginaleffects` package
#'
#' A list of model types supported by the `marginaleffects` package
#'
#' @format data.frame
#' \describe{
#'   \item{Model}{package::function of the model type}
#'   \item{Support: Effect}{Can `marginaleffects` retrieve marginal effects?}
#'   \item{Support: Std.Errors}{Can `marginaleffects` retrieve standard errors?}
#'   \item{Validty: Stata}{Has the validity of numerical results been checked against Stata's margins command?}
#'   \item{Validty: margins}{Has the validity of numerical results been checked against R's margins package?}
#' }
"supported_models"
