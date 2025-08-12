#' Internal S4 class for marginaleffects
#'
#' This S4 class is used internally to hold common arguments passed between
#' functions to simplify the function signatures and reduce redundant argument passing.
#'
#' @slot model The fitted model object
#' @slot modeldata The model data frame
#' @slot newdata The new data frame for predictions
#' @slot vcov_model The variance-covariance matrix
#' @slot call The original function call
#' @slot df The degrees of freedom
#' @slot wts The weights specification
#' @slot type The sanitized type from sanitize_type()
#' @keywords internal
setClass(
    "marginaleffects_internal",
    slots = c(
        call = "ANY",
        df = "ANY",
        hypothesis_null = "ANY",
        hypothesis_direction = "ANY",
        model = "ANY",
        modeldata = "ANY", # TODO: lmerTest returns nfnGroupedData
        newdata = "data.frame",
        type = "ANY",
        variables = "list",
        vcov_model = "ANY",
        wts = "ANY"
    )
)

#' Constructor for marginaleffects_internal class
#'
#' @param model The fitted model object
#' @param modeldata The model data frame
#' @param newdata The new data frame for predictions
#' @param vcov_model The variance-covariance matrix
#' @param call The original function call
#' @param df The degrees of freedom
#' @param wts The weights specification
#' @param type The sanitized type from sanitize_type()
#' @return An object of class marginaleffects_internal
#' @keywords internal
new_marginaleffects_internal <- function(model = NULL,
                                         call = NULL,
                                         df = NULL,
                                         hypothesis_null = NULL,
                                         hypothesis_direction = NULL,
                                         modeldata = data.frame(),
                                         newdata = data.frame(),
                                         type = NULL,
                                         variables = list(),
                                         vcov_model = NULL,
                                         wts = FALSE) {
    new("marginaleffects_internal",
        call = call,
        df = df,
        hypothesis_null = hypothesis_null,
        hypothesis_direction = hypothesis_direction,
        model = model,
        modeldata = modeldata,
        newdata = newdata,
        type = type,
        vcov_model = vcov_model,
        wts = wts)
}
