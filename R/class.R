#' Define class unions
#' @keywords internal
#' @noRd
setClassUnion("matrixOrNULL", c("matrix", "NULL"))

#' @keywords internal
#' @noRd
setClassUnion("numericOrNULL", c("numeric", "NULL"))

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
#' @slot jacobian The jacobian matrix or NULL
#' @keywords internal
setClass(
    "marginaleffects_internal",
    slots = c(
        call = "ANY",
        conf_level = "ANY",
        df = "ANY",
        draws = "matrixOrNULL",
        hypothesis = "ANY",
        hypothesis_null = "ANY",
        hypothesis_direction = "ANY",
        jacobian = "matrixOrNULL",
        model = "ANY",
        modeldata = "ANY", # TODO: lmerTest returns nfnGroupedData
        newdata = "data.frame",
        type = "ANY",
        variables = "list",
        vcov_model = "ANY",
        wts = "numericOrNULL"
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
                                         conf_level = 0.95,
                                         df = NULL,
                                         draws = NULL,
                                         hypothesis = NULL,
                                         hypothesis_null = NULL,
                                         hypothesis_direction = NULL,
                                         jacobian = NULL,
                                         modeldata = data.frame(),
                                         newdata = data.frame(),
                                         type = NULL,
                                         variables = list(),
                                         vcov_model = NULL,
                                         wts = NULL) {
    new("marginaleffects_internal",
        call = call,
        conf_level = conf_level,
        df = df,
        draws = draws,
        hypothesis = hypothesis,
        hypothesis_null = hypothesis_null,
        hypothesis_direction = hypothesis_direction,
        jacobian = jacobian,
        model = model,
        modeldata = modeldata,
        newdata = newdata,
        type = type,
        variables = variables,
        vcov_model = vcov_model,
        wts = wts)
}
