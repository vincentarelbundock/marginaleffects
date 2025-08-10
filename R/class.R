#' Internal S4 class for marginaleffects
#'
#' This S4 class is used internally to hold common arguments passed between
#' functions to simplify the function signatures and reduce redundant argument passing.
#'
#' @slot model The fitted model object
#' @slot modeldata The model data frame
#' @slot newdata The new data frame for predictions
#' @slot vcov_model The variance-covariance matrix
#' @keywords internal
setClass(
    "marginaleffects_internal",
    slots = c(
        model = "ANY",
        modeldata = "ANY", # TODO: lmerTest returns nfnGroupedData
        newdata = "data.frame",
        vcov_model = "ANY"
    )
)

#' Constructor for marginaleffects_internal class
#'
#' @param model The fitted model object
#' @param modeldata The model data frame
#' @param newdata The new data frame for predictions
#' @param vcov_model The variance-covariance matrix
#' @return An object of class marginaleffects_internal
#' @keywords internal
new_marginaleffects_internal <- function(model,
                                         modeldata = data.frame(),
                                         newdata = data.frame(),
                                         vcov_model = NULL) {
    new("marginaleffects_internal",
        model = model,
        modeldata = modeldata,
        newdata = newdata,
        vcov_model = vcov_model)
}
