#' Define class unions
#' @keywords internal
#' @noRd
setClassUnion("matrixOrNULL", c("matrix", "NULL"))

#' @keywords internal
#' @noRd
setClassUnion("numericOrNULL", c("numeric", "NULL"))

#' @keywords internal
#' @noRd
setClassUnion("characterOrNULL", c("character", "NULL"))

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
#' @slot calling_function The name of the calling function (comparisons, predictions, hypotheses)
#' @slot df The degrees of freedom
#' @slot wts The weights specification
#' @slot type The sanitized type from sanitize_type()
#' @slot jacobian The jacobian matrix or NULL
#' @keywords internal
setClass(
    "marginaleffects_internal",
    slots = c(
        call = "ANY",
        calling_function = "character",
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
        numderiv = "list",
        type = "ANY",
        variables = "list",
        variable_names_response = "characterOrNULL",
        vcov_model = "ANY",
        wts = "numericOrNULL"
    )
)

#' Constructor for marginaleffects_internal class
#'
#' @param model The fitted model object (required)
#' @param call The original function call (required)
#' @param newdata The new data frame for predictions
#' @param vcov_model The variance-covariance matrix
#' @param df The degrees of freedom
#' @param wts The weights specification
#' @param type The sanitized type from sanitize_type()
#' @return An object of class marginaleffects_internal
#' @keywords internal
new_marginaleffects_internal <- function(
    model,
    call,
    conf_level = 0.95,
    df = NULL,
    draws = NULL,
    hypothesis = NULL,
    hypothesis_null = NULL,
    hypothesis_direction = NULL,
    jacobian = NULL,
    newdata = data.frame(),
    numderiv = list("fdforward"),
    type = NULL,
    variables = list(),
    vcov_model = NULL,
    wts = NULL) {
    modeldata <- get_modeldata(model, additional_variables = TRUE)

    variable_names_response <- hush(unlist(
        insight::find_response(model, combine = TRUE, component = "all"),
        use.names = FALSE
    ))

    # Extract calling function from call
    calling_function <- extract_calling_function(call)

    new(
        "marginaleffects_internal",
        call = call,
        calling_function = calling_function,
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
        numderiv = numderiv,
        type = type,
        variables = variables,
        variable_names_response = variable_names_response,
        vcov_model = vcov_model,
        wts = wts
    )
}

#' Extract calling function name from call object
#'
#' @param call The call object from which to extract the function name
#' @return Character string of the calling function ("comparisons", "predictions", or "hypotheses")
#' @keywords internal
extract_calling_function <- function(call) {
    if (is.call(call)) {
        func_name <- as.character(call[[1]])
        # Handle namespaced calls like marginaleffects::predictions
        if (length(func_name) > 1) {
            func_name <- func_name[length(func_name)]
        }
        # Map avg_ functions to their base function
        if (startsWith(func_name, "avg_")) {
            func_name <- sub("^avg_", "", func_name)
        }
        # Return the appropriate function name if it's one of the main functions
        if (func_name %in% c("comparisons", "predictions", "hypotheses")) {
            return(func_name)
        }
    }
    # Default fallback
    return("unknown")
}
