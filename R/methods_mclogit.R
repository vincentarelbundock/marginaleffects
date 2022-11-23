#' @rdname get_group_names
#' @export
get_group_names.mblogit <- function(model, type, ...) {
    out <- get_predict(model, type = type)
    if ("group" %in% colnames(out)) {
        out <- unique(out$group)
    } else {
        out <- "main_marginaleffects"
    }
    return(out)
}

#' @include sanity_model.R
#' @rdname sanity_model_specific
#' @export
sanity_model_specific.mblogit <- function(model, calling_function = "marginaleffects", ...) {
    if (calling_function == "marginaleffects") {
        variables <- insight::find_variables(model, flatten = TRUE)
        dat <- insight::get_data(model)
        dat <- dat[, intersect(variables, colnames(dat))]
        flag <- any(sapply(dat, is.character))
        if (isTRUE(flag)) {
            stop("Cannot compute marginal effects for models of class `mblogit` when the data includes character variables. Please convert character variables to factors in the dataset before fitting the model, and call `marginaleffects` again.", call. = FALSE)
        }
    }
}


#' @rdname get_predict
#' @export
get_predict.mblogit <- function(model,
                                newdata = insight::get_data(model),
                                vcov = FALSE,
                                conf_level = 0.95,
                                type = "response",
                                ...) {

    if (!isTRUE(checkmate::check_flag(vcov, null.ok = TRUE)) &&
        isTRUE(list(...)$calling_function == "predictions")) {
        stop("The `vcov` argument is not supported for this model class.", call. = FALSE)
    }

    out <- suppressMessages(
        get_predict.multinom(model = model,
                             newdata = newdata,
                             type = type,
                             ...))
    return(out)
}


#' @rdname get_coef
#' @export
get_coef.mblogit <- function(model, ...) {
    # get_coef.default uses `insight::get_parameters`, but does not combine
    # Response and Parameter columns. This also matches naming scheme from
    # `vcov` 
    stats::coef(model)
}