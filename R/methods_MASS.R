#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.polr <- function(model, ...) {
    out <- insight::get_parameters(model)
    out <- stats::setNames(out$Estimate, out$Parameter)
    names(out) <- gsub("Intercept: ", "", names(out))
    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.polr <- function(model, coefs) {
    # in basic model classes coefficients are named vector
    idx <- match(names(model$coefficients), names(coefs))
    model[["coefficients"]] <- coefs[idx]
    idx <- match(names(model$zeta), names(coefs))
    model[["zeta"]] <- coefs[idx]
    model
}


#' @include get_group_names.R
#' @rdname get_group_names
#' @export
get_group_names.polr <- function(model, ...) {
    resp <- insight::get_response(model)
    if (is.factor(resp)) {
        out <- levels(resp)
    } else {
        out <- unique(resp)
    }
    return(out)
}


#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.polr <- function(model, ...) {
    out <- suppressMessages(insight::get_varcov(model))
    return(out)
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.polr <- function(model, 
                             newdata = insight::get_data(model), 
                             type = "response", 
                             group_name = "1",
                             ...) {

    pred <- stats::predict(model, 
                           newdata = newdata, 
                           type = type)

    sanity_predict_numeric(pred = pred, model = model, newdata = newdata, type = type)

    # numDeriv expects a vector
    if (is.matrix(pred) && (!is.null(group_name) && group_name != "main_marginaleffect")) {
        pred <- pred[, group_name, drop = TRUE]
    }

    return(pred)
}

