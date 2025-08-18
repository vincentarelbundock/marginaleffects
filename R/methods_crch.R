#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.crch <- function(model, coefs, ...) {
    # coefs are split between location coefs (which can be length 0) and scale coefs
    # (which must be length > 0 and always start with "(scale)_" due to get_parameters(),
    # to match with get_varcov(., component = "all") output). In crch object, these
    # are stored as two elements in a list, with scale coefs lacking the "(scale)_"
    # prefix, so we remove it.
    location_coefs <- coefs[!startsWith(names(coefs), "(scale)_")]
    scale_coefs <- coefs[startsWith(names(coefs), "(scale)_")]
    names(scale_coefs) <- sub("(scale)_", "", names(scale_coefs), fixed = TRUE)

    if (length(location_coefs) > 0) {
        model[["coefficients"]]$location[names(location_coefs)] <- location_coefs
    }
    model[["coefficients"]]$scale[names(scale_coefs)] <- scale_coefs

    model
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.crch <- function(model, newdata = NULL, type = "location", ...) {
    pred <- stats::predict(model, newdata = newdata, type = type)
    sanity_predict_vector(
        pred = pred,
        model = model,
        newdata = newdata,
        type = type
    )
    sanity_predict_numeric(
        pred = pred,
        model = model,
        newdata = newdata,
        type = type
    )
    out <- data.table(estimate = pred)
    out <- add_rowid(out, newdata)
    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.hxlr <- function(model, coefs, ...) {
    # in crch::hxlr, coefficients are held in:
    # model$coefficients$intercept
    # model$coefficients$location
    # model$coefficients$scale
    #
    # note: there are no prefixes in coef() output, so coefs may have same name
    out <- model

    idx_int <- length(model$coefficients$intercept)
    idx_loc <- length(model$coefficients$location)

    out$coefficients$intercept[] <- coefs[1:idx_int]
    out$coefficients$location[] <- coefs[idx_int + 1:idx_loc]
    out$coefficients$scale[] <- coefs[(idx_int + idx_loc + 1):length(coefs)]

    return(out)
}


#' @export
#' @noRd
get_predict.hxlr <- get_predict.crch
