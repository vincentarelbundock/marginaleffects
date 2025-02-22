#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.betareg <- function(model, coefs, ...) {
    # coefs are split between mean coefs (which can be length 0) and precision coefs
    # (which must be length > 0 and always start with "(phi)_" due to get_coef.betareg(),
    # to match with get_varcov(., component = "all") output). In betareg object, these
    # are stored as two elements in a list, with precision coefs lacking the "(phi)_"
    # prefix, so we remove it.
    mean_coefs <- coefs[names(coefs) != "(phi)" & !startsWith(names(coefs), "(phi)_")]
    precision_coefs <- coefs[names(coefs) == "(phi)" | startsWith(names(coefs), "(phi)_")]
    names(precision_coefs) <- sub("(phi)_", "", names(precision_coefs),
        fixed = TRUE)

    if (length(mean_coefs) > 0) {
        model[["coefficients"]]$mean[names(mean_coefs)] <- mean_coefs
    }
    model[["coefficients"]]$precision[names(precision_coefs)] <- precision_coefs

    model
}

#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.betareg <- function(model, ...) {
    mean_coefs <- model$coefficients$mean
    precision_coefs <- model$coefficients$precision

    # precision coefs have "(phi)_" appended to their names in covariance matrix; mean coefficients
    # never have this, so no risk of duplicate names, and precision coefs are always determined.
    # Mimicking coef.betareg(., "full").
    if (!identical(names(precision_coefs), "(phi)")) {
        names(precision_coefs) <- paste0("(phi)_", names(precision_coefs))
    }
    c(mean_coefs, precision_coefs)
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.betareg <- function(model, newdata, type = "response", at = 0.5, ...) {
    out <- stats::predict(model, newdata = newdata, type = type, at = at)
    out <- data.frame(
        rowid = seq_len(nrow(newdata)),
        estimate = out)
    return(out)
}


#' @rdname sanitize_model_specific
sanitize_model_specific.betareg <- function(model, ...) {
    insight::check_if_installed("insight", minimum_version = "0.17.1")
    return(model)
}
