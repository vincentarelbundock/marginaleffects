#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.betareg <- function(model, coefs, ...) {
    # coefs are split between mean coefs (which can be length 0) and precision coefs
    # (which must be length > 0 and always start with "(phi)_" due to get_coef.betareg(),
    # to match with get_varcov(., component = "all") output). In betareg object, these
    # are stored as two elements in a list, with precision coefs lacking the "(phi)_"
    # prefix, so we remove it.
    mu <- !grepl("\\(phi\\)|^Log\\(nu\\)", names(coefs))
    if (length(mu) > 0) {
        if ("mean" %in% names(model$coefficients)) {
            model[["coefficients"]][["mean"]] <- coefs[mu]
        } else if ("mu" %in% names(model$coefficients)) {
            model[["coefficients"]][["mu"]] <- coefs[mu]
        }
    }

    phi <- startsWith(names(coefs), "(phi)")
    if (length(phi) > 0) {
        if ("precision" %in% names(model$coefficients)) {
            model[["coefficients"]][["precision"]] <- coefs[phi]
        } else if ("phi" %in% names(model$coefficients)) {
            model[["coefficients"]][["phi"]] <- coefs[phi]
        }
    }

    nu <- startsWith(names(coefs), "Log(nu)")
    if (length(nu) > 0 && "nu" %in% names(model[["coefficients"]])) {
        model[["coefficients"]][["nu"]] <- coefs[nu]
    }

    return(model)
}

#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.betareg <- function(model, ...) {
    out <- model$coefficients
    for (n in names(out)) {
        if (n == "phi") {
            names(out[[n]]) <- sprintf("(phi)_%s", names(out[[n]]))
        }
    }
    out <- stats::setNames(unlist(out), unlist(lapply(out, names)))
    return(out)
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.betareg <- function(model, newdata, type = "response", ...) {
    dots <- list(...)
    args <- list(
        model,
        newdata = newdata,
        type = type
    )
    args[["at"]] <- dots[["at"]]
    out <- do.call(stats::predict, args)
    out <- data.table(estimate = out)
    out <- add_rowid(out, newdata)
    return(out)
}


#' @rdname sanitize_model_specific
sanitize_model_specific.betareg <- function(model, ...) {
    insight::check_if_installed("insight", minimum_version = "0.17.1")
    return(model)
}
