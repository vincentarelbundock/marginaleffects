#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.hurdle <- function(model, coefs, ...) {
    # in pscl::hurdle, coefficients are held in a named list:
    # model$coefficients. Each element of the list is a vector which
    # corresponds to one equation (e.g., "zero" or "response"). When calling
    # "coef(model)", the equation label is prefixed to the term name with an
    # underscore.
    out <- model
    for (lab in names(out$coefficients)) {
        idx <- paste0(lab, "_", names(out$coefficients[[lab]]))
        idx <- match(idx, names(coefs))
        # probably too conservative
        if (anyNA(idx)) {
            stop(
                "Mismatched coefficients names. Please check the `marginaleffects::`set_coef.hurdle` or `set_coef.zeroinfl` function.",
                call. = FALSE
            )
        }
        out$coefficients[[lab]] <- stats::setNames(
            coefs[idx],
            names(out$coefficients[[lab]])
        )
    }
    return(out)
}


#' @rdname set_coef
#' @export
set_coef.zeroinfl <- set_coef.hurdle


#' @rdname get_group_names
#' @export
get_group_names.hurdle <- function(model, type = "count", ...) {
    if (type == "prob") {
        out <- colnames(stats::predict(model, type = "prob"))
    } else {
        out <- "main_marginaleffect"
    }
    return(out)
}
