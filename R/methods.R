#' @noRd

#' @noRd
#' @export
vcov.comparisons <- function(object, ...) {
    # align J and V: This might be a problematic hack, but I have not found examples yet.
    V <- components(object, "vcov_model")
    J <- components(object, "jacobian")
    aligned <- align_jacobian_vcov(J, V, object, ...)
    aligned$J %*% aligned$V %*% t(aligned$J)
}


#' @noRd
#' @export
vcov.predictions <- vcov.comparisons


#' @noRd
#' @export
vcov.hypotheses <- vcov.comparisons


#' @noRd
#' @export
vcov.slopes <- vcov.comparisons


#' @export
#' @noRd
coef.comparisons <- function(object, ...) {
    if (!is.null(object$estimate)) {
        out <- object$estimate
        if (is.null(names(out))) {
            lab <- tryCatch(get_labels(object), error = function(e) NULL)
            if (length(lab) == length(out)) {
                out <- stats::setNames(out, lab)
            }
        }
        return(out)
    } else {
        stop("The input object does not contain an 'estimate' element.")
    }
}


#' @export
#' @noRd
coef.slopes <- coef.comparisons


#' @export
#' @noRd
coef.predictions <- coef.comparisons


#' @export
#' @noRd
coef.hypotheses <- coef.comparisons


#' @export
#' @noRd
df.residual.comparisons <- function(object, ...) {
    out <- tryCatch(
        stats::df.residual(components(object, "model")),
        error = function(e) NULL
    )
    if (is.null(out)) out <- Inf
    return(out)
}


#' @export
#' @noRd
df.residual.predictions <- df.residual.comparisons


#' @export
#' @noRd
df.residual.slopes <- df.residual.comparisons



#' @noRd
#' @export
`[.marginaleffects` <- function(x, i, j, drop = FALSE) {
    out <- NextMethod() # dispatch to [.data.frame
    attr(out, "marginaleffects") <- attr(x, "marginaleffects", exact = TRUE)
    class(out) <- class(x)
    out
}

#' @noRd
#' @export
subset.marginaleffects <- function(x, subset, select, drop = FALSE, ...) {
    out <- NextMethod() # dispatch to subset.data.frame
    attr(out, "marginaleffects") <- attr(x, "marginaleffects", exact = TRUE)
    class(out) <- class(x)
    out
}

#' @noRd
#' @export
`[.predictions` <- `[.marginaleffects`

#' @noRd
#' @export
`[.comparisons` <- `[.marginaleffects`

#' @noRd
#' @export
`[.slopes` <- `[.marginaleffects`

#' @noRd
#' @export
`[.hypotheses` <- `[.marginaleffects`

#' @noRd
#' @export
subset.predictions <- subset.marginaleffects

#' @noRd
#' @export
subset.comparisons <- subset.marginaleffects

#' @noRd
#' @export
subset.slopes <- subset.marginaleffects

#' @noRd
#' @export
subset.hypotheses <- subset.marginaleffects
