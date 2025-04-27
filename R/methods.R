#' @noRd
#' @export
vcov.comparisons <- function(object, ...) {
  # align J and V: This might be a problematic hack, but I have not found examples yet.
  V <- attr(object, "vcov")
  J <- attr(object, "jacobian")
  if (!isTRUE(ncol(J) == ncol(V))) {
    beta <- get_coef(object, ...)
    # Issue #718: ordinal::clm in test-pkg-ordinal.R
    if (
      anyNA(beta) &&
        anyDuplicated(names(beta)) &&
        ncol(J) > ncol(V) &&
        ncol(J) == length(beta) &&
        length(stats::na.omit(beta)) == ncol(V)
    ) {
      J <- J[, !is.na(beta), drop = FALSE]
    } else {
      cols <- intersect(colnames(J), colnames(V))
      if (length(cols) == 0) {
        insight::format_error(
          "The jacobian does not match the variance-covariance matrix."
        )
      }
      V <- V[cols, cols, drop = FALSE]
      J <- J[, cols, drop = FALSE]
    }
  }
  J %*% V %*% t(J)
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
    stats::df.residual(attr(object, "model")),
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
