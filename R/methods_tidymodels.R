supported_engine <- function(x) {
    insight::check_if_installed("parsnip")
    tmp <- parsnip::extract_fit_engine(x)
    flag <- inherits(try(sanitize_model(tmp), silent = TRUE), "try-error")
    return(!flag)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.model_fit <- function(model, coefs, ...) {

  if (!"fit" %in% names(model)) {
    return(model)
  }

  model$fit <- set_coef(model$fit, coefs, ...)

  return(model)
}



#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.model_fit <- function(model, newdata, type = NULL, ...) {
    out <- stats::predict(model, new_data = newdata, type = type)

    if (type == "numeric") {
        out <- data.frame(rowid = seq_along(out), estimate = out[[".pred"]])

    } else if (type == "class") {
        out <- data.frame(rowid = seq_along(out), estimate = out[[".pred_class"]])

    } else if (type == "prob") {
        colnames(out) <- substr(colnames(out), 7, nchar(colnames(out)))
        out$rowid <- seq_len(nrow(out))
        out <- melt(out, id.vars = "rowid", variable.name = "group", value.name = "estimate")
    }

    return(out)
}


#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.workflow <- get_predict.model_fit


#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.model_fit <- function(model, type = NULL, ...) {

    if (isTRUE(type == "class")) {
        return(FALSE)
    }

    if (isTRUE(supported_engine(model))) {
        tmp <- parsnip::extract_fit_engine(model)
        out <- get_vcov(tmp)
    } else {
        out <- FALSE
    }
    return(out)
}


#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_vcov.workflow <- get_vcov.model_fit