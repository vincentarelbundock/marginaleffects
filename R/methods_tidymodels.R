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


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.workflow <- function(model, coefs, ...) {
    if ("fit" %in% names(model) && "fit" %in% names(model$fit)) {
        model$fit$fit <- set_coef(model$fit$fit, coefs, ...)
    }
    return(model)
}


#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.model_fit <- function(model, newdata, type = NULL, ...) {
    out <- stats::predict(model, new_data = newdata, type = type)

    if (type == "numeric") {
        v <- intersect(c(".pred", ".pred_res"), colnames(out))[1]
        out <- data.frame(rowid = seq_len(nrow(out)), estimate = out[[v]])
    } else if (type == "class") {
        out <- data.frame(
            rowid = seq_len(nrow(out)),
            estimate = out[[".pred_class"]]
        )
    } else if (type == "prob") {
        colnames(out) <- substr(colnames(out), 7, nchar(colnames(out)))
        out$rowid <- seq_len(nrow(out))
        data.table::setDT(out)
        out <- data.table::melt(
            out,
            id.vars = "rowid",
            variable.name = "group",
            value.name = "estimate"
        )
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
#' @keywords internal
#' @export
get_vcov.model_fit <- function(model, type = NULL, ...) {
    if (isTRUE(type == "class")) {
        return(FALSE)
    }
    vcov <- sanitize_vcov(model, vcov)
    if (isTRUE(supported_engine(model))) {
        tmp <- parsnip::extract_fit_engine(model)
        out <- get_vcov(tmp)
    } else {
        out <- FALSE
    }
    return(out)
}


#' @include get_vcov.R
#' @rdname get_vcov
#' @keywords internal
#' @export
get_vcov.workflow <- get_vcov.model_fit


#' @include get_coef.R
#' @rdname get_coef
#' @keywords internal
#' @export
get_coef.workflow <- function(model, ...) {
    if (isTRUE(supported_engine(model))) {
        tmp <- parsnip::extract_fit_engine(model)
        out <- get_coef(tmp)
    } else {
        out <- NULL
    }
    return(out)
}
