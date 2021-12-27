#' @rdname get_coef
#' @export
get_coef.brmsfit <- function(model, ...) {
    out <- insight::get_parameters(model)
    out <- apply(out, 2, stats::median)
    return(out)
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.brmsfit <- function(model,
                                newdata = insight::get_data(model),
                                type = "response",
                                ...) {

    assert_dependency("rstantools")

    if ("include_random" %in% names(list(...))) {
        stop("The `include_random` argument is not available for models of this class. Consider using the `re.form` argument, which will be pushed forward to the `posterior_linpred`, `posterior_epred`, or `posterior_predict` function.")
    }

    checkmate::assert_choice(type, choices = c("response", "link", "prediction"))

    if (type == "link") {
        draws <- rstantools::posterior_linpred(
            model,
            newdata = newdata,
            ...)
    } else if (type == "response") {
        draws <- rstantools::posterior_epred(
            model,
            newdata = newdata,
            ...)
    } else if (type == "prediction") {
        draws <- rstantools::posterior_predict(
            model,
            newdata = newdata,
            ...)
    }

    if ("rowid_internal" %in% colnames(newdata)) {
        idx <- newdata[["rowid_internal"]]
    } else if ("rowid" %in% colnames(newdata)) {
        idx <- newdata[["rowid"]]
    } else {
        idx <- 1:nrow(newdata)
    }

    # 1d outcome
    if (length(dim(draws)) == 2) {
        out <- data.frame(
            rowid = idx,
            group = "main_marginaleffect",
            predicted = apply(draws, 2, stats::median))

    # multi-dimensional outcome
    } else if (length(dim(draws)) == 3) {
        out <- apply(draws, c(2, 3), stats::median)
        colnames(out) <- dimnames(draws)[[3]]
        out <- data.frame(
            rowid = rep(idx, times = ncol(out)),
            group = rep(colnames(out), each = nrow(out)),
            predicted = c(out))
    } else {
        stop("marginaleffects cannot extract posterior draws from this model. Please report this problem to the Bug tracker with a reporducible example: https://github.com/vincentarelbundock/marginaleffects/issues")
    }

    # group for multi-valued outcome
    if (length(dim(draws)) == 3) {
        draws <- lapply(1:dim(draws)[3], function(i) draws[, , i])
        draws <- do.call("cbind", draws)
    }
    attr(out, "posterior_draws") <- t(draws)

    return(out)
}


#' @include get_group_names.R
#' @rdname get_group_names
#' @export
get_group_names.brmsfit <- function(model, ...) {
    if (!is.null(model$family) && "cumulative" %in% model$family) {
        out <- unique(insight::get_response(model))
    } else {
        out <- "main_marginaleffect"
    }
    return(out)
}
