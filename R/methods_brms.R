#' @rdname get_coef
#' @export
get_coef.brmsfit <- function(model, ...) {
    out <- insight::get_parameters(model)
    out <- apply(out, 2, median)
    return(out)
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.brmsfit <- function(model,
                                newdata = insight::get_data(model),
                                type = "response",
                                group_name = NULL,
                                ...) {

    assert_dependency("rstantools")

    type <- match.arg(type, choices = c("response", "link", "prediction"))

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

    # 1d outcome
    if (length(dim(draws)) == 2) {
        out <- data.frame(
            rowid = 1:nrow(newdata),
            group = "main_marginaleffect",
            predicted = apply(draws, 2, stats::median))

    # multi-dimensional outcome
    } else if (length(dim(draws)) == 3) {
        out <- apply(draws, c(2, 3), stats::median)
        colnames(out) <- dimnames(draws)[[3]]
        if (!is.null(group_name)) {
            out <- out[, group_name, drop = TRUE]
            draws <- draws[, , match(group_name, dimnames(draws)[[3]])]
        }
    } else {
        stop("marginaleffects cannot extract posterior draws from this model. Please report this problem to the Bug tracker with a reporducible example: https://github.com/vincentarelbundock/marginaleffects/issues")
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
