#' @include sanity_model.R
#' @rdname sanity_model_specific
#' @export
sanity_model_specific.brmsfit <- function(model, ...) {
    # terms: brmsfit objects do not have terms immediately available
    te <- tryCatch(attr(stats::terms(stats::formula(model)$formula), "term.labels"), error = function(e) NULL)
    if (any(grepl("^factor\\(", te))) {
        stop("The `factor()` function cannot be used in the model formula of a `brmsfit` model. Please convert your variable to a factor before fitting the model, or use the `mo()` function to specify monotonic variables (see the `brms` vignette on monotonic variables).",
             call. = FALSE)
    }
}


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
                                vcov = FALSE,
                                conf_level = 0.95,
                                type = "response",
                                ...) {

    assert_dependency("rstantools")

    checkmate::assert_choice(type, choices = c("response", "link", "prediction", "average"))

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
    } else if (type == "average") {
        draws <- brms::pp_average(
            model,
            newdata = newdata,
            summary = FALSE,
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
        stop("marginaleffects cannot extract posterior draws from this model. Please report this problem to the Bug tracker with a reporducible example: https://github.com/vincentarelbundock/marginaleffects/issues", call. = FALSE)
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


#' @rdname get_vcov
#' @export
get_vcov.brmsfit <- function(model,
                             vcov = NULL,
                             ...) {
    if (!is.null(vcov) && !is.logical(vcov)) {
        msg <- "The `vcov` argument is not supported for models of this class."
        warning(msg, call. = FALSE)
    }
    return(NULL)
}

