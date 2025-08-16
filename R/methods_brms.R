#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.brmsfit <- function(model, ...) {
    insight::check_if_installed("collapse", minimum_version = "1.9.0")
    # terms: brmsfit objects do not have terms immediately available
    te <- tryCatch(
        attr(stats::terms(stats::formula(model)$formula), "term.labels"),
        error = function(e) NULL
    )
    if (any(grepl("^factor\\(", te))) {
        stop(
            "The `factor()` function cannot be used in the model formula of a `brmsfit` model. Please convert your variable to a factor before fitting the model, or use the `mo()` function to specify monotonic variables (see the `brms` vignette on monotonic variables).",
            call. = FALSE
        )
    }
    return(model)
}


#' @rdname get_coef
#' @export
get_coef.brmsfit <- function(model, ...) {
    out <- insight::get_parameters(model)
    out <- collapse::dapply(out, MARGIN = 2, FUN = collapse::fmedian)
    return(out)
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.brmsfit <- function(
    model,
    newdata = insight::get_data(model),
    type = "response",
    ...
) {
    checkmate::assert_choice(
        type,
        choices = c("response", "link", "prediction", "average")
    )

    if (type == "link") {
        insight::check_if_installed("rstantools")
        draws <- rstantools::posterior_linpred(
            model,
            newdata = newdata,
            ...
        )
    } else if (type == "response") {
        insight::check_if_installed("rstantools")
        draws <- rstantools::posterior_epred(
            model,
            newdata = newdata,
            ...
        )
    } else if (type == "prediction") {
        insight::check_if_installed("rstantools")
        draws <- rstantools::posterior_predict(
            model,
            newdata = newdata,
            ...
        )
    } else if (type == "average") {
        insight::check_if_installed("brms")
        draws <- brms::pp_average(
            model,
            newdata = newdata,
            summary = FALSE,
            ...
        )
    }

    if ("rowid_internal" %in% colnames(newdata)) {
        idx <- newdata[["rowid_internal"]]
    } else if ("rowid" %in% colnames(newdata)) {
        idx <- newdata[["rowid"]]
    } else {
        idx <- seq_len(nrow(newdata))
    }

    # resp_subset sometimes causes dimension mismatch
    if (length(dim(draws)) == 2 && nrow(newdata) != ncol(draws)) {
        msg <- sprintf(
            "Dimension mismatch: There are %s parameters in the posterior draws but %s observations in `newdata` (or the original dataset).",
            ncol(draws),
            nrow(newdata)
        )
        stop_sprintf(msg)
    }

    # 1d outcome
    if (length(dim(draws)) == 2) {
        med <- collapse::dapply(draws, MARGIN = 2, FUN = collapse::fmedian)
        out <- data.frame(
            rowid = idx,
            group = "main_marginaleffect",
            estimate = med
        )

        # multi-dimensional outcome
    } else if (length(dim(draws)) == 3) {
        out <- apply(draws, c(2, 3), stats::median)
        levnames <- dimnames(draws)[[3]]
        if (is.null(levnames)) {
            colnames(out) <- seq_len(ncol(out))
        } else {
            colnames(out) <- levnames
        }
        out <- data.frame(
            rowid = rep(idx, times = ncol(out)),
            group = rep(colnames(out), each = nrow(out)),
            estimate = c(out)
        )
        out$group <- group_to_factor(out$group, model)
    } else {
        stop(
            "marginaleffects cannot extract posterior draws from this model. Please report this problem to the Bug tracker with a reporducible example: https://github.com/vincentarelbundock/marginaleffects/issues",
            call. = FALSE
        )
    }

    # group for multi-valued outcome
    if (length(dim(draws)) == 3) {
        draws <- lapply(seq_len(dim(draws)[3]), function(i) draws[,, i])
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
get_vcov.brmsfit <- function(model, vcov = NULL, ...) {
    if (!is.null(vcov) && !is.logical(vcov)) {
        warn_sprintf(
            "The `vcov` argument is not supported for models of this class."
        )
    }
    vcov <- sanitize_vcov(model, vcov)
    return(NULL)
}
