get_contrasts <- function(model,
                          newdata = NULL,
                          type = "response",
                          variables = NULL,
                          contrast_factor = "reference",
                          contrast_numeric = 1,
                          cache = NULL,
                          eps = 1e-4,
                          ...) {

    # cache is efficient for the delta method Jacobian when we need to manipulate
    # the coefficients but don't need to rebuild the contrast data every time.
    if (is.null(cache)) {
        cache <- get_contrast_data(model,
                newdata = newdata,
                variables = variables,
                contrast_factor = contrast_factor,
                contrast_numeric = contrast_numeric,
                eps = eps,
                ...)
    }
    original <- cache[["original"]]
    lo <- cache[["lo"]]
    hi <- cache[["hi"]]

    # some predict() methods need data frames and will convert data.tables
    # internally, which can be very expensive if done many times. we do it once
    # here.
    setDF(lo)
    setDF(hi)

    pred_lo <- get_predict(
        model,
        type = type,
        vcov = FALSE,
        conf.level = NULL,
        newdata = lo,
        ...)

    pred_hi <- get_predict(
        model,
        type = type,
        vcov = FALSE,
        conf.level = NULL,
        newdata = hi,
        ...)

    pred_lo$term <- cache[["ter"]]
    pred_hi$term <- cache[["ter"]]

    draws_lo <- attr(pred_lo, "posterior_draws")
    draws_hi <- attr(pred_hi, "posterior_draws")
    if (is.null(draws_lo)) {
        draws <- NULL
    } else {
        # TODO: Transformation is not available
        if (isTRUE(is.function(dots[["transformation"]]))) {
            warning("The `transformation` argument is ignored for Bayesian models.")
        }
        draws <- draws_hi - draws_lo
    }

    out <- pred_lo
    setDT(out)

    dots <- list(...)
    if (isTRUE(is.function(dots[["transformation"]]))) {
        fun <- dots[["transformation"]]
    } else {
        fun <- function(hi, lo) hi - lo
    }

    con <- try(fun(pred_hi[["predicted"]], pred_lo[["predicted"]]), silent = TRUE)
    if (!isTRUE(checkmate::check_numeric(con, len = nrow(out)))) {
        msg <- sprintf("The function supplied to the `transformation` argument must accept two numeric vectors of predicted probabilities of length %s, and return a single numeric vector of the same length.", nrow(out))
        stop(msg, call. = FALSE)
    }
    out[, "comparison" := con]
    out[, "predicted" := NULL]

    # univariate outcome:
    # original is the "composite" data that we constructed by binding terms and
    # compute predictions. It includes a term column, which we need to
    # replicate for each group.
    mult <- nrow(out) / nrow(original)
    if (isTRUE(mult == 1)) {
        out[, "term" := original[["term"]]]
        out[, "contrast" := original[["contrast"]]]

    # group or multivariate outcomes
    } else if (isTRUE(mult > 1)) {
        out[, "term" := rep(original$term, times = mult)]
        out[, "contrast" := rep(original$contrast, times = mult)]

    # cross-contrasts or weird cases
    } else {
        out <- merge(out, newdata, by = "rowid")
        if (isTRUE(nrow(out) == nrow(lo))) {
            tmp <- data.table(lo)[, .SD, .SDcols = patterns("^contrast")]
            out <- cbind(out, tmp)
            idx <- c("rowid", grep("^contrast", colnames(out), value = TRUE), colnames(out))
            idx <- unique(idx)
            out <- out[, ..idx]
        }
        out[, "term" := "interaction"]
    }

    # normalize slope
    # not available for cross-contrasts
    if ("contrast" %in% colnames(out)) {
        idx <- out$contrast == "dydx"
        out[idx == TRUE, "comparison" := comparison / eps]
        if (!is.null(draws)) {
            draws[idx == TRUE, ] <- draws[idx == TRUE, ] / eps
        }
    }

    # output
    attr(out, "posterior_draws") <- draws
    attr(out, "original") <- cache[["original"]]
    return(out)
}
