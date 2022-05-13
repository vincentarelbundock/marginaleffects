get_contrasts <- function(model,
                          newdata,
                          type,
                          variables,
                          transform_pre,
                          contrast_factor,
                          contrast_numeric,
                          cache = NULL,
                          eps = 1e-4,
                          marginalmeans,
                          ...) {

    dots <- list(...)

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

    pred_lo <- try(get_predict(
        model,
        type = type,
        vcov = FALSE,
        newdata = lo,
        ...), silent = TRUE)

    pred_hi <- try(get_predict(
        model,
        type = type,
        vcov = FALSE,
        newdata = hi,
        ...), silent = TRUE)

    if (inherits(pred_hi, "try-error") && inherits(pred_lo, "try-error")) {
        stop("Unable to compute adjusted predictions for this model. Either the `newdata` does not meet the requirements of the model's `predict()` method, or this model is not supported. If you believe this model should be supported, you can file a report on the Github Issue Tracker: https://github.com/vincentarelbundock/marginaleffects/issues", call. = FALSE)
    }

    # bayes
    draws_lo <- attr(pred_lo, "posterior_draws")
    draws_hi <- attr(pred_hi, "posterior_draws")
    if (is.null(draws_lo)) {
        draws <- NULL
    } else if (!is.null(transform_pre)) {
        stop("The `transform_pre` argument is not supported for Bayesian models.", call. = FALSE)
    } else {
        draws <- draws_hi - draws_lo
    }

    out <- pred_lo
    setDT(out)

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

    # sanitize_transform_pre returns NULL by default to allow us to warn
    # when the argument is not supported
    if (is.null(transform_pre)) {
        transform_pre <- function(hi, lo) hi - lo
    }

    idx <- grep("^contrast|^group$|^term$", colnames(out), value = TRUE)
    out[, predicted_lo := pred_lo$predicted]
    out[, predicted_hi := pred_hi$predicted]
    if (isTRUE(marginalmeans)) {
        out <- out[, .(predicted_lo = mean(predicted_lo), predicted_hi = mean(predicted_hi)), by = idx]
        out[, "comparison" := transform_pre(predicted_hi, predicted_lo)]
        out[, c("predicted_hi", "predicted_lo") := NULL]

    } else {
        wrapfun <- function(hi, lo, n) {
            con <- try(transform_pre(hi, lo), silent = TRUE)
            if (!isTRUE(checkmate::check_numeric(con, len = n)) &&
                !isTRUE(checkmate::check_numeric(con, len = 1))) {
                msg <- sprintf("The function supplied to the `transform_pre` argument must accept two numeric vectors of predicted probabilities of length %s, and return a single numeric value or a numeric vector of length %s, with no missing value.", n, n)
                stop(msg, call. = FALSE)
            }
            return(con)
        }
        out[, "comparison" := wrapfun(predicted_hi, predicted_lo, .N), by = idx]
        out[, c("predicted_hi", "predicted_lo", "predicted") := NULL]
    }

    # normalize slope
    # not available for interactions
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
