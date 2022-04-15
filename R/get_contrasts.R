get_contrast_data <- function(model,
                              newdata = NULL,
                              variables = NULL,
                              contrast_factor = "reference",
                              contrast_numeric = 1,
                              ...) {

    lo <- hi <- ter <- lab <- original <- rowid <- list()

    for (v in variables) {
        # logical and character before factor because they get picked up by find_categorical
        variable_class <- find_variable_class(variable = v, newdata = newdata, model = model)

        if (variable_class == "logical") {
            tmp <- get_contrast_data_logical(
                model,
                newdata,
                v,
                ...)
        } else if (variable_class == "factor") {
            tmp <- get_contrast_data_factor(
                model,
                newdata,
                v,
                contrast_factor = contrast_factor,
                ...)
        } else if (variable_class == "numeric") {
            tmp <- get_contrast_data_numeric(
                model,
                newdata,
                v,
                contrast_numeric = contrast_numeric,
                ...)
        } else if (variable_class == "character") {
            tmp <- get_contrast_data_character(
                model,
                newdata,
                v,
                ...)
        } else {
            stop("variable class not supported.")
        }

        lo <- c(lo, list(tmp$lo))
        hi <- c(hi, list(tmp$hi))
        ter <- c(ter, list(tmp$ter))
        lab <- c(lab, list(tmp$lab))
        original <- c(original, list(tmp$original))
        rowid <- c(rowid, list(tmp$rowid))
    }

    lo <- rbindlist(lo)
    hi <- rbindlist(hi)
    original <- rbindlist(original)
    ter <- unlist(ter)
    lab <- unlist(lab)
    lo[, "term" := ter]
    hi[, "term" := ter]
    original[, "term" := ter]
    lo[, "contrast" := lab]
    hi[, "contrast" := lab]
    original[, "contrast" := lab]

    out <- list(lo = lo, hi = hi, original = original)

    return(out)
}


get_contrasts <- function(model,
                          newdata = NULL,
                          type = "response",
                          variables = NULL,
                          contrast_factor = "reference",
                          contrast_numeric = 1,
                          cache = NULL,
                          ...) {

    # cache is efficient for the delta method Jacobian when we need to manipulate
    # the coefficients but don't need to rebuild the contrast data every time.
    if (is.null(cache)) {
        cache <- get_contrast_data(model,
                newdata = newdata,
                variables = variables,
                contrast_factor = contrast_factor,
                contrast_numeric = contrast_numeric,
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
        draws <- draws_hi - draws_lo
    }

    out <- pred_lo
    setDT(out)

    out[, "comparison" := pred_hi$predicted - predicted]
    out[, "predicted" := NULL]

    # univariate outcome:
    # original is the "composite" data that we constructed by binding terms and
    # compute predictions. It includes a term column, which we need to
    # replicate for each group.
    if (nrow(out) == nrow(original)) {
        out[, "term" := original[["term"]]]
        out[, "contrast" := original[["contrast"]]]

    # group or multivariate outcomes
    } else {
        mult <- nrow(out) / nrow(original)
        if (mult > 1) {
            out[, "term" := rep(original$term, times = mult)]
            out[, "contrast" := rep(original$contrast, times = mult)]
        }
    }

    # normalize slope
    eps <- getOption("marginaleffects_deriv_eps", default = 0.0001)
    idx <- out$contrast == "dydx"
    out[idx == TRUE, "comparison" := comparison / eps]
    if (!is.null(draws)) {
        draws[idx == TRUE, ] <- draws[idx == TRUE, ] / eps
    }

    # output
    attr(out, "posterior_draws") <- draws
    attr(out, "original") <- cache[["original"]]
    return(out)
}
