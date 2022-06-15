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
                          hypothesis = NULL,
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

    pred_lo <- myTryCatch(get_predict(
        model,
        type = type,
        vcov = FALSE,
        newdata = lo,
        ...))

    pred_hi <- myTryCatch(get_predict(
        model,
        type = type,
        vcov = FALSE,
        newdata = hi,
        ...))

    if (inherits(pred_hi$value, "data.frame")) pred_hi <- pred_hi$value
    if (inherits(pred_lo$value, "data.frame")) pred_lo <- pred_lo$value

    if (!inherits(pred_hi, "data.frame") || !inherits(pred_lo, "data.frame")) {
        msg <- format_msg(paste(
        "Unable to compute adjusted predictions for this model. Either the
        `newdata` does not meet the requirements of the model's `predict()`
        method, or this model is not supported. If you believe this model
        should be supported, you can file a report on the Github Issue Tracker:
        https://github.com/vincentarelbundock/marginaleffects/issues"))
        if (!is.null(pred_hi$error)) {
            msg <- paste(msg, "\n\nIn addition:", pred_lo$error)
        }
        stop(msg, call. = FALSE)
    }

    # output data.frame
    out <- pred_lo
    setDT(out)


    # univariate outcome:
    # original is the "composite" data that we constructed by binding terms and
    # compute predictions. It includes a term column, which we need to
    # replicate for each group.
    out[, "eps_tmp" := NA_real_]
    mult <- nrow(out) / nrow(original)
    if (isTRUE(mult == 1)) {
        out[, "term" := original[["term"]]]
        out[, "contrast" := original[["contrast"]]]
        if ("eps" %in% colnames(original)) {
            out[, "eps_tmp" := original[["eps"]]]
        }

    # group or multivariate outcomes
    } else if (isTRUE(mult > 1)) {
        out[, "term" := rep(original$term, times = mult)]
        out[, "contrast" := rep(original$contrast, times = mult)]
        if ("eps" %in% colnames(original)) {
            out[, "eps_tmp" := rep(original$eps, times = mult)]
        }

    # cross-contrasts or weird cases
    } else {
        out <- merge(out, newdata, by = "rowid")
        if (isTRUE(nrow(out) == nrow(lo))) {
            tmp <- data.table(lo)[, .SD, .SDcols = patterns("^contrast|eps")]
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
        # slope not supported with interactions, where we have `contrast_var1`, `contrast_var2` columns
        if ("contrast" %in% colnames(out)) {
            out[, "transform_pre_idx" := fifelse(contrast == "dydx", 1, 2)]
            transform_pre_list <- list(
                function(hi, lo, eps, ...) (hi - lo) / eps,
                function(hi, lo, ...) hi - lo)
        } else {
            out[, "transform_pre_idx" := 1]
            transform_pre_list <- list(
            function(hi, lo, ...) hi - lo)
        }
    # only 1 transformation supported when explicitly called
    } else {
        out[, "transform_pre_idx" := 1]
        if (!"eps" %in% names(formals(transform_pre))) {
            transform_pre_list <- list(
                function(hi, lo, ...) transform_pre(hi, lo, ...)
            )
        } else {
            transform_pre_list <- list(
                function(hi, lo, eps, ...) transform_pre(hi, lo, eps, ...)
            )
        }
    }
    
    # bayes
    draws_lo <- attr(pred_lo, "posterior_draws")
    draws_hi <- attr(pred_hi, "posterior_draws")
    if (is.null(draws_lo)) {
        draws <- NULL
    } else {
        draws <- draws_lo
        for (i in seq_along(transform_pre_list)) {
            idx <- out[["transform_pre_idx"]] == i
            if (any(idx)) {
                f <- transform_pre_list[[i]]
                if ("eps" %in% names(formals(f))) {
                    draws[idx, ] <- f(draws_hi[idx, ], draws_lo[idx, ], eps = out[idx][["eps_tmp"]])
                } else {
                    draws[idx, ] <- f(draws_hi[idx, ], draws_lo[idx, ])
                }
            }
        }
    }


    idx <- grep("^contrast|^group$|^term$|^transform_pre_idx$", colnames(out), value = TRUE)
    out[, predicted_lo := pred_lo$predicted]
    out[, predicted_hi := pred_hi$predicted]
    if (isTRUE(marginalmeans)) {
        out <- out[, .(predicted_lo = mean(predicted_lo), predicted_hi = mean(predicted_hi), eps = mean(eps_tmp)), by = idx]
        out[, "comparison" := transform_pre_list[[transform_pre_idx[1]]](
            out$predicted_hi, out$predicted_lo, out$eps_tmp
        ), by = "term"]
        out[, c("predicted_hi", "predicted_lo") := NULL]

    } else {
        wrapfun <- function(hi, lo, n, transform_pre_idx, eps_tmp) {
            f <- transform_pre_list[[transform_pre_idx[1]]]
            if ("eps" %in% names(formals(f))) {
                con <- try(f(hi, lo, eps = eps_tmp), silent = TRUE)
            } else {
                con <- try(f(hi, lo), silent = TRUE)
            }
            if (!isTRUE(checkmate::check_numeric(con, len = n)) &&
                !isTRUE(checkmate::check_numeric(con, len = 1))) {
                msg <- format_msg(
                "The function supplied to the `transform_pre` argument must accept two numeric
                vectors of predicted probabilities of length %s, and return a single numeric
                value or a numeric vector of length %s, with no missing value.")
                msg <- sprintf(msg, n, n)
                stop(msg, call. = FALSE)
            }
            return(con)
        }
        tmp <- out[, .(comparison = wrapfun(
            hi = predicted_hi,
            lo = predicted_lo, 
            n = .N,
            transform_pre_idx = transform_pre_idx,
            eps_tmp = eps_tmp)),
            by = idx]
        if (nrow(tmp) != nrow(out)) {
            out <- tmp
        } else {
            out[, "comparison" := tmp$comparison]
        }
    }

    out <- get_hypothesis(out, hypothesis, "comparison")

    idx <- which(!colnames(out) %in% c("transform_pre_idx", "predicted_hi", "predicted_lo", "predicted"))
    out <- out[, ..idx]

    # output
    attr(out, "posterior_draws") <- draws
    attr(out, "original") <- cache[["original"]]
    return(out)
}

