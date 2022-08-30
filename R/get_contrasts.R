get_contrasts <- function(model,
                          newdata,
                          type,
                          variables,
                          original,
                          lo,
                          hi,
                          wts = NULL,
                          marginalmeans,
                          by = NULL,
                          hypothesis = NULL,
                          interaction = FALSE,
                          ...) {

    dots <- list(...)

    # some predict() methods need data frames and will convert data.tables
    # internally, which can be very expensive if done many times. we do it once
    # here.
    setDF(lo)
    setDF(hi)
    setDF(original)

    pred_lo <- myTryCatch(get_predict(
        model,
        type = type,
        vcov = FALSE,
        newdata = lo,
        ...))[["value"]]

    pred_hi <- myTryCatch(get_predict(
        model,
        type = type,
        vcov = FALSE,
        newdata = hi,
        ...))[["value"]]

    pred_or <- myTryCatch(get_predict(
        model,
        type = type,
        vcov = FALSE,
        newdata = original,
        ...))[["value"]]

    # lots of indexing later requires a data.table
    setDT(original)

    if (!inherits(pred_hi, "data.frame") || !inherits(pred_lo, "data.frame") || !inherits(pred_or, "data.frame")) {
        msg <- insight::format_message("Unable to compute adjusted predictions for this model. Either the `newdata` does not meet the requirements of the model's `predict()` method, or this model is not supported. If you believe this model should be supported, you can file a report on the Github Issue Tracker: https://github.com/vincentarelbundock/marginaleffects/issues")
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
    out[, "marginaleffects_eps" := NA_real_] # default (probably almost always overwritten)
    out[, "marginaleffects_wts_internal" := NA_real_] # default (probably almost always overwritten)
    mult <- nrow(out) / nrow(original)
    if (isTRUE(mult == 1)) {
        for (v in grep("^term$|^contrast|^marginaleffects_eps$|^marginaleffects_wts_internal$", colnames(original), value = TRUE)) {
            out[, (v) := original[[v]]]
        }

    # group or multivariate outcomes
    } else if (isTRUE(mult > 1)) {
        for (v in grep("^term$|^contrast|^marginaleffects_eps$|^marginaleffects_wts_internal$", colnames(original), value = TRUE)) {
            out[, (v) := rep(original[[v]], times = mult)]
        }

    # cross-contrasts or weird cases
    } else {
        out <- merge(out, newdata, by = "rowid")
        if (isTRUE(nrow(out) == nrow(lo))) {
            tmp <- data.table(lo)[, .SD, .SDcols = patterns("^contrast|marginaleffects_eps|marginaleffects_wts_internal")]
            out <- cbind(out, tmp)
            idx <- c("rowid", grep("^contrast", colnames(out), value = TRUE), colnames(out))
            idx <- unique(idx)
            out <- out[, ..idx]
        }
    }

    if (!"term" %in% colnames(out)) {
        out[, "term" := "interaction"]
    }

    # transform_pre function could be different for different terms
    # sanitize_variables() ensures all functions are identical when there are interactions
    fun_list <- sapply(names(variables), function(x) variables[[x]][["function"]])
    fun_list[["interaction"]] <- fun_list[[1]]

    # elasticity requires the original (properly aligned) predictor values
    # this will discard factor variables which are duplicated, so in principle
    # it should be the "correct" size
    elasticities <- c(
        "dydx",
        "eyex",
        "eydx",
        "dyex",
        "dydxavg",
        "eyexavg",
        "eydxavg",
        "dyexavg")
    elasticities <- Filter(
        function(x) is.character(x$transform_pre) && x$transform_pre %in% elasticities,
        variables)
    elasticities <- lapply(elasticities, function(x) x$name)
    if (length(elasticities) > 0) {
        for (v in names(elasticities)) {
            idx2 <- c("rowid", "term", "type", "group", grep("^contrast", colnames(out), value = TRUE))
            idx2 <- intersect(idx2, colnames(out))
            # discard other terms to get right length vector
            idx2 <- out[term == v, ..idx2]
            # original is NULL when interaction=TRUE
            if (!is.null(original)) {
                idx1 <- c(v, "rowid", "rowidcf", "term", "type", "group", grep("^contrast", colnames(original), value = TRUE))
                idx1 <- intersect(idx1, colnames(original))
                idx1 <- original[, ..idx1]
                idx2 <- merge(idx1, idx2)
            }
            elasticities[[v]] <- idx2[[v]]
        }
    }

    draws <- attr(pred_lo, "posterior_draws")

    # frequentist
    if (is.null(draws)) {
        draws_lo <- draws_hi <- draws_or <- NULL

    # bayes
    } else {
        draws_lo <- attr(pred_lo, "posterior_draws")
        draws_hi <- attr(pred_hi, "posterior_draws")
        draws_or <- attr(pred_or, "posterior_draws")
    }

    out[, predicted_lo := pred_lo[["predicted"]]]
    out[, predicted_hi := pred_hi[["predicted"]]]
    out[, predicted := pred_or[["predicted"]]]

    # the `by` variables must be included for group-by data.table operations
    if (!is.null(by)) {
        bycols <- sort(setdiff(
            unique(c(colnames(out), colnames(newdata))),
            c("rowid", "rowidcf", "predicted", "predicted_lo", "predicted_hi", "dydx", "comparison")))
        bycols <- paste(bycols, collapse = ", ")
        flagA1 <- checkmate::check_character(by)
        flagA2 <- checkmate::check_true(all(by %in% c(colnames(out), colnames(newdata))))
        flagB1 <- checkmate::check_data_frame(by)
        flagB2 <- checkmate::check_true("by" %in% colnames(by))
        flagB3 <- checkmate::check_true(all(setdiff(colnames(by), "by") %in% colnames(out)))

        if (!(isTRUE(flagA1) && isTRUE(flagA2)) && !(isTRUE(flagB1) && isTRUE(flagB2) && isTRUE(flagB3))) {
            msg <- c(
                "The `by` argument must be either:", "",
                sprintf("1. Character vector in which each element is part of: %s", bycols),
                "",
                sprintf("2. A data frame with a `by` column of labels, and in which all other columns are elements of: %s", bycols),
                "",
                "It can sometimes be useful to supply a data frame explicitly to the `newdata` argument in order to be able to group by all available columns."
             )
            stop(insight::format_message(msg), call. = FALSE)
        }

        # `by` data.frame
        if (isTRUE(checkmate::check_data_frame(by))) {
            idx <- setdiff(intersect(colnames(out), colnames(by)), "by")
            out[by, by := by, on = idx]
            bycols <- "by"

        # `by` vector
        } else {
            # don't double merge the weights
            bycols <- setdiff("marginaleffects_wts_internal", colnames(out)) 
            bycols <- c(bycols, "rowid", by)
            bycols <- intersect(bycols, colnames(newdata))
            tmp <- data.frame(newdata)[, bycols, drop = FALSE]
            out <- merge(out, tmp, by = "rowid", all.x = TRUE, sort = FALSE)
            bycols <- by
        }
    } else {
        bycols <- NULL
    }

    if ("by" %in% bycols) {
        idx <- "by"
    } else {
        idx <- grep("^contrast|^group$|^term$|^type$|^transform_pre_idx$", colnames(out), value = TRUE)
        idx <- unique(c(idx, bycols))
    }

    # we feed these columns to safefun(), even if they are useless for categoricals
    if (!"marginaleffects_eps" %in% colnames(out)) out[, "marginaleffects_eps" := NA]
    if (!"marginaleffects_wts_internal" %in% colnames(out))  out[, "marginaleffects_wts_internal" := NA]

    if (isTRUE(marginalmeans)) {
        out <- out[, .(
            predicted_lo = mean(predicted_lo),
            predicted_hi = mean(predicted_hi),
            predicted = mean(predicted),
            marginaleffects_eps = mean(marginaleffects_eps),
            marginaleffects_wts_internal = mean(marginaleffects_wts_internal)),
        by = idx]
    }

    # safe version of transform_pre
    # unknown arguments
    # singleton vs vector
    # different terms use different functions
    safefun <- function(hi, lo, y, n, term, interaction, eps, wts) {

        # when interaction=TRUE, sanitize_transform_pre enforces a single function
        if (isTRUE(interaction)) {
            fun <- fun_list[[1]]
        } else {
            fun <- fun_list[[term[1]]]
        }
        args <- list(
            "hi" = hi,
            "lo" = lo,
            "y" = y,
            "eps" = eps,
            "w" = wts,
            "x" = elasticities[[term[1]]])
        args <- args[names(args) %in% names(formals(fun))]
        con <- try(do.call("fun", args), silent = TRUE)
        if (!isTRUE(checkmate::check_numeric(con, len = n)) && !isTRUE(checkmate::check_numeric(con, len = 1))) {
            msg <- insight::format_message("The function supplied to the `transform_pre` argument must accept two numeric vectors of predicted probabilities of length %s, and return a single numeric value or a numeric vector of length %s, with no missing value.") #nolintr
            stop(sprintf(msg, n, n), call. = FALSE)
        }
        if (length(con) == 1) {
            con <- c(con, rep(NA_real_, length(hi) - 1))
        }
        return(con)
    }

    # bayesian
    if (!is.null(draws)) {
        term_names <- unique(out$term)
        # loop over columns (draws) and term names because different terms could use different functions 
        for (tn in term_names) {
            for (i in seq_len(ncol(draws))) {
                idx <- out$term == tn
                draws[idx, i] <- safefun(
                    hi = draws_hi[idx, i],
                    lo = draws_lo[idx, i],
                    y = draws_or[idx, i],
                    n = sum(idx),
                    term = out$term[idx],
                    interaction = interaction,
                    wts = out$marginaleffects_wts_internal[idx],
                    eps = out$marginaleffects_eps[idx])
            }
        }
        # function returns unique value
        idx <- !is.na(draws[, 1])
        draws <- draws[idx, , drop = FALSE]

        # if transform_pre returns a single value, then we padded with NA. That
        # also means we don't want `rowid` otherwise we will merge and have
        # useless duplicates.
        if (any(!idx)) {
            out[, "rowid" := NULL]
            out <- out[idx, , drop = FALSE]
        }

    # frequentist
    } else {
        # We want to write the "comparison" column in-place because it safer
        # than group-merge; there were several bugs related to this in the past.
        # safefun() returns 1 value and NAs when the function retunrs a
        # singleton.
        out[, "comparison" := safefun(
            hi = predicted_hi,
            lo = predicted_lo,
            y = predicted,
            n = .N,
            term = term,
            interaction = interaction,
            wts = marginaleffects_wts_internal,
            eps = marginaleffects_eps),
        by = idx]

        # if transform_pre returns a single value, then we padded with NA. That
        # also means we don't want `rowid` otherwise we will merge and have
        # useless duplicates.
        if (any(is.na(out$comparison))) {
            out[, "rowid" := NULL]
        }
        out <- out[!is.na(comparison)]
    }

    out <- get_hypothesis(out, hypothesis, column = "comparison", by = by)

    # output
    attr(out, "posterior_draws") <- draws
    attr(out, "original") <- original
    return(out)
}