get_contrasts <- function(model,
                          newdata,
                          type,
                          variables,
                          original,
                          lo,
                          hi,
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
        ...))

    pred_hi <- myTryCatch(get_predict(
        model,
        type = type,
        vcov = FALSE,
        newdata = hi,
        ...))

    pred_or <- myTryCatch(get_predict(
        model,
        type = type,
        vcov = FALSE,
        newdata = original,
        ...))

    # lots of indexing later requires a data.table
    setDT(original)

    if (inherits(pred_hi$value, "data.frame")) pred_hi <- pred_hi$value
    if (inherits(pred_lo$value, "data.frame")) pred_lo <- pred_lo$value
    if (inherits(pred_or$value, "data.frame")) pred_or <- pred_or$value


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
    out[, "marginaleffects_eps" := NA_real_] # default (probably almost always overwritten)
    mult <- nrow(out) / nrow(original)
    if (isTRUE(mult == 1)) {
        for (v in grep("^term$|^contrast|^marginaleffects_eps$", colnames(original), value = TRUE)) {
            out[, (v) := original[[v]]]
        }

    # group or multivariate outcomes
    } else if (isTRUE(mult > 1)) {
        for (v in grep("^term$|^contrast|^marginaleffects_eps$", colnames(original), value = TRUE)) {
            out[, (v) := rep(original[[v]], times = mult)]
        }

    # cross-contrasts or weird cases
    } else {
        out <- merge(out, newdata, by = "rowid")
        if (isTRUE(nrow(out) == nrow(lo))) {
            tmp <- data.table(lo)[, .SD, .SDcols = patterns("^contrast|marginaleffects_eps")]
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
    fun <- function(x) is.character(x$transform_pre) && x$transform_pre %in% elasticities
    elasticities <- Filter(fun, variables)
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

    # frequentist
    if (is.null(attr(pred_lo, "posterior_draws"))) {
        draws <- NULL

    # bayes
    } else {
        draws_lo <- attr(pred_lo, "posterior_draws")
        draws_hi <- attr(pred_hi, "posterior_draws")
        draws_or <- attr(pred_or, "posterior_draws")

        wrapfun <- function(fun, hi, lo, y, x, eps) {
            args <- list(
                hi = hi,
                lo = lo,
                y = y,
                x = x,
                eps = eps)
            args <- args[intersect(names(args), names(formals(fun)))]
            out <- do.call("fun", args)
            return(out)
        }

        # need to loop over columns for transform_pre with `mean()`, which
        # takes the average of the whole matrix and returns a single
        # numeric.
        idx <- grep("^group$|^term$|^contrast", colnames(out), value = TRUE)
        idx <- apply(out[, ..idx], 1, paste, collapse = "|")
        draws <- list()
        for (i in unique(idx)) {
            idx2 <- i == idx
            draws_lo_sub <- draws_lo[idx2, , drop = FALSE]
            draws_hi_sub <- draws_hi[idx2, , drop = FALSE]
            draws_or_sub <- draws_or[idx2, , drop = FALSE]
            tn <- out[idx2, term][1]
            x <- elasticities[[tn]]
            x <- rep(x, times = nrow(draws_lo) / length(x))[idx2]
            fun <- fun_list[[tn]]

            draws_sub <- wrapfun(
                fun = fun,
                hi = draws_hi_sub,
                lo = draws_lo_sub,
                y = draws_or_sub,
                x = x,
                eps = out[idx2, marginaleffects_eps])

            # usually happens when `transform_pre` returns a unique value instead of a vector
            if (!is.matrix(draws_sub)) {
                draws_sub <- sapply(
                    seq_len(ncol(draws_hi_sub)),
                    function(j) wrapfun(
                        fun = fun,
                        hi = draws_hi_sub[, j],
                        lo = draws_lo_sub[, j],
                        y = draws_or_sub[, j],
                        x = x,
                        eps = out[idx2, marginaleffects_eps])
                )
                draws_sub <- matrix(draws_sub, nrow = 1)
            }
            draws <- append(draws, list(draws_sub))
        }
        draws <- do.call("rbind", draws)
    }

    idx <- grep("^contrast|^group$|^term$|^type$|^transform_pre_idx$", colnames(out), value = TRUE)
    idx <- c(idx, by)
    out[, predicted_lo := pred_lo[["predicted"]]]
    out[, predicted_hi := pred_hi[["predicted"]]]
    out[, predicted_or := pred_or[["predicted"]]]

    # we feed this column to safefun(), even if it is useless for categoricals
    if (!"marginaleffects_eps" %in% colnames(out)) {
        out[, "marginaleffects_eps" := NA]
    }

    # the `by` variables must be included for group-by data.table operations
    if (!is.null(by)) {
        by_merge <- setdiff(by, colnames(out))
        if (length(by_merge) > 0) {
            cols <- c("rowid", by_merge, grep("^contrast", colnames(original), value = TRUE))
            i <- c("rowid", grep("^contrast", colnames(original), value = TRUE))
            # unique important for grid_type = "counterfactual"
            tmp <- unique(original[, ..cols])
            out <- merge(out, tmp, by = i, sort = FALSE)
        }
    }

    # do not feed unknown arguments to a `transform_pre`
    safefun <- function(hi, lo, y, n, term, interaction, eps, recycle = TRUE) {
        # when interaction=TRUE, sanitize_transform_pre enforces a single function
        if (isTRUE(interaction)) {
            fun <- fun_list[[1]]
        } else {
            fun <- fun_list[[term[1]]]
        }

        args <- list("hi" = hi, "lo" = lo, "y" = y, "eps" = eps, "x" = elasticities[[term[1]]])
        args <- args[names(args) %in% names(formals(fun))]
        con <- try(do.call("fun", args), silent = TRUE)
        if (!isTRUE(checkmate::check_numeric(con, len = n)) &&
        !isTRUE(checkmate::check_numeric(con, len = 1))) {
            msg <- format_msg(
                "The function supplied to the `transform_pre` argument must accept two numeric
                vectors of predicted probabilities of length %s, and return a single numeric
                value or a numeric vector of length %s, with no missing value.")
            msg <- sprintf(msg, n, n)
            stop(msg, call. = FALSE)
        }

        if (length(con) == 1 && recycle == FALSE) {
             stop("no recycling allowed", call. = FALSE)
        }

        out = list(comparison = con)
        return(out)
    }

    if (isTRUE(marginalmeans)) {
        out <- out[, .(
            predicted_lo = mean(predicted_lo),
            predicted_hi = mean(predicted_hi),
            predicted_or = mean(predicted_or),
            eps = mean(marginaleffects_eps)),
        by = idx][
        , "comparison" := safefun(
            hi = predicted_hi,
            lo = predicted_lo,
            y = predicted_or,
            n = .N,
            term = term,
            interaction = interaction,
            eps = eps)$comparison,
        by = "term"]

    } else {
        # We want to write the "comparison" column in-place because it safer
        # than group-merge; there were several bugs related to this in the
        # past. However, we also want to avoid recycling and return a 1-row
        # data frame when appropriate. The first call will error when safefun()
        # returns a vector of length one. Then, we fallback on the group-merge
        # strategy for 1-row output.
        e <- tryCatch(
            out[, "comparison" := safefun(
                hi = predicted_hi,
                lo = predicted_lo,
                y = predicted_or,
                n = .N,
                term = term,
                interaction = interaction,
                eps = marginaleffects_eps,
                recycle = FALSE)$comparison,
            by = idx]
            , error = function(e) e)
        if (inherits(e, "error")) {
            if (identical(e$message, "no recycling allowed")) {
                out <- out[, .(comparison = safefun(
                    hi = predicted_hi,
                    lo = predicted_lo,
                    y = predicted_or,
                    n = .N,
                    term = term,
                    interaction = interaction,
                    eps = marginaleffects_eps,
                    recycle = TRUE)$comparison),
                by = idx]
            } else {
                stop(e$message, call. = FALSE)
            }
        }
    }

    out <- get_hypothesis(out, hypothesis, "comparison")

    # output
    attr(out, "posterior_draws") <- draws
    attr(out, "original") <- original
    return(out)
}

