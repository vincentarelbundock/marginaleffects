get_contrasts <- function(model,
                          newdata,
                          type,
                          variables,
                          original,
                          lo,
                          hi,
                          marginalmeans,
                          hypothesis = NULL,
                          interaction = FALSE,
                          ...) {

    dots <- list(...)

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


    # predicted values on the original data
    # needed for elasticities but don't waste time if we not needed
    if (any(sapply(variables, function(x) x$label %in% c("eyex", "eydx", "dyex")))) {
        pred_or <- myTryCatch(get_predict(
            model,
            type = type,
            vcov = FALSE,
            newdata = original,
            ...))
        if (inherits(pred_or$value, "data.frame")) pred_or <- pred_or$value
    } else {
        pred_or <- pred_hi
    }

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
        out[, "term" := original[["term"]]]
        out[, "contrast" := original[["contrast"]]]
        if ("marginaleffects_eps" %in% colnames(original)) {
            out[, "marginaleffects_eps" := original[["marginaleffects_eps"]]]
        }

    # group or multivariate outcomes
    } else if (isTRUE(mult > 1)) {
        out[, "term" := rep(original$term, times = mult)]
        out[, "contrast" := rep(original$contrast, times = mult)]
        if ("marginaleffects_eps" %in% colnames(original)) {
            out[, "marginaleffects_eps" := rep(original$marginaleffects_eps, times = mult)]
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
        out[, "term" := "interaction"]
    }

    # transform_pre function could be different for different terms
    fun_list <- sapply(names(variables), function(x) variables[[x]][["function"]])

    # elasticity requires the original (properly aligned) predictor values
    # this will discard factor variables which are duplicated, so in principle
    # it should be the "correct" size
    elasticities <- Filter(function(x) x$label %in% c("eyex", "eydx", "dyex"), variables)
    elasticities <- lapply(elasticities, function(x) x$name)
    if (length(elasticities) > 0) {
        for (v in names(elasticities)) {
            idx2 <- c("rowid", "term", "type", "group", grep("^contrast", colnames(out), value = TRUE))
            idx2 <- intersect(idx2, colnames(out))
            # discard other terms to get right length vector
            idx2 <- out[term == v, ..idx2]
            # original is NULL when interaction=TRUE
            if (!is.null(original)) {
                idx1 <- c(v, "rowid", "term", "type", "group", grep("^contrast", colnames(original), value = TRUE))
                idx1 <- intersect(idx1, colnames(original))
                idx1 <- original[, ..idx1]
                idx2 <- merge(idx1, idx2)
            }
            elasticities[[v]] <- idx2[[v]]
        }
    }

    # bayes
    draws_lo <- attr(pred_lo, "posterior_draws")
    draws_hi <- attr(pred_hi, "posterior_draws")
    draws_or <- attr(pred_or, "posterior_draws")
    if (is.null(draws_lo)) {
        draws <- NULL
    } else {
        draws <- draws_lo
        termnames <- unique(out$term)
        for (tn in termnames) {
            # sanity_variables ensures that all functions are identical when interaction=TRUE
            if (isTRUE(interaction)) {
                fun <- fun_list[[1]]
            } else {
                fun <- fun_list[[tn]]
            }
            idx <- out$term == tn
            args <- list(
                hi = draws_hi[idx, ],
                lo = draws_lo[idx, ],
                eps = out[idx, marginaleffects_eps],
                x = elasticities[[tn]][idx])
            args <- args[intersect(names(args), names(formals(fun)))]
            draws[idx, ] <- do.call("fun", args)
        }
    }

    idx <- grep("^contrast|^group$|^term$|^type$|^transform_pre_idx$", colnames(out), value = TRUE)
    out[, predicted_lo := pred_lo[["predicted"]]]
    out[, predicted_hi := pred_hi[["predicted"]]]

    if (!is.null(pred_or[["predicted"]])) {
        out[, predicted_or := pred_or[["predicted"]]]
    }

    # we feed this column to safefun(), even if it is useless for categoricals
    if (!"marginaleffects_eps" %in% colnames(out)) {
        out[, "marginaleffects_eps" := NA]
    }

    # do not feed unknown arguments to a `transform_pre`
    safefun <- function(hi, lo, or, n, term, interaction, eps) {
        if (isTRUE(interaction)) {
            fun <- fun_list[[1]]
        } else {
            fun <- fun_list[[term[1]]]
        }
        args <- list("hi" = hi, "lo" = lo, "or" = or, "eps" = eps, "x" = elasticities[[term[1]]])
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
        return(con)
    }

    if (isTRUE(marginalmeans)) {
        out <- out[, .(
            predicted_lo = mean(predicted_lo),
            predicted_hi = mean(predicted_hi),
            predicted_or = mean(predicted_or),
            eps = mean(marginaleffects_eps)),
        by = idx]
        out[, "comparison" := safefun(
            hi = predicted_hi,
            lo = predicted_lo,
            or = predicted_or,
            n = .N,
            term = term,
            interaction = interaction,
            eps = eps),
        by = "term"]

    } else {
        # tmp needed to avoid recycling when safefun() returns length 1 value
        # assign back into `out` to preserve `rowid` when possible
        tmp <- out[, .(comparison = safefun(
            hi = predicted_hi,
            lo = predicted_lo,
            or = predicted_or,
            n = .N,
            term = term,
            interaction = interaction,
            eps = marginaleffects_eps)),
        by = idx]
        if (nrow(tmp) != nrow(out)) {
            out <- tmp
        } else {
            out[, "comparison" := tmp$comparison]
        }
    }

    out <- get_hypothesis(out, hypothesis, "comparison")

    # output
    attr(out, "posterior_draws") <- draws
    attr(out, "original") <- original
    return(out)
}

