predictions_hi_lo <- function(model, lo, hi, type, ...) {
    if (inherits(model, c("brmsfit", "bart"))) {
        predictions_hi_lo_bayesian(model, lo, hi, type, ...)
    } else {
        predictions_hi_lo_frequentist(model, lo, hi, type, ...)
    }
}


comparison_call_args <- function(term, cross, wts, tmp_idx, newdata, variables, fun_list, elasticities) {
    tn <- term[1]
    fun <- if (isTRUE(cross)) {
        fun_list[[1]]
    } else {
        fun_list[[tn]]
    }
    fun_formals <- names(formals(fun))

    args <- list(
        "eps" = variables[[tn]]$eps,
        "w" = wts,
        "newdata" = newdata
    )

    # sometimes x is exactly the same length, but not always
    args[["x"]] <- elasticities[[tn]][tmp_idx]
    args <- args[names(args) %in% fun_formals]

    fun_key <- if (isTRUE(cross)) {
        variables[[1]][["fun_key"]]
    } else {
        variables[[tn]][["fun_key"]]
    }

    list(
        fun = fun,
        fun_key = fun_key,
        args = args,
        uses_y = "y" %in% fun_formals
    )
}


comparison_validate_result <- function(con, n) {
    if (
        !isTRUE(checkmate::check_numeric(con, len = n)) &&
            !isTRUE(checkmate::check_numeric(con, len = 1))
    ) {
        msg <- sprintf(
            "The function supplied to the `comparison` argument must accept two numeric vectors of predicted probabilities of length %s, and return a single numeric value or a numeric vector of length %s, with no missing value.",
            n,
            n
        ) # nolint
        stop_sprintf(msg)
    }
}


comparison_call <- function(hi, lo, y, n, term, cross, wts, tmp_idx, newdata, variables, fun_list, elasticities) {
    if (n == 0 || length(hi) == 0) {
        return(list(
            value = numeric(0),
            fun = NULL,
            fun_key = NULL,
            args = list(),
            uses_y = FALSE
        ))
    }

    call <- comparison_call_args(
        term = term,
        cross = cross,
        wts = wts,
        tmp_idx = tmp_idx,
        newdata = newdata,
        variables = variables,
        fun_list = fun_list,
        elasticities = elasticities
    )

    value_args <- call$args
    value_args$hi <- hi
    value_args$lo <- lo
    if (isTRUE(call$uses_y)) {
        value_args$y <- y
    }
    con <- try(do_call(call$fun, value_args), silent = TRUE)
    comparison_validate_result(con, n)
    call$value <- con
    call
}


prepare_elasticities <- function(variables, original, out, by, elasticities) {
    FUN <- function(z) {
        (is.character(z$comparison) && z$comparison %in% elasticities) ||
            (is.function(z$comparison) && "x" %in% names(formals(z$comparison)))
    }
    elasticities <- Filter(FUN, variables)

    if (length(elasticities) > 0) {
        # assigning a subset of "original" to "idx1" takes time and memory
        # better to do this here for most columns and add the "v" column only
        # in the loop
        if (!is.null(original)) {
            idx1 <- c(
                "rowid",
                "rowidcf",
                "term",
                "group",
                grep("^contrast", colnames(original), value = TRUE)
            )
            idx1 <- intersect(idx1, colnames(original))
            idx1 <- original[, ..idx1]
        }

        for (v in names(elasticities)) {
            idx2 <- unique(c(
                "rowid",
                "term",
                "group",
                by,
                grep("^contrast", colnames(out), value = TRUE)
            ))
            idx2 <- intersect(idx2, colnames(out))
            # discard other terms to get right length vector
            idx2 <- out[term == v, ..idx2]
            # original is NULL when cross=TRUE
            if (!is.null(original)) {
                # if not first iteration, need to remove previous "v" and "elast"
                if (v %in% colnames(idx1)) {
                    idx1[, (v) := NULL]
                }
                if ("elast" %in% colnames(idx1)) {
                    idx1[, elast := NULL]
                }
                idx1[, (v) := original[[v]]]
                setnames(idx1, old = v, new = "elast")
                on_cols <- intersect(colnames(idx1), colnames(idx2))
                idx2 <- unique(merge(idx2, idx1, by = on_cols, sort = FALSE)[
                    ,
                    elast := elast
                ])
            }
            elasticities[[v]] <- idx2$elast
        }
    }
    return(elasticities)
}


comparison_plan_build <- function(
    mfx,
    type,
    variables,
    original,
    lo,
    hi,
    model_perturbed = NULL,
    by = NULL,
    hypothesis = NULL,
    cross = FALSE,
    verbose = TRUE,
    ...) {
    dots <- list(...)
    on.exit(settings_rm("marginaleffects_safefun_return1"), add = TRUE)
    newdata <- mfx@newdata
    model <- if (is.null(model_perturbed)) mfx@model else model_perturbed

    predictions <- predictions_hi_lo(model, lo, hi, type, ...)
    list2env(predictions, environment())
    out <- data.table(pred_lo)

    elasticity_names <- c(
        "eyex", "eydx", "dyex",
        "eyexavg", "eydxavg", "dyexavg"
    )
    fun <- function(x) {
        isTRUE(checkmate::check_choice(x$comparison, choices = elasticity_names))
    }
    n_elasticities <- length(Filter(fun, variables))
    custom_fun <- any(sapply(variables, function(x) identical(x$label, "custom")))
    need_y <- n_elasticities > 0 || custom_fun

    if (isTRUE(need_y)) {
        pred_or <- get_predict_error(
            model,
            type = type,
            newdata = original,
            ...
        )
        out[, predicted := pred_or[["estimate"]]]
    } else {
        pred_or <- NULL
    }

    extra_cols <- character()
    working_cols <- character()
    working_cols <- c(
        working_cols,
        intersect(
            c("rowidcf", "term", "group", "type", "comparison_idx"),
            colnames(original)
        ),
        grep("^contrast", colnames(original), value = TRUE),
        intersect("marginaleffects_wts_internal", colnames(original))
    )
    if (isTRUE(checkmate::check_data_frame(by))) {
        working_cols <- c(
            working_cols,
            setdiff(intersect(colnames(original), colnames(by)), "by")
        )
    }
    if (
        isTRUE(checkmate::check_formula(hypothesis)) &&
            length(mfx@variable_names_datagrid) > 0
    ) {
        working_cols <- c(
            working_cols,
            intersect(mfx@variable_names_datagrid, colnames(original))
        )
    }
    working_cols <- setdiff(unique(working_cols), colnames(out))
    if (length(working_cols) > 0) {
        n_out <- nrow(out)
        n_orig <- nrow(original)
        if (n_orig > 0 && n_out %% n_orig == 0) {
            for (col in working_cols) {
                v <- original[[col]]
                if (n_out != n_orig) {
                    v <- rep(v, length.out = n_out)
                }
                out[, (col) := v]
            }
        } else {
            out <- cbind(out, original[, ..working_cols])
        }
    }
    extra_cols <- setdiff(colnames(original), colnames(out))

    if (isTRUE(cross)) {
        cols <- setdiff(extra_cols, colnames(out))
        if (length(cols) > 0) {
            out <- cbind(out, original[, ..cols])
            extra_cols <- character()
        }
        out <- merge(out, newdata, by = "rowid", all.x = TRUE, sort = FALSE)
        if (isTRUE(nrow(out) == nrow(lo))) {
            tmp <- lo[
                ,
                .SD,
                .SDcols = patterns("^contrast|marginaleffects_wts_internal")
            ]
            out <- cbind(out, tmp)
            idx <- c(
                "rowid",
                grep("^contrast", colnames(out), value = TRUE),
                colnames(out)
            )
            idx <- unique(idx)
            out <- out[, ..idx]
        }
    }

    if (!"term" %in% colnames(out)) {
        out[, "term" := "cross"]
    }

    if (isTRUE(checkmate::check_data_frame(by))) {
        tmp <- setdiff(intersect(colnames(out), colnames(by)), "by")
        by <- harmonize_by_types(out, by)
        out[by, by := by, on = tmp]
        by <- "by"
    } else if (isTRUE(by)) {
        regex <- "^term$|^contrast_?|^group$"
        by <- unique(grep(regex, colnames(out), value = TRUE))
    } else if (isTRUE(checkmate::check_character(by))) {
        regex <- "^term$|^contrast_?|^group$"
        by <- unique(c(by, grep(regex, colnames(out), value = TRUE)))
    }

    fun_list <- sapply(names(variables), function(x) variables[[x]][["function"]])
    fun_list[["cross"]] <- fun_list[[1]]
    elasticities <- prepare_elasticities(variables, original, out, by, elasticity_names)

    draws_lo <- attr(pred_lo, "posterior_draws")
    draws_hi <- attr(pred_hi, "posterior_draws")
    draws_or <- attr(pred_or, "posterior_draws")
    # Scratch matrix overwritten with comparison draws; draws_lo stays unchanged
    # as the low-prediction input to compare_hi_lo_bayesian().
    draws <- draws_lo

    out[, predicted_lo := pred_lo[["estimate"]]]
    out[, predicted_hi := pred_hi[["estimate"]]]
    if (!is.null(pred_or)) {
        out[, predicted := pred_or[["estimate"]]]
    } else {
        out[, predicted := NA_real_]
    }

    idx <- grep(
        "^contrast|^group$|^term$|^type$|^comparison_idx$",
        colnames(out),
        value = TRUE
    )

    if (isTRUE(checkmate::check_character(by))) {
        tmp <- intersect(colnames(newdata), c(by, colnames(out)))
        if (length(tmp) > 1) {
            tmp <- subset(newdata, select = tmp)
            out <- tryCatch(merge(out, tmp, all.x = TRUE, sort = FALSE), error = function(e) {warning(e); out})
            idx <- unique(c(idx, by))
        }
    }

    if (!"marginaleffects_wts_internal" %in% colnames(out)) {
        out[, "marginaleffects_wts_internal" := NA]
    }

    tmp <- grep("^term$|^contrast|^group$", colnames(out), value = TRUE)
    if (length(tmp) > 0) {
        out[, tmp_idx := seq_len(.N), by = tmp]
    } else {
        out[, tmp_idx := seq_len(.N)]
    }

    if (!is.null(draws)) {
        built <- comparison_plan_build_bayesian(
            out = out,
            draws = draws,
            draws_hi = draws_hi,
            draws_lo = draws_lo,
            draws_or = draws_or,
            by = by,
            cross = cross,
            variables = variables,
            fun_list = fun_list,
            elasticities = elasticities,
            newdata = newdata
        )
        out <- built$out
        draws <- built$draws
        plan <- built$plan
    } else {
        built <- comparison_plan_build_frequentist(
            out = out,
            idx = idx,
            cross = cross,
            variables = variables,
            fun_list = fun_list,
            elasticities = elasticities,
            newdata = newdata,
            pred_hi = pred_hi[["estimate"]],
            pred_lo = pred_lo[["estimate"]],
            pred_or = if (!is.null(pred_or)) pred_or[["estimate"]] else NULL,
            type = type,
            dots = dots,
            hi = hi,
            lo = lo,
            original = original,
            need_y = need_y
        )
        out <- built$out
        plan <- built$plan
    }

    if ("rowid_dedup" %in% colnames(out)) {
        out[, "rowid_dedup" := NULL]
    }

    auto_mean_fun_sub <- any(grepl("^mean\\(", unique(out$contrast)))
    if (!auto_mean_fun_sub && any(grepl("^contrast[_]?", colnames(out)))) {
        if (is.null(draws) && !is.null(plan)) {
            agg <- record_plan_aggregation(
                out,
                draws = NULL,
                newdata = newdata,
                by = by,
                verbose = verbose
            )
            out <- agg$out
            plan$agg <- agg$agg
        } else {
            out <- get_by(
                out,
                draws = draws,
                newdata = newdata,
                by = by,
                verbose = verbose
            )
            draws <- attr(out, "posterior_draws")
        }
    }

    if (
        is.null(hypothesis) &&
            length(extra_cols) > 0 &&
            "rowid" %in% colnames(out) &&
            "rowid" %in% colnames(original)
    ) {
        cols <- setdiff(extra_cols, colnames(out))
        idx_extra <- match(out[["rowid"]], original[["rowid"]])
        if (length(cols) > 0 && !anyNA(idx_extra)) {
            prediction_cols <- if (is.null(pred_or)) {
                c("predicted_lo", "predicted_hi", "predicted")
            } else {
                c("predicted_lo", "predicted_hi")
            }
            prediction_cols <- intersect(prediction_cols, colnames(out))
            out_cols <- setdiff(colnames(out), prediction_cols)
            out <- cbind(
                out[, ..out_cols],
                original[idx_extra, ..cols],
                out[, ..prediction_cols]
            )
        }
    }

    attr(out, "posterior_draws") <- draws
    if (is.null(draws) && !is.null(plan)) {
        hyp <- hypothesis_compile(
            hypothesis,
            out,
            by = by,
            newdata = original,
            mfx = mfx
        )
        out <- hyp$cmp
        plan$hyp <- hyp$hyp
        baseline <- comparison_plan_apply(
            plan,
            pred_hi[["estimate"]],
            pred_lo[["estimate"]],
            if (!is.null(pred_or)) pred_or[["estimate"]] else NULL
        )
        validate_plan_replay("comparison", baseline, out[["estimate"]])
    } else {
        out <- get_hypothesis(
            out,
            hypothesis,
            by = by,
            newdata = original,
            draws = draws,
            mfx = mfx
        )
    }

    list(cmp = out, plan = plan)
}
