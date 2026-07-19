predictions_hi_lo <- function(model, lo, hi, type, ...) {
    if (inherits(model, c("brmsfit", "bart"))) {
        predictions_hi_lo_bayesian(model, lo, hi, type, ...)
    } else {
        predictions_hi_lo_frequentist(model, lo, hi, type, ...)
    }
}


comparison_call_args <- function(term, wts, tmp_idx, context) {
    tn <- term[1]
    key <- if (isTRUE(context$cross)) 1L else tn
    fun <- context$fun_list[[key]]
    fun_formals <- names(formals(fun))

    args <- list(
        "eps" = context$variables[[tn]]$eps,
        "w" = wts,
        "newdata" = context$newdata
    )

    # sometimes x is exactly the same length, but not always
    args[["x"]] <- context$elasticities[[tn]][tmp_idx]
    args <- args[names(args) %in% fun_formals]

    list(
        fun = fun,
        fun_key = context$variables[[key]][["fun_key"]],
        args = args,
        uses_y = "y" %in% fun_formals
    )
}


comparison_validate_result <- function(con, n) {
    if (
        !isTRUE(checkmate::check_numeric(con, len = n, any.missing = FALSE)) &&
            !isTRUE(checkmate::check_numeric(con, len = 1, any.missing = FALSE))
    ) {
        msg <- sprintf(
            "The function supplied to the `comparison` argument must accept two numeric vectors of predicted probabilities of length %s, and return a single numeric value or a numeric vector of length %s, with no missing value.",
            n,
            n
        ) # nolint
        stop_sprintf(msg)
    }
}


comparison_call <- function(hi, lo, y, n, term, wts, tmp_idx, context) {
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
        wts = wts,
        tmp_idx = tmp_idx,
        context = context
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


prepare_elasticities <- function(variables, original, out, by, elasticity_names) {
    needs_x <- function(variable) {
        (is.character(variable$comparison) && variable$comparison %in% elasticity_names) ||
            (is.function(variable$comparison) && "x" %in% names(formals(variable$comparison)))
    }
    elasticities <- Filter(needs_x, variables)

    if (length(elasticities) == 0) {
        return(elasticities)
    }

    # Reuse stable columns from `original`; add the current variable in the loop.
    if (!is.null(original)) {
        original_cols <- c(
            "rowid",
            "rowidcf",
            "term",
            "group",
            grep("^contrast", colnames(original), value = TRUE)
        )
        original_cols <- intersect(original_cols, colnames(original))
        original_index <- original[, ..original_cols]
    }

    for (v in names(elasticities)) {
        out_cols <- unique(c(
            "rowid",
            "term",
            "group",
            by,
            grep("^contrast", colnames(out), value = TRUE)
        ))
        out_cols <- intersect(out_cols, colnames(out))

        # Keep only the current term so the elasticity vector has the right length.
        out_index <- out[term == v, ..out_cols]

        # `original` is NULL when cross=TRUE.
        if (!is.null(original)) {
            # Remove columns left over from the previous loop iteration.
            if (v %in% colnames(original_index)) {
                original_index[, (v) := NULL]
            }
            if ("elast" %in% colnames(original_index)) {
                original_index[, elast := NULL]
            }
            original_index[, (v) := original[[v]]]
            setnames(original_index, old = v, new = "elast")
            on_cols <- intersect(colnames(original_index), colnames(out_index))
            out_index <- unique(merge(out_index, original_index, by = on_cols, sort = FALSE))
        }
        elasticities[[v]] <- out_index$elast
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
    pred_lo <- predictions$pred_lo
    pred_hi <- predictions$pred_hi
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
        if (isTRUE(checkmate::check_matrix(mfx@vcov_model))) {
            original <- add_model_matrix_attribute_data(mfx, original)
        }
        pred_or <- get_predict_error(
            model,
            type = type,
            newdata = original,
            ...
        )
    } else {
        pred_or <- NULL
    }

    working_cols <- c(
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

    by_regex <- "^term$|^contrast_?|^group$"
    implicit_by <- unique(grep(by_regex, colnames(out), value = TRUE))
    if (isTRUE(checkmate::check_data_frame(by))) {
        tmp <- setdiff(intersect(colnames(out), colnames(by)), "by")
        by <- harmonize_by_types(out, by)
        out[by, by := by, on = tmp]
        by <- "by"
    } else {
        by <- sanitize_by(
            mfx,
            by,
            out = out,
            implicit = implicit_by,
            implicit_first = FALSE,
            prune = FALSE
        )
    }

    fun_list <- sapply(names(variables), function(x) variables[[x]][["function"]])
    elasticities <- prepare_elasticities(variables, original, out, by, elasticity_names)
    comparison_context <- list(
        cross = cross,
        newdata = newdata,
        variables = variables,
        fun_list = fun_list,
        elasticities = elasticities
    )

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
        built <- compare_hi_lo_bayesian(
            out = out,
            draws = draws,
            draws_hi = draws_hi,
            draws_lo = draws_lo,
            draws_or = draws_or,
            by = by,
            context = comparison_context
        )
        out <- built$out
        draws <- built$draws
        plan <- NULL
    } else {
        built <- comparison_plan_build_frequentist(
            out = out,
            idx = idx,
            context = comparison_context,
            n_pred = length(pred_lo[["estimate"]]),
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
