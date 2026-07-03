comparison_plan_build <- function(
    mfx,
    type,
    variables,
    original,
    lo,
    hi,
    model_perturbed = NULL,
    by = NULL,
    byfun = NULL,
    hypothesis = NULL,
    cross = FALSE,
    verbose = TRUE,
    ...) {
    dots <- list(...)
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
        for (v in colnames(by)) {
            if (isTRUE(is.character(out[[v]])) && isTRUE(is.numeric(by[[v]]))) {
                by[[v]] <- as.character(by[[v]])
            } else if (isTRUE(is.numeric(out[[v]])) && isTRUE(is.character(by[[v]]))) {
                by[[v]] <- as.numeric(by[[v]])
            }
        }
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

    draws <- attr(pred_lo, "posterior_draws")
    draws_lo <- attr(pred_lo, "posterior_draws")
    draws_hi <- attr(pred_hi, "posterior_draws")
    draws_or <- attr(pred_or, "posterior_draws")

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
        result <- compare_hi_lo_bayesian(
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
        out <- result$out
        draws <- result$draws
        plan <- NULL
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
            agg <- estimate_plan_record_agg(
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
        if (!isTRUE(all.equal(baseline, out[["estimate"]], tolerance = 1e-12, check.attributes = FALSE))) {
            stop_sprintf("Internal error: comparison plan baseline check failed.")
        }
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

    settings_rm("marginaleffects_safefun_return1")
    list(cmp = out, plan = plan)
}

comparison_plan_build_frequentist <- function(
    out,
    idx,
    cross,
    variables,
    fun_list,
    elasticities,
    newdata,
    pred_hi,
    pred_lo,
    pred_or,
    type,
    dots,
    hi,
    lo,
    original,
    need_y) {
    n_pred <- length(pred_lo)
    na_keep <- if (anyNA(out$predicted_lo)) which(!is.na(out$predicted_lo)) else NULL
    if (!is.null(na_keep)) {
        out <- out[na_keep]
    }

    idx <- intersect(idx, colnames(out))
    out[, marginaleffects_plan_row_id := seq_len(.N)]
    if (length(idx) > 0) {
        groups_dt <- out[, .(rows = list(marginaleffects_plan_row_id)), keyby = idx]
        perm <- unlist(groups_dt$rows, use.names = FALSE)
    } else {
        perm <- seq_len(nrow(out))
    }
    out_sorted <- out[perm]
    perm_store <- if (identical(perm, seq_len(nrow(out)))) NULL else perm

    if (length(idx) > 0) {
        bounds <- out_sorted[, .(first = .I[1], last = .I[.N]), keyby = idx]
    } else {
        bounds <- data.table::data.table(first = 1L, last = nrow(out_sorted))
    }

    plan_groups <- vector("list", nrow(bounds))
    out_parts <- vector("list", nrow(bounds))
    n_comp <- 0L
    any_scalar_aggregate <- FALSE

    for (j in seq_len(nrow(bounds))) {
        rows <- seq.int(bounds$first[[j]], bounds$last[[j]])
        n <- length(rows)
        term <- out_sorted$term[rows]
        tn <- term[[1]]
        if (isTRUE(cross)) {
            fun <- fun_list[[1]]
        } else {
            fun <- fun_list[[tn]]
        }
        args <- list(
            eps = variables[[tn]]$eps,
            w = out_sorted$marginaleffects_wts_internal[rows],
            newdata = newdata,
            x = elasticities[[tn]][out_sorted$tmp_idx[rows]]
        )
        args <- args[names(args) %in% names(formals(fun))]
        uses_y <- "y" %in% names(formals(fun))
        value_args <- args
        value_args$hi <- out_sorted$predicted_hi[rows]
        value_args$lo <- out_sorted$predicted_lo[rows]
        if (isTRUE(uses_y)) {
            value_args$y <- out_sorted$predicted[rows]
        }
        con <- try(do_call(fun, value_args), silent = TRUE)
        if (
            !isTRUE(checkmate::check_numeric(con, len = n)) &&
                !isTRUE(checkmate::check_numeric(con, len = 1))
        ) {
            msg <- sprintf(
                "The function supplied to the `comparison` argument must accept two numeric vectors of predicted probabilities of length %s, and return a single numeric value or a numeric vector of length %s, with no missing value.",
                n,
                n
            )
            stop_sprintf(msg)
        }

        if (length(con) == 1) {
            if (n > 1) {
                any_scalar_aggregate <- TRUE
            }
            out_idx <- n_comp + 1L
            part <- out_sorted[rows[[1]], , drop = FALSE]
            part[, estimate := con]
            n_comp <- n_comp + 1L
        } else {
            out_idx <- seq.int(n_comp + 1L, n_comp + n)
            part <- out_sorted[rows, , drop = FALSE]
            part[, estimate := con]
            n_comp <- n_comp + n
        }
        out_parts[[j]] <- part
        plan_groups[[j]] <- list(
            idx = rows,
            out_idx = out_idx,
            scalar = length(con) == 1,
            uses_y = uses_y,
            fun = fun,
            args = args
        )
    }

    out <- data.table::rbindlist(out_parts, fill = TRUE)
    out[, tmp_idx := NULL]
    out[, marginaleffects_plan_row_id := NULL]

    if (isTRUE(any_scalar_aggregate)) {
        keep_cols <- c(
            idx,
            grep(
                "^estimate$|^contrast|^group$|^term$|^marginaleffects_wts_internal$",
                colnames(out),
                value = TRUE
            )
        )
        keep_cols <- unique(intersect(keep_cols, colnames(out)))
        out <- subset(out, select = keep_cols)
    }

    est_keep <- if (anyNA(out$estimate)) which(!is.na(out$estimate)) else NULL
    if (!is.null(est_keep)) {
        out <- out[est_keep, drop = FALSE]
    }

    plan <- list(
        n_pred = n_pred,
        need_y = need_y,
        predict_args = list(
            type = type,
            hi = hi,
            lo = lo,
            original = original,
            dots = dots
        ),
        na_keep = na_keep,
        perm = perm_store,
        groups = plan_groups,
        n_comp = n_comp,
        est_keep = est_keep,
        agg = NULL,
        hyp = NULL,
        check = list(n_pred = n_pred)
    )
    list(out = out, plan = plan)
}

comparison_plan_apply <- function(plan, hi, lo, y = NULL) {
    stopifnot(length(hi) == plan$check$n_pred)
    stopifnot(length(lo) == plan$check$n_pred)
    if (!is.null(plan$na_keep)) {
        hi <- hi[plan$na_keep]
        lo <- lo[plan$na_keep]
        if (!is.null(y)) y <- y[plan$na_keep]
    }
    if (!is.null(plan$perm)) {
        hi <- hi[plan$perm]
        lo <- lo[plan$perm]
        if (!is.null(y)) y <- y[plan$perm]
    }

    est <- numeric(plan$n_comp)
    for (g in plan$groups) {
        args <- g$args
        args$hi <- hi[g$idx]
        args$lo <- lo[g$idx]
        if (isTRUE(g$uses_y)) {
            args$y <- y[g$idx]
        }
        est[g$out_idx] <- do_call(g$fun, args)
    }
    if (!is.null(plan$est_keep)) {
        est <- est[plan$est_keep]
    }
    if (!is.null(plan$agg)) {
        est <- estimate_plan_apply_agg(plan$agg, est)
    }
    if (!is.null(plan$hyp)) {
        est <- plan$hyp$apply(est)
    }
    est
}

comparison_plan_predict <- function(.plan, model_perturbed, ...) {
    dots <- estimate_plan_predict_dots(.plan$predict_args$dots, list(...))
    args_hi <- c(
        list(
            model = model_perturbed,
            type = .plan$predict_args$type,
            newdata = .plan$predict_args$hi
        ),
        dots
    )
    args_lo <- c(
        list(
            model = model_perturbed,
            type = .plan$predict_args$type,
            newdata = .plan$predict_args$lo
        ),
        dots
    )
    pred_hi <- do_call(get_predict, args_hi)
    pred_lo <- do_call(get_predict, args_lo)
    pred_or <- NULL
    if (isTRUE(.plan$need_y)) {
        args_or <- c(
            list(
                model = model_perturbed,
                type = .plan$predict_args$type,
                newdata = .plan$predict_args$original
            ),
            dots
        )
        pred_or <- do_call(get_predict, args_or)
        pred_or <- pred_or[["estimate"]]
    }
    list(
        hi = pred_hi[["estimate"]],
        lo = pred_lo[["estimate"]],
        or = pred_or
    )
}
