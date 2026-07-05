autodiff_op_registry <- c(
    difference = "difference",
    ratio = "ratio",
    differenceavg = "differenceavg",
    ratioavg = "ratioavg",
    differenceavgwts = "differenceavg",
    ratioavgwts = "ratioavg"
)

autodiff_lower_fail <- function(reason = "") {
    list(ok = FALSE, spec = NULL, reason = reason)
}

autodiff_lower_ok <- function(spec) {
    list(ok = TRUE, spec = spec, reason = "")
}

autodiff_lower_model <- function(plan, mfx, type) {
    args <- get_autodiff_args(mfx@model, mfx, type)
    if (is.null(args)) {
        return(autodiff_lower_fail(""))
    }
    if (is.character(args)) {
        return(autodiff_lower_fail(args[[1]]))
    }

    coefs <- get_coef(mfx@model)
    if (anyNA(coefs)) {
        return(autodiff_lower_fail("models with NA coefficients (aliased terms)"))
    }

    list(ok = TRUE, args = args, coefs = coefs)
}

autodiff_check_matrix <- function(X, coefs, plan) {
    isTRUE(checkmate::check_matrix(X)) &&
        ncol(X) == length(coefs) &&
        nrow(X) == plan$n_pred
}

autodiff_build_agg_blocks <- function(agg, n_est) {
    if (
        !is.list(agg$blocks) ||
            !isTRUE(checkmate::check_integerish(agg$n, len = 1, lower = 1))
    ) {
        return(autodiff_lower_fail("this form of the `by` argument"))
    }

    segments <- integer(n_est)
    seen <- integer(n_est)
    seen_segments <- integer(agg$n)
    weights <- if (isTRUE(agg$weighted)) rep(NA_real_, n_est) else NULL

    for (block in agg$blocks) {
        idx <- block$idx
        if (is.null(idx)) {
            return(autodiff_lower_fail("this form of the `by` argument"))
        }
        if (is.null(dim(idx))) {
            idx <- matrix(idx, ncol = 1)
        }
        cols <- block$cols
        if (
            !isTRUE(checkmate::check_matrix(idx)) ||
                !isTRUE(checkmate::check_integerish(cols, lower = 1, upper = agg$n)) ||
                ncol(idx) != length(cols)
        ) {
            return(autodiff_lower_fail("this form of the `by` argument"))
        }

        w <- NULL
        if (isTRUE(agg$weighted)) {
            w <- block$w
            if (is.null(w)) {
                return(autodiff_lower_fail("missing values in weights"))
            }
            if (is.null(dim(w))) {
                w <- matrix(w, ncol = 1)
            }
            if (!identical(dim(w), dim(idx)) || anyNA(w)) {
                return(autodiff_lower_fail("missing values in weights"))
            }
        }

        for (j in seq_len(ncol(idx))) {
            idx_j <- idx[, j]
            col_j <- cols[[j]]
            if (
                !isTRUE(checkmate::check_integerish(idx_j, lower = 1, upper = n_est)) ||
                    anyDuplicated(idx_j)
            ) {
                return(autodiff_lower_fail("this form of the `by` argument"))
            }
            segments[idx_j] <- col_j
            seen[idx_j] <- seen[idx_j] + 1L
            seen_segments[col_j] <- seen_segments[col_j] + 1L
            if (isTRUE(agg$weighted)) {
                weights[idx_j] <- w[, j]
            }
        }
    }

    if (!identical(seen, rep(1L, n_est))) {
        return(autodiff_lower_fail("this form of the `by` argument"))
    }
    if (!identical(seen_segments, rep(1L, agg$n))) {
        return(autodiff_lower_fail("this form of the `by` argument"))
    }
    if (isTRUE(agg$weighted) && anyNA(weights)) {
        return(autodiff_lower_fail("missing values in weights"))
    }

    list(
        ok = TRUE,
        agg = list(
            segments = segments,
            num_segments = agg$n,
            weights = weights
        )
    )
}

autodiff_build_agg <- function(agg, n_est) {
    if (is.null(agg)) {
        return(list(ok = TRUE, agg = NULL))
    }
    if (!is.null(agg$blocks)) {
        return(autodiff_build_agg_blocks(agg, n_est))
    }
    autodiff_lower_fail("this form of the `by` argument")
}

autodiff_lower_hyp <- function(hyp) {
    if (is.null(hyp)) {
        return(list(ok = TRUE, hyp = NULL))
    }
    if (identical(hyp$kind, "matrix") && isTRUE(checkmate::check_matrix(hyp$H))) {
        return(list(ok = TRUE, hyp = hyp$H))
    }
    autodiff_lower_fail("this form of the `hypothesis` argument")
}

autodiff_lower_predictions <- function(plan, mfx, type) {
    model <- autodiff_lower_model(plan, mfx, type)
    if (!isTRUE(model$ok)) {
        return(model)
    }

    X <- attr(mfx@newdata, "marginaleffects_model_matrix")
    if (!autodiff_check_matrix(X, model$coefs, plan)) {
        return(autodiff_lower_fail("this model/data configuration"))
    }

    if (isTRUE(plan$has_na)) {
        return(autodiff_lower_fail("missing values in predictions"))
    }

    if (!is.null(plan$keep)) {
        X <- X[plan$keep, , drop = FALSE]
    }

    agg <- autodiff_build_agg(plan$agg, nrow(X))
    if (!isTRUE(agg$ok)) {
        return(agg)
    }

    n_after_agg <- if (is.null(agg$agg)) nrow(X) else agg$agg$num_segments
    hyp <- autodiff_lower_hyp(plan$hyp)
    if (!isTRUE(hyp$ok)) {
        return(hyp)
    }

    n_out <- if (is.null(hyp$hyp)) n_after_agg else ncol(hyp$hyp)
    autodiff_lower_ok(list(
        kind = "predictions",
        model = model$args,
        X = X,
        X_hi = NULL,
        X_lo = NULL,
        ops = NULL,
        est_keep = NULL,
        agg = agg$agg,
        hyp = hyp$hyp,
        n_out = n_out
    ))
}

autodiff_validate_comparison_groups <- function(plan) {
    if (length(plan$groups) == 0) {
        stop_sprintf("Internal error: comparison plan has no groups.")
    }
    idx <- unlist(lapply(plan$groups, function(g) g$idx), use.names = FALSE)
    out_idx <- unlist(lapply(plan$groups, function(g) g$out_idx), use.names = FALSE)
    if (!identical(idx, seq_len(max(idx)))) {
        stop_sprintf("Internal error: comparison plan group indices are not consecutive.")
    }
    if (!identical(out_idx, seq_len(plan$n_comp))) {
        stop_sprintf("Internal error: comparison plan output indices are not consecutive.")
    }
}

autodiff_op_weights <- function(g) {
    if (!grepl("wts$", g$fun_key)) {
        return(NULL)
    }
    w <- g$args$w
    if (is.null(w)) {
        return(NULL)
    }
    if (anyNA(w)) {
        return(structure(list(reason = "missing values in weights"), class = "autodiff_weight_error"))
    }
    w
}

autodiff_lower_comparisons <- function(plan, mfx, type, hi, lo) {
    model <- autodiff_lower_model(plan, mfx, type)
    if (!isTRUE(model$ok)) {
        return(model)
    }

    X_hi <- attr(hi, "marginaleffects_model_matrix")
    X_lo <- attr(lo, "marginaleffects_model_matrix")
    if (
        !autodiff_check_matrix(X_hi, model$coefs, plan) ||
            !autodiff_check_matrix(X_lo, model$coefs, plan)
    ) {
        return(autodiff_lower_fail("this model/data configuration"))
    }

    if (!is.null(plan$na_keep)) {
        X_hi <- X_hi[plan$na_keep, , drop = FALSE]
        X_lo <- X_lo[plan$na_keep, , drop = FALSE]
    }
    if (!is.null(plan$perm)) {
        X_hi <- X_hi[plan$perm, , drop = FALSE]
        X_lo <- X_lo[plan$perm, , drop = FALSE]
    }

    if (any(vapply(plan$groups, function(g) is.na(g$fun_key), logical(1)))) {
        return(autodiff_lower_fail("custom comparison functions"))
    }
    if (isTRUE(plan$need_y)) {
        return(autodiff_lower_fail("elasticities"))
    }

    bad <- setdiff(
        unique(vapply(plan$groups, function(g) g$fun_key, character(1))),
        names(autodiff_op_registry)
    )
    if (length(bad) > 0) {
        return(autodiff_lower_fail(sprintf("comparison='%s'", bad[[1]])))
    }

    autodiff_validate_comparison_groups(plan)

    ops <- vector("list", length(plan$groups))
    for (j in seq_along(plan$groups)) {
        g <- plan$groups[[j]]
        w <- autodiff_op_weights(g)
        if (inherits(w, "autodiff_weight_error")) {
            return(autodiff_lower_fail(w$reason))
        }
        ops[[j]] <- list(
            op = unname(autodiff_op_registry[[g$fun_key]]),
            n = length(g$idx),
            w = w
        )
    }

    n_est <- if (is.null(plan$est_keep)) plan$n_comp else length(plan$est_keep)
    agg <- autodiff_build_agg(plan$agg, n_est)
    if (!isTRUE(agg$ok)) {
        return(agg)
    }

    n_after_agg <- if (is.null(agg$agg)) n_est else agg$agg$num_segments
    hyp <- autodiff_lower_hyp(plan$hyp)
    if (!isTRUE(hyp$ok)) {
        return(hyp)
    }

    n_out <- if (is.null(hyp$hyp)) n_after_agg else ncol(hyp$hyp)
    autodiff_lower_ok(list(
        kind = "comparisons",
        model = model$args,
        X = NULL,
        X_hi = X_hi,
        X_lo = X_lo,
        ops = ops,
        est_keep = plan$est_keep,
        agg = agg$agg,
        hyp = hyp$hyp,
        n_out = n_out
    ))
}

autodiff_try <- function(plan, mfx, kind, type, vcov, estimate, hi = NULL, lo = NULL) {
    if (!isTRUE(settings_get("autodiff"))) {
        return(NULL)
    }
    if (is.null(plan)) {
        return(NULL)
    }
    if (!isTRUE(checkmate::check_matrix(vcov))) {
        return(NULL)
    }

    low <- switch(kind,
        predictions = autodiff_lower_predictions(plan, mfx, type),
        comparisons = autodiff_lower_comparisons(plan, mfx, type, hi, lo),
        stop_sprintf("Internal error: unknown autodiff lowering kind.")
    )
    if (!isTRUE(low$ok)) {
        reason <- low$reason
        if (is.null(reason)) reason <- ""
        if (nzchar(reason)) autodiff_warning(reason)
        return(NULL)
    }

    coefs <- get_coef(mfx@model)
    result <- tryCatch(
        autodiff_pipeline_call(low$spec, coefs),
        error = function(e) {
            warning(sprintf(
                "Automatic differentiation failed (%s). Reverting to finite differences.",
                conditionMessage(e)
            ), call. = FALSE)
            NULL
        }
    )
    if (is.null(result)) {
        return(NULL)
    }

    if (!isTRUE(all.equal(
        result$estimate,
        estimate,
        tolerance = 1e-8,
        check.attributes = FALSE
    ))) {
        warning(
            "Automatic differentiation did not reproduce the estimates computed in R. Reverting to finite differences. Please report this at https://github.com/vincentarelbundock/marginaleffects/issues",
            call. = FALSE
        )
        return(NULL)
    }

    se <- autodiff_se_from_jacobian(result$jacobian, vcov, coefs, mfx@model)
    if (is.null(se)) {
        return(NULL)
    }

    if (isTRUE(getOption("marginaleffects_autodiff_message", default = FALSE))) {
        message("\nJAX is fast!")
    }
    list(estimate = result$estimate, std.error = se, jacobian = result$jacobian)
}
