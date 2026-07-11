#' Request unconditional variance in marginaleffects calls
#'
#' @param vcov Variance estimator for the linearized scores. Supported values
#'   are `"HC1"`, `"robust"`, `"HC0"`, or a one-sided formula such
#'   as `~id` for one-way clustered inference.
#' @param df Degrees of freedom. `"residual"` uses residual degrees of freedom
#'   for linear robust models, cluster count minus one for linear clustered
#'   models, and normal inference for GLMs.
#'
#' @return An object which can be supplied to the `vcov` argument of
#'   [avg_predictions()], [avg_comparisons()], or [avg_slopes()].
#'
#' @details
#' Unconditional variance is available for effects evaluated over original
#' model-data rows, valid subsets of those rows, or counterfactual grids that
#' preserve a valid `rowid`/`rowidcf` mapping to original model-data rows. The
#' effect must be averaged or aggregated with `avg_*()` or `by`, or it must be a
#' scalar comparison. Hypotheses applied directly to unit-level effects are
#' rejected because the empirical-distribution influence function is not
#' identifiable from an arbitrary post-hoc hypothesis function. Synthetic grids
#' such as `newdata = "mean"` are rejected because the current implementation
#' cannot generally infer how those grid values vary with the empirical
#' covariate distribution. Models must provide compatible score and bread
#' matrices through `sandwich::estfun()`/`sandwich::bread()` or model-specific
#' equivalents. Supported model classes are validated through an allow-list.
#' Multiple-imputation objects, prediction methods that return posterior draws,
#' baseline-hazard dependent `coxph` predictions such as `type = "survival"` or
#' `type = "expected"`, average predictions from `fixest` models with fixed
#' effects, and nonlinear `fixest` models with fixed effects are rejected
#' explicitly.
#'
#' @export
unconditional <- function(vcov = "HC1", df = "residual") {
    structure(
        list(vcov = vcov, df = df),
        class = "marginaleffects_vcov_unconditional"
    )
}


is_unconditional_vcov <- function(vcov) {
    if (inherits(vcov, "marginaleffects_vcov_unconditional") ||
        (is.character(vcov) && length(vcov) == 1L && isTRUE(tolower(vcov) == "unconditional"))) {
        return(TRUE)
    }
    if (!is.call(vcov)) {
        return(FALSE)
    }
    fun <- tryCatch(as.character(vcov[[1]]), error = function(e) character())
    length(fun) > 0L && identical(fun[[length(fun)]], "unconditional")
}


update_unconditional_vcov_transform <- function(mfx, estimate, transform) {
    vcov_type <- tryCatch(mfx@vcov_type, error = function(e) "")
    if (!isTRUE(grepl("^Unconditional", vcov_type))) {
        return(mfx)
    }

    if (!is.function(transform)) {
        if (is.null(transform[[1]])) {
            return(mfx)
        }
        transform <- transform[[1]]
    }

    if (is.null(estimate)) {
        return(mfx)
    }

    V <- mfx@vcov_model
    if (!isTRUE(checkmate::check_matrix(V, mode = "numeric"))) {
        return(mfx)
    }
    if (nrow(V) != ncol(V) || length(estimate) != nrow(V)) {
        stop_sprintf(
            "Internal error: unconditional vcov has %d rows but the transformed estimates have length %d.",
            nrow(V),
            length(estimate)
        )
    }

    estimate <- as.numeric(estimate)
    if (anyNA(estimate) || any(!is.finite(estimate))) {
        msg <- paste0(
            "`vcov = \"unconditional\"` cannot transform the saved covariance ",
            "matrix when pre-transform estimates are non-finite."
        )
        stop_sprintf(msg)
    }

    transform_checked <- function(x) {
        out <- transform(x)
        if (!is.numeric(out) || length(out) != length(x)) {
            stop_sprintf(
                "The `transform` function must return a numeric vector with the same length as its input."
            )
        }
        as.numeric(out)
    }

    H <- tryCatch(
        get_jacobian(transform_checked, estimate, mfx@numderiv %||% list("fdforward")),
        error = function(e) {
            stop_sprintf(
                "`vcov = \"unconditional\"` could not numerically differentiate the `transform` function: %s",
                conditionMessage(e)
            )
        }
    )
    if (!identical(dim(H), c(length(estimate), length(estimate))) || anyNA(H) || any(!is.finite(H))) {
        stop_sprintf(
            "`vcov = \"unconditional\"` could not compute a finite transform Jacobian."
        )
    }

    V <- H %*% V %*% t(H)
    V <- (V + t(V)) / 2
    dimnames(V) <- dimnames(mfx@vcov_model)
    mfx@vcov_model <- V
    mfx@jacobian <- diag(nrow(V))
    mfx
}


plan_unconditional_se <- function(
    built,
    mfx,
    estimates,
    type,
    unconditional,
    dots = list(),
    contrast_data = NULL,
    variables = NULL,
    numderiv = NULL) {

    if (!is.null(mfx) && !is.null(mfx@draws)) {
        stop_sprintf(
            "`vcov = \"unconditional\"` is not supported for models or prediction methods that return posterior draws."
        )
    }

    inference_cols <- intersect(
        c("std.error", "statistic", "p.value", "s.value", "conf.low", "conf.high", "df"),
        colnames(estimates)
    )
    if (length(inference_cols) > 0) {
        estimates[inference_cols] <- NULL
    }

    plan <- built$plan
    kind <- plan$kind
    if (!isTRUE(kind %in% c("predictions", "comparisons"))) {
        stop_sprintf("Unknown plan kind: %s", kind %||% "NULL")
    }

    model <- mfx@model
    validate_unconditional_model_support(model, kind, type = type)
    modeldata <- data.table::as.data.table(mfx@modeldata)
    n <- nrow(modeldata)
    allow_mismatch <- get_unconditional_allow_mismatch(mfx, variables)
    numderiv <- tryCatch(mfx@numderiv, error = function(e) numderiv) %||% list("fdforward")

    rowid <- sanitize_unconditional_plan(
        plan = plan,
        modeldata = modeldata,
        n = n,
        df = unconditional$df,
        n_estimates = nrow(estimates),
        allow_mismatch = allow_mismatch
    )

    # Unconditional variance adds two influence components:
    # sampling variation in the empirical covariate distribution, and
    # coefficient uncertainty mapped through the existing effect Jacobian.
    beta_dot <- get_unconditional_beta_dot(model)
    if (nrow(beta_dot) != n) {
        stop_sprintf(
            "`vcov = \"unconditional\"` requires one score row per model-data row."
        )
    }

    J <- get_unconditional_plan_jacobian(
        plan = plan,
        mfx = mfx,
        estimates = estimates,
        type = type,
        dots = dots,
        kind = kind,
        contrast_data = contrast_data,
        variables = variables,
        numderiv = numderiv
    )

    if (is.null(colnames(J)) || anyNA(colnames(J)) || any(colnames(J) == "")) {
        stop_sprintf("The unconditional effect Jacobian must have named coefficient columns.")
    }
    missing_beta <- setdiff(colnames(J), colnames(beta_dot))
    missing_jacobian <- setdiff(colnames(beta_dot), colnames(J))
    if (length(missing_beta) > 0 || length(missing_jacobian) > 0) {
        missing <- unique(c(missing_beta, missing_jacobian))
        stop_sprintf(
            "The unconditional score matrix does not match the effect Jacobian. Missing columns: %s.",
            paste(missing, collapse = ", ")
        )
    }
    cols <- colnames(J)
    beta_phi <- beta_dot[, cols, drop = FALSE] %*% t(J[, cols, drop = FALSE])

    empirical_phi <- get_unconditional_plan_empirical_phi(
        plan = plan,
        model = model,
        n = n,
        rowid = rowid,
        numderiv = numderiv
    )

    if (ncol(empirical_phi) != nrow(J)) {
        stop_sprintf(
            "Internal error: empirical unconditional influence has %d columns, but the Jacobian has %d rows.",
            ncol(empirical_phi), nrow(J)
        )
    }

    Phi <- empirical_phi + beta_phi
    inputs <- list(
        model = model,
        n = n,
        vcov = unconditional$vcov
    )
    V <- get_unconditional_vcov(Phi, inputs)
    se <- sqrt(diag(V))
    se[se == 0] <- NA_real_

    estimates$std.error <- as.vector(se)
    if (unconditional_df_has_finite(unconditional$df)) {
        estimates$df <- unconditional$df
    }

    mfx@vcov_model <- V
    mfx@vcov_type <- if (unconditional$vcov$type == "cluster") {
        sprintf("Unconditional (clustered by %s)", unconditional$vcov$cluster_var)
    } else {
        "Unconditional"
    }
    mfx@df <- unconditional$df
    mfx@jacobian <- diag(nrow(V))

    list(mfx = mfx, estimates = estimates)
}


get_unconditional_plan_jacobian <- function(
    plan,
    mfx,
    estimates,
    type,
    dots,
    kind,
    contrast_data = NULL,
    variables = NULL,
    numderiv = NULL) {

    coefs <- get_coef(mfx@model)
    V <- diag(length(coefs))
    dimnames(V) <- list(names(coefs), names(coefs))

    if (identical(kind, "predictions")) {
        fun <- function(model_perturbed, ...) {
            pred <- prediction_plan_predict(plan, model_perturbed, ...)
            prediction_plan_apply(plan, pred)
        }
        args <- list(
            mfx = mfx,
            model_perturbed = mfx@model,
            vcov = V,
            type = type,
            FUN = fun,
            hypothesis = mfx@hypothesis
        )
    } else {
        fun <- function(model_perturbed, ...) {
            preds <- comparison_plan_predict(plan, model_perturbed, ...)
            comparison_plan_apply(plan, preds$hi, preds$lo, preds$or)
        }
        args <- list(
            mfx = mfx,
            model_perturbed = mfx@model,
            vcov = V,
            type = type,
            FUN = fun,
            variables = variables,
            hypothesis = mfx@hypothesis,
            hi = contrast_data$hi,
            lo = contrast_data$lo,
            original = contrast_data$original,
            estimates = estimates,
            numderiv = numderiv
        )
    }

    args <- utils::modifyList(args, dots)
    se <- do_call(get_se_delta, args)
    J <- attr(se, "jacobian")
    if (!isTRUE(checkmate::check_matrix(J, mode = "numeric", nrows = nrow(estimates)))) {
        stop_sprintf("Unable to compute the unconditional effect Jacobian.")
    }
    J
}


get_unconditional_plan_empirical_phi <- function(
    plan,
    model,
    n,
    rowid,
    numderiv = list("fdforward")) {

    if (identical(plan$kind, "predictions")) {
        return(get_unconditional_prediction_plan_phi(
            plan,
            model,
            n,
            rowid = rowid,
            numderiv = numderiv))
    }
    get_unconditional_comparison_plan_phi(
        plan,
        model,
        n,
        rowid = rowid,
        numderiv = numderiv)
}


get_unconditional_prediction_plan_phi <- function(
    plan,
    model,
    n,
    rowid,
    numderiv = list("fdforward")) {

    pred <- prediction_plan_predict(plan, model)
    if (!is.null(plan$keep)) {
        pred <- pred[plan$keep]
    }

    phi <- get_unconditional_aggregate_phi(
        agg = plan$agg,
        base_est = pred,
        rowid = rowid,
        n = n
    )

    apply_unconditional_hypothesis_phi(phi, plan, pred, numderiv = numderiv)
}


get_unconditional_comparison_plan_phi <- function(
    plan,
    model,
    n,
    rowid,
    numderiv = list("fdforward")) {

    base <- get_unconditional_comparison_base(plan, model, rowid, n)

    phi <- get_unconditional_aggregate_phi(
        agg = plan$agg,
        base_est = base$estimate,
        rowid = base$rowid,
        n = n,
        base_phi = base$phi
    )

    apply_unconditional_hypothesis_phi(phi, plan, base$estimate, numderiv = numderiv)
}


get_unconditional_comparison_base <- function(plan, model, rowid, n) {
    preds <- comparison_plan_predict(plan, model)
    hi <- preds$hi
    lo <- preds$lo
    y <- preds$or

    if (!is.null(plan$na_keep)) {
        hi <- hi[plan$na_keep]
        lo <- lo[plan$na_keep]
        if (!is.null(y)) {
            y <- y[plan$na_keep]
        }
        rowid <- rowid[plan$na_keep]
    }
    if (!is.null(plan$perm)) {
        hi <- hi[plan$perm]
        lo <- lo[plan$perm]
        if (!is.null(y)) {
            y <- y[plan$perm]
        }
        rowid <- rowid[plan$perm]
    }

    est <- numeric(plan$n_comp)
    est_rowid <- rep(NA_integer_, plan$n_comp)
    phi <- matrix(0, nrow = n, ncol = plan$n_comp)

    for (g in plan$groups) {
        args <- g$args
        args$hi <- hi[g$idx]
        args$lo <- lo[g$idx]
        if (isTRUE(g$uses_y)) {
            args$y <- y[g$idx]
        }
        con <- do_call(g$fun, args)
        if (length(con) != length(g$out_idx)) {
            stop_sprintf("Internal error: comparison plan group changed shape.")
        }

        est[g$out_idx] <- con
        if (length(con) == length(g$idx)) {
            est_rowid[g$out_idx] <- rowid[g$idx]
        } else if (length(con) == 1L && length(g$idx) > 1L) {
            phi[, g$out_idx] <- get_unconditional_scalar_comparison_phi(
                group = g,
                hi = hi,
                lo = lo,
                y = y,
                rowid = rowid,
                n = n,
                theta = con
            )
        }
    }

    if (!is.null(plan$est_keep)) {
        est <- est[plan$est_keep]
        est_rowid <- est_rowid[plan$est_keep]
        phi <- phi[, plan$est_keep, drop = FALSE]
    }

    list(estimate = est, rowid = est_rowid, phi = phi)
}


get_unconditional_scalar_comparison_phi <- function(group, hi, lo, y, rowid, n, theta) {
    idx <- group$idx
    ng <- length(idx)
    out <- numeric(n)
    if (ng <= 1) {
        return(out)
    }

    for (j in seq_along(idx)) {
        keep <- seq_along(idx) != j
        args <- group$args
        args <- lapply(args, function(x) {
            if (length(x) == ng) {
                return(x[keep])
            }
            x
        })
        args$hi <- hi[idx][keep]
        args$lo <- lo[idx][keep]
        if (isTRUE(group$uses_y)) {
            args$y <- y[idx][keep]
        }
        theta_minus <- do_call(group$fun, args)
        if (length(theta_minus) != 1L || anyNA(theta_minus)) {
            stop_sprintf(
                "`vcov = \"unconditional\"` could not linearize a scalar comparison function."
            )
        }
        out[rowid[idx[j]]] <- out[rowid[idx[j]]] +
            (n / ng) * (ng - 1) * (theta - theta_minus)
    }
    out
}


get_unconditional_aggregate_phi <- function(agg, base_est, rowid, n, base_phi = NULL) {
    if (is.null(base_phi)) {
        base_phi <- matrix(0, nrow = n, ncol = length(base_est))
    }

    if (is.null(agg)) {
        return(base_phi)
    }

    out <- matrix(0, nrow = n, ncol = agg$n)

    for (block in agg$blocks) {
        idx_mat <- block$idx
        e <- base_est[idx_mat]
        dim(e) <- dim(idx_mat)
        rid <- rowid[idx_mat]
        dim(rid) <- dim(idx_mat)

        if (isTRUE(agg$weighted)) {
            w <- block$w
        } else {
            w <- matrix(1, nrow = nrow(idx_mat), ncol = ncol(idx_mat))
        }

        for (j in seq_along(block$cols)) {
            col <- block$cols[[j]]
            idx <- idx_mat[, j]
            ej <- e[, j]
            rj <- rid[, j]
            wj <- w[, j]
            ok <- !is.na(ej) & !is.na(wj) & wj != 0
            if (!any(ok)) {
                next
            }
            theta <- sum(ej[ok] * wj[ok]) / sum(wj[ok])
            alpha <- numeric(length(ej))
            alpha[ok] <- wj[ok] / sum(wj[ok])
            out[, col] <- out[, col] + as.vector(base_phi[, idx, drop = FALSE] %*% alpha)

            ok_rowid <- ok & !is.na(rj)
            if (any(ok_rowid)) {
                contrib <- numeric(length(ej))
                contrib[ok_rowid] <- n * wj[ok_rowid] / sum(wj[ok]) * (ej[ok_rowid] - theta)
                out[, col] <- out[, col] + rowsum_unconditional(contrib[ok_rowid], rj[ok_rowid], n)
            }
        }
    }

    out[is.na(out)] <- 0
    out
}


apply_unconditional_hypothesis_phi <- function(
    phi,
    plan,
    pre_hypothesis_estimate,
    numderiv = list("fdforward")) {

    if (is.null(plan$hyp)) {
        return(phi)
    }

    if (is.null(plan$agg)) {
        theta <- pre_hypothesis_estimate
    } else {
        theta <- apply_plan_aggregation(plan$agg, pre_hypothesis_estimate)
    }

    H <- get_jacobian(
        func = plan$hyp$apply,
        x = theta,
        numderiv = numderiv
    )
    phi %*% t(H)
}


rowsum_unconditional <- function(x, group, n) {
    out <- numeric(n)
    s <- rowsum(x, group, reorder = FALSE)
    idx <- as.integer(rownames(s))
    out[idx] <- s[, 1]
    out
}


get_unconditional_coef <- function(model) {
    beta <- stats::coef(model)
    if (anyNA(beta)) {
        stop_sprintf("`vcov = \"unconditional\"` does not support models with aliased coefficients.")
    }
    beta
}


get_unconditional_beta_dot <- function(model) {
    insight::check_if_installed("sandwich")
    if (inherits(model, "fixest")) {
        insight::check_if_installed("fixest")
        scores <- model$scores
        bread <- tryCatch(
            fixest::bread(model),
            error = function(e) {
                msg <- paste0(
                    "`vcov = \"unconditional\"` requires model scores and a ",
                    "bread matrix. `fixest::bread()` failed for this model: %s"
                )
                stop_sprintf(
                    msg,
                    conditionMessage(e)
                )
            }
        )
    } else {
        scores <- tryCatch(
            sandwich::estfun(model),
            error = function(e) {
                msg <- paste0(
                    "`vcov = \"unconditional\"` requires a ",
                    "`sandwich::estfun()` method for models of class \"%s\". ",
                    "Original error: %s"
                )
                stop_sprintf(
                    msg,
                    class(model)[1],
                    conditionMessage(e)
                )
            }
        )
        bread <- tryCatch(
            sandwich::bread(model),
            error = function(e) {
                msg <- paste0(
                    "`vcov = \"unconditional\"` requires a ",
                    "`sandwich::bread()` method for models of class \"%s\". ",
                    "Original error: %s"
                )
                stop_sprintf(
                    msg,
                    class(model)[1],
                    conditionMessage(e)
                )
            }
        )
    }

    beta <- get_coef(model)
    if (anyNA(beta)) {
        stop_sprintf("`vcov = \"unconditional\"` does not support models with aliased coefficients.")
    }
    if (is.null(names(beta)) || anyNA(names(beta)) || any(names(beta) == "")) {
        stop_sprintf("`vcov = \"unconditional\"` requires named model coefficients.")
    }
    if (!isTRUE(checkmate::check_matrix(scores, min.rows = 1, min.cols = 1))) {
        stop_sprintf(
            "`vcov = \"unconditional\"` could not extract a valid score matrix for this model."
        )
    }
    if (!isTRUE(checkmate::check_matrix(bread, nrows = ncol(scores), ncols = ncol(scores)))) {
        stop_sprintf(
            "`vcov = \"unconditional\"` could not extract a valid bread matrix for this model."
        )
    }
    if (is.null(colnames(scores)) && ncol(scores) == length(beta)) {
        colnames(scores) <- names(beta)
    }
    if (is.null(colnames(bread)) && ncol(bread) == length(beta)) {
        colnames(bread) <- names(beta)
    }
    if (is.null(rownames(bread)) && nrow(bread) == length(beta)) {
        rownames(bread) <- names(beta)
    }

    cols <- names(beta)
    missing_scores <- setdiff(cols, colnames(scores))
    missing_bread_rows <- setdiff(cols, rownames(bread))
    missing_bread_cols <- setdiff(cols, colnames(bread))
    if (length(missing_scores) > 0 || length(missing_bread_rows) > 0 || length(missing_bread_cols) > 0) {
        missing <- unique(c(missing_scores, missing_bread_rows, missing_bread_cols))
        msg <- paste0(
            "`vcov = \"unconditional\"` could not align model coefficients ",
            "with the score and bread matrices. Missing columns: %s."
        )
        stop_sprintf(msg, paste(missing, collapse = ", "))
    }

    scores <- scores[, cols, drop = FALSE]
    bread <- bread[cols, cols, drop = FALSE]
    scores %*% bread
}


get_unconditional_vcov <- function(Phi, inputs) {
    n <- inputs$n
    correction <- get_unconditional_correction(inputs)
    if (inputs$vcov$type == "cluster") {
        S <- rowsum(Phi, inputs$vcov$cluster, reorder = FALSE)
        return(crossprod(S) / n^2 * correction)
    }
    crossprod(Phi) / n^2 * correction
}


get_unconditional_correction <- function(inputs) {
    if (inputs$vcov$type == "HC0") {
        return(1)
    }

    model <- inputs$model
    n <- inputs$n
    is_linear <- is_unconditional_linear_model(model)
    df_residual <- tryCatch(stats::df.residual(model), error = function(e) NA_real_)
    if (length(df_residual) != 1L || !is.finite(df_residual)) {
        df_residual <- n - length(get_unconditional_coef(model))
    }

    if (inputs$vcov$type == "cluster") {
        G <- length(unique(inputs$vcov$cluster))
        if (G < 2) {
            stop_sprintf("Cluster-robust unconditional variance requires at least two clusters.")
        }
        if (isTRUE(is_linear)) {
            if (df_residual < 1) {
                msg <- paste0(
                    "`vcov = \"unconditional\"` requires more observations ",
                    "than estimated coefficients for finite-sample corrected ",
                    "linear inference. Use `unconditional(\"HC0\", df = Inf)` ",
                    "to omit the finite-sample correction."
                )
                stop_sprintf(msg)
            }
            return((G / (G - 1)) * ((n - 1) / df_residual))
        }
        return(G / (G - 1))
    }

    if (isTRUE(is_linear)) {
        if (df_residual < 1) {
            msg <- paste0(
                "`vcov = \"unconditional\"` requires more observations ",
                "than estimated coefficients for finite-sample corrected ",
                "linear inference. Use `unconditional(\"HC0\", df = Inf)` ",
                "to omit the finite-sample correction."
            )
            stop_sprintf(msg)
        }
        return(n / df_residual)
    }
    if (n <= 1) {
        msg <- paste0(
            "`vcov = \"unconditional\"` requires at least two observations ",
            "for finite-sample corrected inference. Use ",
            "`unconditional(\"HC0\", df = Inf)` to omit the finite-sample ",
            "correction."
        )
        stop_sprintf(msg)
    }
    n / (n - 1)
}
