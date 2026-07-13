# Unconditional variance for averaged predictions and comparisons
#
# This file implements influence-function variance estimates that account for
# two sources of uncertainty: estimation of the model coefficients and sampling
# variation in the empirical distribution of the covariates over which results
# are averaged. At a high level, the implementation:
#
# 1. records and replays the prediction or comparison plan;
# 2. differentiates the final estimand with respect to model coefficients;
# 3. constructs its empirical-distribution influence component for population
#    or subgroup averages, including supported scalar contrasts;
# 4. obtains each coefficient's observation-level influence function and
#    combines the two components after aggregation and hypotheses; and
# 5. forms a robust or one-way cluster-robust covariance matrix from their sum.
#
# `vcovUnconditional()` creates the user-facing variance request. The
# `plan_unconditional_se()` path coordinates the calculation; the remaining
# helpers linearize predictions, comparisons, transformations, and model
# coefficients. Request validation and row-to-model-data matching live in
# `vcov_unconditional_sanitization.R`.


# User-facing request -------------------------------------------------------

#' Request unconditional variance in marginaleffects calls
#'
#' @param type Character string specifying the finite-sample adjustment. The
#'   available types are `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`,
#'   `"HC4"`, `"HC4m"`, and `"HC5"`. `"HC"` is an alias for `"HC0"`, and
#'   `"robust"` is an alias for `"HC1"`.
#' @param cluster An optional one-sided formula such as `~id` identifying the
#'   variable used for one-way clustered inference.
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
#' explicitly. HC2 through HC5 adjustments require the model to provide
#' leverage values through [stats::hatvalues()].
#'
#' @export
vcovUnconditional <- function(type = "HC1", cluster = NULL) {
    structure(
        list(type = type, cluster = cluster),
        class = "marginaleffects_vcov_unconditional"
    )
}


# Main influence-function calculation --------------------------------------

# Coordinate the complete unconditional-SE calculation for a recorded plan.
# This is the main bridge between the generic predictions/comparisons pipeline
# and the mathematical decomposition in equation (5) of the paper:
#
#   IF_i(theta) = empirical_i(theta) + J_beta(theta) IF_i(beta).
#
# The helpers below construct the two terms separately. Keeping them at the
# observation level until the end retains their covariance, which is the term
# needed for robustness to conditional-mean model misspecification.
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
        n_estimates = nrow(estimates),
        allow_mismatch = allow_mismatch
    )

    # Derivative of every final estimand with respect to beta. This includes
    # comparisons, aggregation, and hypotheses recorded in the plan.
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

    # Empirical component: variation from averaging over the observed
    # covariate distribution, holding beta fixed.
    empirical_phi <- get_unconditional_empirical_phi(
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

    # Equation (22)/(25): one row per observation, one column per coefficient.
    beta_dot <- get_unconditional_beta_dot(model)
    if (nrow(beta_dot) != n) {
        stop_sprintf(
            "`vcov = \"unconditional\"` requires one score row per model-data row."
        )
    }
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
    # Coefficient component in equations (5), (8), (14), and (17):
    # IF_i(beta)' J_beta(theta), for every observation and estimand.
    beta_phi <- beta_dot[, cols, drop = FALSE] %*% t(J[, cols, drop = FALSE])

    # Sum before taking cross-products. Expanding this square preserves the two
    # cross-covariance terms omitted by the simplified variance estimator in
    # equation (18); the result corresponds to the complete estimator in (17).
    Phi <- empirical_phi + beta_phi
    inputs <- list(
        model = model,
        n = n,
        vcov = unconditional$vcov
    )
    # Phi contains influence values, not per-estimate score contributions.
    # get_unconditional_vcov() therefore applies the n^-2 scaling required for
    # the covariance of the sample estimator.
    V <- get_unconditional_vcov(Phi, inputs)
    se <- sqrt(diag(V))
    se[se == 0] <- NA_real_

    estimates$std.error <- as.vector(se)
    if (unconditional_df_has_finite(mfx@df)) {
        estimates$df <- mfx@df
    }

    mfx@vcov_model <- V
    mfx@vcov_type <- if (!is.null(unconditional$vcov$cluster)) {
        sprintf("Unconditional (clustered by %s)", unconditional$vcov$cluster_var)
    } else {
        "Unconditional"
    }
    mfx@jacobian <- diag(nrow(V))

    list(mfx = mfx, estimates = estimates)
}


# Estimand Jacobian ---------------------------------------------------------

# Differentiate the complete recorded estimand with respect to model
# coefficients. Replaying the plan ensures J describes exactly what is shown
# to the user, including nonlinear comparisons, averages, and hypotheses.
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


# Empirical-distribution linearization -------------------------------------

# Construct the empirical influence component from a common base representation:
# estimates, source-row IDs, and any influence values already created by a
# scalar comparison. At this stage model coefficients are fixed; coefficient
# uncertainty is added once, centrally, in plan_unconditional_se().
get_unconditional_empirical_phi <- function(
    plan,
    model,
    n,
    rowid,
    numderiv = list("fdforward")) {

    if (identical(plan$kind, "predictions")) {
        estimate <- prediction_plan_predict(plan, model)
        if (!is.null(plan$keep)) {
            estimate <- estimate[plan$keep]
        }
        base <- list(
            estimate = estimate,
            rowid = rowid,
            phi = NULL
        )
    } else {
        base <- get_unconditional_comparison_base(plan, model, rowid, n)
    }

    phi <- get_unconditional_aggregate_phi(
        agg = plan$agg,
        base_est = base$estimate,
        rowid = base$rowid,
        n = n,
        base_phi = base$phi
    )

    apply_unconditional_hypothesis_phi(phi, plan, base$estimate, numderiv = numderiv)
}


# Reconstruct comparison estimates at fixed beta and attach an empirical
# influence column to each one. Vector-valued comparison functions remain at
# the unit level. Scalar functions have already aggregated several rows, so
# their influence functions must be constructed here explicitly.
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


# Linearize a scalar comparison over one group. Known smooth contrasts use the
# exact plug-in expressions from the paper. Arbitrary user functions fall back
# to delete-one jackknife pseudo-values, which provide a generic first-order
# approximation when no analytic empirical influence function is available.
get_unconditional_scalar_comparison_phi <- function(group, hi, lo, y, rowid, n, theta) {
    # Equations (17) and Corollary 9 in Hansen and Overgaard (2024)
    # give exact plug-in influence functions for differences and relative
    # effects. Prefer those to delete-one approximations for known contrasts.
    out <- get_unconditional_known_scalar_comparison_phi(
        group = group,
        hi = hi,
        lo = lo,
        rowid = rowid,
        n = n
    )
    if (!is.null(out)) {
        return(out)
    }

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
        # (ng - 1) * (theta - theta_minus) is the delete-one pseudo-value
        # centered at theta. n/ng converts a group-average contribution to an
        # influence value on the full-sample n^-1 sum scale used throughout.
        out[rowid[idx[j]]] <- out[rowid[idx[j]]] +
            (n / ng) * (ng - 1) * (theta - theta_minus)
    }
    out
}


# Exact empirical influence functions for built-in scalar averages. These are
# applications of the delta method to group means. In particular,
# `lnratioavg` implements Corollary 9, and `differenceavg` implements the
# empirical part of equation (17). Weighted variants target the corresponding
# weighted empirical distribution.
get_unconditional_known_scalar_comparison_phi <- function(
    group,
    hi,
    lo,
    rowid,
    n) {

    key <- group$fun_key
    supported <- c(
        "differenceavg", "differenceavgwts",
        "ratioavg", "ratioavgwts",
        "lnratioavg", "lnratioavgwts",
        "lnoravg", "lnoravgwts",
        "liftavg", "liftavgwts"
    )
    if (length(key) != 1L || is.na(key) || !key %in% supported) {
        return(NULL)
    }

    idx <- group$idx
    hi <- hi[idx]
    lo <- lo[idx]
    rid <- rowid[idx]
    weighted <- endsWith(key, "wts")
    w <- if (weighted) group$args$w else rep(1, length(idx))
    if (length(w) != length(idx) || anyNA(w) || any(!is.finite(w)) || sum(w) == 0) {
        return(NULL)
    }

    mean_hi <- sum(w * hi) / sum(w)
    mean_lo <- sum(w * lo) / sum(w)
    key <- sub("wts$", "", key)

    unit_phi <- switch(
        key,
        differenceavg = (hi - lo) - (mean_hi - mean_lo),
        ratioavg = {
            if (mean_lo == 0) return(NULL)
            (hi - mean_hi) / mean_lo -
                mean_hi * (lo - mean_lo) / mean_lo^2
        },
        lnratioavg = {
            if (mean_hi <= 0 || mean_lo <= 0) return(NULL)
            (hi - mean_hi) / mean_hi - (lo - mean_lo) / mean_lo
        },
        lnoravg = {
            if (mean_hi <= 0 || mean_hi >= 1 || mean_lo <= 0 || mean_lo >= 1) {
                return(NULL)
            }
            (hi - mean_hi) / (mean_hi * (1 - mean_hi)) -
                (lo - mean_lo) / (mean_lo * (1 - mean_lo))
        },
        liftavg = {
            if (mean_lo == 0) return(NULL)
            (hi - mean_hi) / mean_lo -
                mean_hi * (lo - mean_lo) / mean_lo^2
        }
    )

    # For equal weights this multiplier is n/ng, matching 1/P(V = v) in the
    # subgroup influence function (11). With sampling weights it becomes the
    # empirical analogue n*w_i/sum(w).
    contribution <- n * w / sum(w) * unit_phi
    rowsum_unconditional(contribution, rid, n)
}


# Aggregate empirical influence values using the exact groups and weights
# recorded by the estimation plan. For an unweighted subgroup, the direct term
# is n/m * (estimate_i - subgroup_mean), matching equations (11) and (14).
# `base_phi` carries influence already created by a scalar comparison through a
# possible second aggregation stage.
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
            # theta and alpha replay the displayed (possibly weighted) mean.
            theta <- sum(ej[ok] * wj[ok]) / sum(wj[ok])
            alpha <- numeric(length(ej))
            alpha[ok] <- wj[ok] / sum(wj[ok])
            # Linear combinations of already-linearized scalar estimates.
            out[, col] <- out[, col] + as.vector(base_phi[, idx, drop = FALSE] %*% alpha)

            # Direct empirical-distribution contribution of this average.
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


# Propagate empirical influence values through a post-aggregation hypothesis.
# If h is the hypothesis map, the delta method gives IF(h(theta)) = H IF(theta).
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


# Accumulate contributions that map to the same original observation. This is
# necessary for counterfactual grids, where several evaluation rows can share a
# single rowid/rowidcf and hence a single sampling unit.
rowsum_unconditional <- function(x, group, n) {
    out <- numeric(n)
    s <- rowsum(x, group, reorder = FALSE)
    idx <- as.integer(rownames(s))
    out[idx] <- s[, 1]
    out
}


# Model-coefficient influence functions ------------------------------------

# Extract the coefficient vector while rejecting aliased models, for which the
# score/bread columns cannot be put in one-to-one correspondence with beta.
get_unconditional_coef <- function(model) {
    beta <- get_coef(model)
    if (anyNA(beta)) {
        stop_sprintf("`vcov = \"unconditional\"` does not support models with aliased coefficients.")
    }
    beta
}


# Construct observation-level IF(beta). Under the sandwich convention,
# estfun() supplies estimating-function contributions and bread() supplies the
# inverse derivative matrix, so scores %*% bread is the plug-in version of
# equation (22)/(25). Model-specific branches normalize this same contract.
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

    beta <- get_unconditional_coef(model)
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


# Covariance assembly -------------------------------------------------------

# Convert the n by q influence matrix into the q by q covariance of q
# estimators. IID inference uses sum_i Phi_i Phi_i' / n^2. Clustered inference
# first sums Phi within clusters, which retains arbitrary within-cluster
# dependence before taking the cross-product.
get_unconditional_vcov <- function(Phi, inputs) {
    n <- inputs$n
    Phi <- adjust_unconditional_influence(Phi, inputs)
    correction <- get_unconditional_correction(inputs)
    if (!is.null(inputs$vcov$cluster)) {
        S <- rowsum(Phi, inputs$vcov$cluster, reorder = FALSE)
        return(crossprod(S) / n^2 * correction)
    }
    crossprod(Phi) / n^2 * correction
}


# HC2--HC5 adjust each observation's influence using the same leverage-based
# factors as sandwich::vcovHC(). HC0 and HC1 leave the influence values intact;
# their difference is the scalar finite-sample correction below.
adjust_unconditional_influence <- function(Phi, inputs) {
    type <- inputs$vcov$type
    if (type %in% c("HC0", "HC1")) {
        return(Phi)
    }

    h <- tryCatch(
        as.numeric(stats::hatvalues(inputs$model)),
        error = function(e) NULL
    )
    if (is.null(h) || length(h) != nrow(Phi) || anyNA(h)) {
        stop_sprintf(
            "`vcovUnconditional(type = \"%s\")` requires model leverage values from `hatvalues()`.",
            type
        )
    }
    if (any(h > 1 - sqrt(.Machine$double.eps))) {
        stop_sprintf(
            "`vcovUnconditional(type = \"%s\")` is undefined when leverage values are equal or close to 1.",
            type
        )
    }

    n <- length(h)
    p <- as.integer(round(sum(h), digits = 0))
    exponent <- switch(
        type,
        "HC2" = 1,
        "HC3" = 2,
        "HC4" = pmin(4, n * h / p),
        "HC4m" = pmin(1, n * h / p) + pmin(1.5, n * h / p),
        "HC5" = 0.5 * pmin(n * h / p, pmax(4, n * 0.7 * max(h) / p))
    )
    Phi / (1 - h)^(exponent / 2)
}


# Apply optional finite-sample corrections after constructing the asymptotic
# plug-in covariance. HC0 is the uncorrected estimator closest to the paper's
# formulas; HC1 uses a conventional degrees-of-freedom multiplier, and all
# clustered requests use a cluster-count multiplier.
get_unconditional_correction <- function(inputs) {
    clustered <- !is.null(inputs$vcov$cluster)
    if (!clustered && inputs$vcov$type != "HC1") return(1)
    model <- inputs$model
    n <- inputs$n
    is_linear <- is_unconditional_linear_model(model)
    df_residual <- tryCatch(stats::df.residual(model), error = function(e) NA_real_)
    if (length(df_residual) != 1L || !is.finite(df_residual)) {
        df_residual <- n - length(get_unconditional_coef(model))
    }
    if (clustered) {
        G <- length(unique(inputs$vcov$cluster))
        if (G < 2) stop_sprintf("Cluster-robust unconditional variance requires at least two clusters.")
        if (isTRUE(is_linear)) {
            if (df_residual < 1) stop_sprintf("`vcov = \"unconditional\"` requires more observations than estimated coefficients for finite-sample corrected linear inference. Use `vcovUnconditional(type = \"HC0\")` with `df = Inf` to omit the finite-sample correction.")
            if (inputs$vcov$type == "HC1") {
                return((G / (G - 1)) * ((n - 1) / df_residual))
            }
            return(G / (G - 1))
        }
        return(G / (G - 1))
    }
    if (isTRUE(is_linear)) {
        if (df_residual < 1) stop_sprintf("`vcov = \"unconditional\"` requires more observations than estimated coefficients for finite-sample corrected linear inference. Use `vcovUnconditional(type = \"HC0\")` with `df = Inf` to omit the finite-sample correction.")
        return(n / df_residual)
    }
    if (n <= 1) stop_sprintf("`vcov = \"unconditional\"` requires at least two observations for finite-sample corrected inference. Use `vcovUnconditional(type = \"HC0\")` with `df = Inf` to omit the finite-sample correction.")
    n / (n - 1)
}


# Output propagation --------------------------------------------------------

# Decide whether finite degrees of freedom should be copied into the displayed
# estimates. The value itself is inherited from the user-facing function and is
# deliberately not computed by the unconditional variance machinery.
unconditional_df_has_finite <- function(df) {
    is.numeric(df) && any(is.finite(df))
}


# Apply the multivariate delta method after a user-requested transformation.
# If g maps the estimate vector to a transformed vector and H is its Jacobian,
# this replaces V with H V H'. The saved unconditional covariance must be
# transformed here because finalize_estimates() otherwise transforms only the
# displayed estimates and standard errors.
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
