#' Unconditional (influence-function) variance for averaged predictions or
#' comparisons
#'
#' Recomputes standard errors on a `marginaleffects` object using the
#' influence-function approach of Hansen & Overgaard (2025, *Metrika*). This
#' accounts for both parameter estimation uncertainty and the sampling
#' variability of the covariate distribution, providing robustness to model
#' misspecification.
#'
#' The function is self-contained: it reads the model, data, and structure from
#' a finished `avg_predictions()` or `avg_comparisons()` result, computes the
#' unconditional variance-covariance matrix, and returns the object with
#' updated `std.error`, `statistic`, `p.value`, `s.value`, `conf.low`, and
#' `conf.high` columns.
#'
#' @param x An object produced by `avg_predictions()` or `avg_comparisons()`.
#'   The model must be `lm` or `glm`, and a single treatment variable
#'   (`variables` argument) is required.
#' @return A modified copy of `x` with unconditional standard errors and
#'   recomputed test statistics and confidence intervals. The unconditional
#'   variance-covariance matrix is stored in the `"marginaleffects"` attribute
#'   and can be retrieved with `attr(x, "marginaleffects")@vcov_model`.
#' @references
#' Hansen SN, Overgaard M (2025). "Variance estimation for average treatment
#' effects estimated by g-computation." *Metrika*, **88**, 419--443.
#' \doi{10.1007/s00184-024-00962-4}
#' @examples
#' mod <- lm(mpg ~ am + hp + wt, data = mtcars)
#' p <- avg_predictions(mod, variables = "am")
#' vcov_unconditional(p)
#'
#' cmp <- avg_comparisons(mod, variables = "am")
#' vcov_unconditional(cmp)
#'
vcov_unconditional <- function(x) {

    # --- Extract and validate the marginaleffects object ---
    mfx <- attr(x, "marginaleffects")
    if (is.null(mfx)) {
        marginaleffects:::stop_sprintf("vcov_unconditional() requires a marginaleffects object produced by avg_predictions() or avg_comparisons().")
    }

    model <- mfx@model
    if (!class(model)[1] %in% c("lm", "glm")) {
        marginaleffects:::stop_sprintf("vcov_unconditional() is only supported for lm and glm models.")
    }

    calling_function <- mfx@calling_function
    if (!calling_function %in% c("predictions", "comparisons")) {
        marginaleffects:::stop_sprintf("vcov_unconditional() only supports objects from avg_predictions() or avg_comparisons().")
    }

    if (length(mfx@variables) != 1) {
        marginaleffects:::stop_sprintf("vcov_unconditional() requires a `variables` argument of length 1.")
    }

    if (!isTRUE(mfx@by) && !is.character(mfx@by)) {
        marginaleffects:::stop_sprintf("vcov_unconditional() requires averaging over observations (e.g., avg_predictions() or avg_comparisons()).")
    }

    # Only support "difference" comparison for now
    if (calling_function == "comparisons") {
        cmp <- mfx@comparison
        if (!is.null(cmp) && !identical(cmp, "difference") && !identical(cmp, "differenceavg")) {
            marginaleffects:::stop_sprintf('vcov_unconditional() only supports comparison = "difference".')
        }
    }

    insight::check_if_installed("sandwich")

    modeldata <- mfx@modeldata
    Y <- insight::get_response(model)
    n <- length(Y)

    trt_name <- mfx@variables[[1]]$name
    trt_levels <- sort(unique(mfx@newdata[[trt_name]]))

    # The influence function requires averaging over the same observations used
    # to fit the model.  Detect when the user passed a custom `newdata`.
    if (calling_function == "predictions") {
        newdata_n <- nrow(mfx@newdata) / length(trt_levels)
    } else {
        newdata_n <- nrow(mfx@newdata)
    }
    if (newdata_n != n) {
        marginaleffects:::stop_sprintf(
            paste(
                "vcov_unconditional() requires that predictions are computed on",
                "the original model dataset (no custom `newdata`). The model was",
                "fit on %d observations but the counterfactual grid implies %d."
            ),
            n, as.integer(newdata_n)
        )
    }
    L <- length(trt_levels)
    if (L < 2) {
        marginaleffects:::stop_sprintf(
            "vcov_unconditional() requires at least 2 levels in the treatment variable \"%s\".",
            trt_name
        )
    }

    fam <- if (inherits(model, "glm")) family(model) else NULL

    # --- Determine subgroups from `by` ---
    if (isTRUE(mfx@by)) {
        group_var <- NULL
    } else {
        group_var <- setdiff(mfx@by, trt_name)
        if (length(group_var) == 0) group_var <- NULL
    }

    by_vals <- NULL
    group_levels <- NULL
    num_groups <- 1L

    if (!is.null(group_var)) {
        if (length(group_var) > 1) {
            marginaleffects:::stop_sprintf(
                "vcov_unconditional() supports at most one grouping variable in `by`. Got: %s.",
                paste(group_var, collapse = ", ")
            )
        }
        if (calling_function == "predictions" && !trt_name %in% mfx@by) {
            marginaleffects:::stop_sprintf(
                'vcov_unconditional() for predictions with `by` requires the treatment variable "%s" in `by`.',
                trt_name
            )
        }
        by_vals <- modeldata[[group_var]]
        if (is.null(by_vals)) {
            marginaleffects:::stop_sprintf('`by` variable "%s" not found in the model data.', group_var)
        }
        group_levels <- sort(unique(by_vals))
        num_groups <- length(group_levels)
    }

    # --- Contrast matrix for comparisons ---
    # For avg_comparisons(), the estimand is Delta = C * Theta where C is a
    # K x L contrast matrix.  The IF propagates linearly.
    contrast_mat <- NULL
    if (calling_function == "comparisons") {
        K <- L - 1
        contrast_mat <- matrix(0, nrow = K, ncol = L)
        for (k in seq_len(K)) {
            contrast_mat[k, 1] <- -1
            contrast_mat[k, k + 1] <- 1
        }
    }

    # --- Counterfactual predictions and gradients ---
    # Hansen & Overgaard (2025), Eq. (4)
    beta <- marginaleffects:::get_coef(model)
    mu_hat <- matrix(NA_real_, nrow = n, ncol = L)
    dmu_dbeta <- vector("list", L)

    for (ell in seq_len(L)) {
        cf_data <- modeldata
        cf_data[[trt_name]] <- trt_levels[ell]

        # Counterfactual data has treatment set to a constant, so
        # model.matrix(model, ...) may fail on factor contrasts.  Fall back
        # to terms-based construction when that happens.
        MM_a <- tryCatch(
            marginaleffects:::get_model_matrix(model, newdata = cf_data, mfx = mfx),
            error = function(e) {
                tryCatch(
                    stats::model.matrix(
                        stats::delete.response(stats::terms(model)),
                        data = cf_data,
                        xlev = model$xlevels
                    ),
                    error = function(e2) {
                        marginaleffects:::stop_sprintf(
                            "vcov_unconditional() failed to construct model matrix for intervention level \"%s\": %s",
                            trt_levels[ell], e$message
                        )
                    }
                )
            }
        )
        if (is.null(MM_a)) {
            marginaleffects:::stop_sprintf(
                "vcov_unconditional() failed to construct model matrix for intervention level \"%s\".",
                trt_levels[ell]
            )
        }

        if (is.null(fam)) {
            # lm: mu = X * beta
            mu_hat[, ell] <- MM_a %*% beta
            dmu_dbeta[[ell]] <- MM_a
        } else {
            # glm: mu = linkinv(X * beta + offset)
            eta <- as.vector(MM_a %*% beta)

            offset <- stats::model.offset(stats::model.frame(model))
            if (!is.null(offset)) {
                off_cf <- stats::model.offset(stats::model.frame(
                    stats::delete.response(stats::terms(model)),
                    data = cf_data
                ))
                if (is.null(off_cf)) {
                    marginaleffects:::stop_sprintf(
                        "vcov_unconditional() requires the offset variable to be present in the counterfactual data for intervention level \"%s\".",
                        trt_levels[ell]
                    )
                }
                eta <- eta + off_cf
            }

            mu_hat[, ell] <- fam$linkinv(eta)
            mu_eta <- fam$mu.eta(eta)
            dmu_dbeta[[ell]] <- MM_a * mu_eta
        }
    }

    # --- Influence function for beta_hat ---
    # Hansen & Overgaard (2025), Eq. (25)
    psi <- sandwich::estfun(model)
    bread_mat <- sandwich::bread(model)
    beta_dot <- psi %*% bread_mat  # n x p

    # --- Helper: influence-function contributions for a set of weights ---
    compute_phi <- function(w_tilde) {
        Theta <- colSums(w_tilde * mu_hat)
        G_hat <- do.call(rbind, lapply(dmu_dbeta, function(D) colSums(w_tilde * D)))
        mean_part <- sweep(mu_hat, 2, Theta, "-") * w_tilde
        beta_part <- beta_dot %*% t(G_hat)
        Phi <- mean_part + beta_part
        if (!is.null(contrast_mat)) {
            Phi <- Phi %*% t(contrast_mat)
        }
        Phi
    }

    # --- Compute variance ---
    if (num_groups == 1L) {
        # Full-sample: Hansen & Overgaard (2025), Eq. (8)
        Phi <- compute_phi(rep(1 / n, n))
        V <- crossprod(Phi) / n^2

    } else {
        # Subgroup estimation: Hansen & Overgaard (2025), Eq. (14)
        Phi_list <- vector("list", num_groups)
        for (g in seq_len(num_groups)) {
            w <- as.numeric(by_vals == group_levels[g])
            Phi_list[[g]] <- compute_phi(w / sum(w))
        }

        ncol_each <- ncol(Phi_list[[1]])

        if (calling_function == "comparisons") {
            Phi_full <- do.call(cbind, Phi_list)
        } else {
            trt_pos <- match(trt_name, mfx@by)
            grp_pos <- match(group_var, mfx@by)
            if (!is.na(trt_pos) && !is.na(grp_pos) && trt_pos < grp_pos) {
                Phi_full <- matrix(0, nrow = n, ncol = ncol_each * num_groups)
                col <- 0
                for (ell in seq_len(ncol_each)) {
                    for (g in seq_len(num_groups)) {
                        col <- col + 1
                        Phi_full[, col] <- Phi_list[[g]][, ell]
                    }
                }
            } else {
                Phi_full <- do.call(cbind, Phi_list)
            }
        }

        V <- crossprod(Phi_full) / n^2
    }

    # --- Overwrite SEs and recompute test statistics / CIs ---
    se <- sqrt(diag(V))
    if (length(se) != nrow(x)) {
        marginaleffects:::stop_sprintf(
            "vcov_unconditional(): unconditional vcov has %d rows but the object has %d rows.",
            length(se), nrow(x)
        )
    }
    x$std.error <- se

    # Recompute statistic, p.value, s.value, conf.low, conf.high
    # Remove columns that get_ci_internal will recompute
    x$statistic <- NULL
    x$p.value <- NULL
    x$s.value <- NULL
    x$conf.low <- NULL
    x$conf.high <- NULL

    x <- marginaleffects:::get_ci_internal(
        x,
        conf_level = mfx@conf_level,
        df = mfx@df,
        draws = mfx@draws,
        hypothesis_null = mfx@hypothesis_null,
        hypothesis_direction = mfx@hypothesis_direction,
        model = mfx@model
    )

    # Store the unconditional vcov on the internal object
    mfx@vcov_model <- V
    mfx@vcov_type <- "unconditional"
    mfx@jacobian <- NULL
    attr(x, "marginaleffects") <- mfx

    x
}
