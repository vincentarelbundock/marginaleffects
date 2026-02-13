#' Influence-function (unconditional) variance estimator
#'
#' Computes the variance of averaged predictions or comparisons using the
#' influence-function approach of Hansen & Overgaard (2025, Metrika). This
#' accounts for both parameter estimation uncertainty and the sampling
#' variability of the averaging, providing robustness to model misspecification.
#'
#' Supports subgroup estimation via the `by` argument. When `by` specifies a
#' grouping variable, the estimator computes group-specific influence functions
#' with Hajek-style normalized weights, and returns the full covariance matrix
#' (including cross-group terms) so that downstream hypothesis tests across
#' groups are valid.
#'
#' @param mfx A `marginaleffects_internal` S4 object (fully built)
#' @return A square covariance matrix whose dimensions match the number of
#'   output rows (L*G for predictions, K*G for comparisons, where G = number
#'   of subgroups).
#' @references
#' Hansen SN, Overgaard M (2025). "Variance estimation for average treatment
#' effects estimated by g-computation." Metrika, 88, 419--443.
#' @noRd
#' @keywords internal
get_vcov_unconditional <- function(mfx = NULL) {
    if (is.null(mfx)) {
        stop_sprintf(
            'vcov = "unconditional" is not supported in this context. Use avg_predictions() or avg_comparisons().'
        )
    }

    model <- mfx@model

    # --- Scope checks ---
    if (!class(model)[1] %in% c("lm", "glm")) {
        stop_sprintf('vcov = "unconditional" is only supported for lm and glm models.')
    }

    if (length(mfx@variables) == 0) {
        stop_sprintf('vcov = "unconditional" requires the `variables` argument.')
    }

    if (!isTRUE(mfx@by) && !is.character(mfx@by)) {
        stop_sprintf(
            'vcov = "unconditional" requires averaging over observations (e.g., avg_predictions() or avg_comparisons()).'
        )
    }

    # Only support "difference" comparison for now
    if (mfx@calling_function == "comparisons") {
        cmp <- mfx@comparison
        if (!is.null(cmp) && !identical(cmp, "difference") && !identical(cmp, "differenceavg")) {
            stop_sprintf('vcov = "unconditional" only supports comparison = "difference".')
        }
    }

    # sandwich is required
    if (!requireNamespace("sandwich", quietly = TRUE)) {
        insight::check_if_installed("sandwich")
    }

    # --- Retrieve training data ---
    modeldata <- mfx@modeldata
    if (is.null(modeldata) || nrow(modeldata) == 0) {
        modeldata <- get_modeldata(model)
    }
    if (is.null(modeldata) || nrow(modeldata) == 0) {
        stop_sprintf('vcov = "unconditional" requires access to the original training data, but none could be found.')
    }

    Y <- insight::get_response(model)
    n <- length(Y)

    # --- Identify intervention scenarios ---
    trt_name <- mfx@variables[[1]]$name
    trt_levels <- sort(unique(mfx@newdata[[trt_name]]))
    L <- length(trt_levels)

    if (L < 2) {
        stop_sprintf(
            'vcov = "unconditional" requires at least 2 levels in the treatment variable "%s".',
            trt_name
        )
    }

    beta <- get_coef(model)
    p <- length(beta)
    fam <- if (inherits(model, "glm")) family(model) else NULL

    # --- Determine subgroups from `by` ---
    if (isTRUE(mfx@by)) {
        group_var <- NULL
    } else {
        group_var <- setdiff(mfx@by, trt_name)
        if (length(group_var) == 0) group_var <- NULL
    }

    if (!is.null(group_var)) {
        if (length(group_var) > 1) {
            stop_sprintf(
                'vcov = "unconditional" supports at most one grouping variable in `by`. Got: %s.',
                paste(group_var, collapse = ", ")
            )
        }
        # For predictions, the treatment variable must be in `by` so output
        # has per-treatment-level rows
        if (mfx@calling_function == "predictions" && !trt_name %in% mfx@by) {
            stop_sprintf(
                'vcov = "unconditional" for predictions with `by` requires the treatment variable "%s" in `by`, e.g., by = c("%s", "%s").',
                trt_name, trt_name, group_var
            )
        }
        by_vals <- modeldata[[group_var]]
        if (is.null(by_vals)) {
            stop_sprintf(
                '`by` variable "%s" not found in the model data.',
                group_var
            )
        }
        group_levels <- sort(unique(by_vals))
        num_groups <- length(group_levels)
    } else {
        group_levels <- NULL
        num_groups <- 1
    }

    # --- Compute predictions and gradients for each intervention ---
    M <- matrix(NA_real_, nrow = n, ncol = L)
    D_list <- vector("list", L)

    for (ell in seq_len(L)) {
        cf_data <- modeldata
        cf_data[[trt_name]] <- trt_levels[ell]

        # Model matrix under intervention
        MM_a <- tryCatch(
            stats::model.matrix(
                stats::delete.response(stats::terms(model)),
                data = cf_data,
                xlev = model$xlevels
            ),
            error = function(e) {
                stop_sprintf(
                    'vcov = "unconditional" failed to construct model matrix for intervention level "%s": %s',
                    trt_levels[ell],
                    e$message
                )
            }
        )

        if (is.null(fam)) {
            # lm: identity link
            M[, ell] <- MM_a %*% beta
            D_list[[ell]] <- MM_a
        } else {
            # glm: general link
            eta <- as.vector(MM_a %*% beta)

            # handle offset
            offset <- stats::model.offset(stats::model.frame(model))
            if (!is.null(offset)) {
                off_cf <- tryCatch(
                    stats::model.offset(stats::model.frame(
                        stats::delete.response(stats::terms(model)),
                        data = cf_data
                    )),
                    error = function(e) NULL
                )
                if (!is.null(off_cf)) {
                    eta <- eta + off_cf
                }
            }

            M[, ell] <- fam$linkinv(eta)
            mu_eta <- fam$mu.eta(eta)
            D_list[[ell]] <- MM_a * mu_eta
        }
    }

    # --- Influence function for beta_hat ---
    # For M-estimators, IF_i = bread %*% psi_i.
    # Works for both lm and glm via sandwich::estfun/bread.
    psi <- sandwich::estfun(model)
    bread_mat <- sandwich::bread(model)
    B <- psi %*% bread_mat # n×p

    # --- Compute group-wise influence functions ---
    # For each group g, the influence function for the averaged prediction is:
    #   Phi_i(w_g) = w_tilde_g_i * (m_i - Theta_g) + G_hat_g %*% B_i
    # where w_tilde_g_i = w_g_i / sum(w_g) is the Hajek weight.
    # The mean part is nonzero only for observations in group g, but the
    # beta-estimation part is nonzero for ALL observations.

    contrast_mat <- NULL
    if (mfx@calling_function == "comparisons") {
        contrast_mat <- build_contrast_matrix_unconditional(trt_levels, mfx)
    }

    if (num_groups == 1) {
        # --- No subgroups: uniform weights (original behavior) ---
        w_tilde <- rep(1 / n, n)
        Theta_hat <- colSums(w_tilde * M)
        G_hat <- do.call(rbind, lapply(D_list, function(D) colSums(w_tilde * D)))

        mean_part <- sweep(M, 2, Theta_hat, "-") * w_tilde
        beta_part <- B %*% t(G_hat)
        Phi <- mean_part + beta_part

        if (!is.null(contrast_mat)) {
            Phi <- Phi %*% t(contrast_mat)
        }

        Gamma_hat <- crossprod(Phi) / n
        V_out <- Gamma_hat / n
    } else {
        # --- Subgroup computation ---
        Phi_list <- vector("list", num_groups)

        for (g in seq_len(num_groups)) {
            w <- as.numeric(by_vals == group_levels[g])
            w_sum <- sum(w)
            w_tilde <- w / w_sum

            Theta_g <- colSums(w_tilde * M)
            G_hat_g <- do.call(rbind, lapply(D_list, function(D) colSums(w_tilde * D)))

            mean_part_g <- sweep(M, 2, Theta_g, "-") * w_tilde
            beta_part_g <- B %*% t(G_hat_g)
            Phi_g <- mean_part_g + beta_part_g # n×L

            if (!is.null(contrast_mat)) {
                Phi_g <- Phi_g %*% t(contrast_mat) # n×K
            }

            Phi_list[[g]] <- Phi_g
        }

        # Stack influence functions in the order matching the pipeline output.
        # - Comparisons with by="sex": get_by() uses keyby = c("term", "sex"),
        #   so output is sorted by group_var (for single term). Group-major.
        # - Predictions with by=c("trt","sex"): keyby follows the order in `by`.
        #   Treatment levels and groups are interleaved per that order.
        ncol_each <- ncol(Phi_list[[1]])

        if (mfx@calling_function == "comparisons") {
            # Group-major: (g1_k1, ..., g1_kK, g2_k1, ..., g2_kK)
            Phi_full <- do.call(cbind, Phi_list)
        } else {
            # Predictions: output sorted by keyby = by-columns (in user order).
            # Treatment variable and group variable positions in `by` determine
            # whether output is trt-major or group-major.
            trt_pos <- match(trt_name, mfx@by)
            grp_pos <- match(group_var, mfx@by)

            if (!is.na(trt_pos) && !is.na(grp_pos) && trt_pos < grp_pos) {
                # trt-major: (trt1/g1, trt1/g2, ..., trtL/g1, trtL/g2)
                Phi_full <- matrix(0, nrow = n, ncol = ncol_each * num_groups)
                col <- 0
                for (ell in seq_len(ncol_each)) {
                    for (g in seq_len(num_groups)) {
                        col <- col + 1
                        Phi_full[, col] <- Phi_list[[g]][, ell]
                    }
                }
            } else {
                # group-major: (g1/trt1, g1/trt2, ..., gG/trt1, gG/trt2)
                Phi_full <- do.call(cbind, Phi_list)
            }
        }

        Gamma_full <- crossprod(Phi_full) / n
        V_out <- Gamma_full / n
    }

    return(V_out)
}


#' Build contrast matrix for unconditional variance
#'
#' @param trt_levels Sorted vector of treatment levels
#' @param mfx The marginaleffects_internal object
#' @return K×L contrast matrix
#' @noRd
build_contrast_matrix_unconditional <- function(trt_levels, mfx) {
    L <- length(trt_levels)

    # For "difference": each non-reference level vs reference (first level)
    K <- L - 1
    C <- matrix(0, nrow = K, ncol = L)
    for (k in seq_len(K)) {
        C[k, 1] <- -1
        C[k, k + 1] <- 1
    }

    return(C)
}
