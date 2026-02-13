#' Influence-function (unconditional) variance estimator
#'
#' Computes the variance of averaged predictions or comparisons using the
#' influence-function approach of Hansen & Overgaard (2025, Metrika). This
#' accounts for both parameter estimation uncertainty and the sampling
#' variability of the averaging.
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
#' Equation numbers below refer to this paper.
#' @noRd
#' @keywords internal
get_vcov_unconditional <- function(mfx = NULL) {
    inputs <- validate_unconditional_inputs(mfx)
    model <- inputs$model
    modeldata <- inputs$modeldata
    n <- inputs$n
    trt_name <- inputs$trt_name
    trt_levels <- inputs$trt_levels
    L <- inputs$L
    fam <- inputs$fam
    group_var <- inputs$group_var
    by_vals <- inputs$by_vals
    group_levels <- inputs$group_levels
    num_groups <- inputs$G
    contrast_mat <- inputs$contrast_mat

    beta <- get_coef(model)

    # --- Compute counterfactual predictions and gradients ---
    # Hansen & Overgaard (2025), Eq. (4):
    #   theta_n^a = (1/n) * sum_i mu(beta_hat; X_i^a)
    # For each intervention level a_ell, we construct counterfactual data
    # X_i^{a_ell} (replace treatment with a_ell, keep covariates) and compute:
    #   mu_hat[i, ell] = mu(beta_hat; X_i^{a_ell})    -- individual predictions
    #   dmu_dbeta[[ell]][i, ] = d/dbeta mu(beta_hat; X_i^{a_ell})  -- gradient rows
    # For lm: mu = X*beta, gradient = X (model matrix).
    # For glm: mu = linkinv(X*beta), gradient = X * mu.eta(eta) (chain rule).
    mu_hat <- matrix(NA_real_, nrow = n, ncol = L)
    dmu_dbeta <- vector("list", L)

    for (ell in seq_len(L)) {
        cf_data <- modeldata
        cf_data[[trt_name]] <- trt_levels[ell]

        MM_a <- tryCatch(
            get_model_matrix(model, newdata = cf_data, mfx = mfx),
            error = function(e) {
                stop_sprintf(
                    'vcov = "unconditional" failed to construct model matrix for intervention level "%s": %s',
                    trt_levels[ell],
                    e$message
                )
            })

        if (is.null(MM_a)) {
            stop_sprintf(
                'vcov = "unconditional" failed to construct model matrix for intervention level "%s".',
                trt_levels[ell]
            )
        }

        if (is.null(fam)) {
            mu_hat[, ell] <- MM_a %*% beta
            dmu_dbeta[[ell]] <- MM_a
        } else {
            eta <- as.vector(MM_a %*% beta)

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

            mu_hat[, ell] <- fam$linkinv(eta)
            mu_eta <- fam$mu.eta(eta)
            dmu_dbeta[[ell]] <- MM_a * mu_eta
        }
    }

    # --- Influence function for beta_hat ---
    # Hansen & Overgaard (2025), Eq. (25):
    #   beta_dot(z) = -M_n^{-1} H(beta_hat; x) (y - mu(beta_hat; x))
    # For M-estimators defined by sum_i psi(Z_i, beta) = 0, the IF is:
    #   beta_dot(Z_i) = -M^{-1} psi(Z_i, beta_hat)
    # In sandwich notation: bread = n * M^{-1}, estfun = psi.
    # So beta_dot_i = psi_i %*% bread = n * (-M^{-1}) psi_i, which gives the
    # correctly scaled IF contribution for each observation.
    psi <- sandwich::estfun(model)
    bread_mat <- sandwich::bread(model)
    beta_dot <- psi %*% bread_mat # n x p

    if (num_groups == 1) {
        # =================================================================
        # Full-sample (no subgroups): Hansen & Overgaard (2025), Eq. (8)
        # =================================================================
        w_tilde <- rep(1 / n, n)
        Phi <- compute_phi_for_weights(
            mu_hat = mu_hat,
            dmu_dbeta = dmu_dbeta,
            beta_dot = beta_dot,
            w_tilde = w_tilde,
            contrast_mat = contrast_mat
        )

        # Eq. (8): plug-in IF covariance and final variance
        #   Gamma_hat = (1/n) sum_i Phi_i Phi_i'   (outer-product average)
        #   Var(Theta_hat) = Gamma_hat / n
        Gamma_hat <- crossprod(Phi) / n
        V_out <- Gamma_hat / n
    } else {
        # =================================================================
        # Subgroup estimation: Hansen & Overgaard (2025), Eq. (14)
        # =================================================================
        # For subset V_i = v with m = sum_i 1(V_i = v) observations:
        #   psi_n^{a,v} = (1/m) sum_{j: V_j=v} mu(beta_hat; X_j^a)
        # The influence function (Eq. 14) uses Hajek-style weights:
        #   w_i = 1(V_i = v),  w_tilde_i = w_i / sum(w) = 1(V_i=v) / m
        # Lambda_n^{a,v} = (1/n) sum_i {
        #   [mu(beta_hat; X_i^a) - psi_n^{a,v}] * 1(V_i=v) / (m/n)
        #   + [(1/m) sum_{j:V_j=v} d/dbeta mu(beta_hat; X_j^a)] * beta_dot(Z_i)
        # }^{otimes 2}
        #
        # Key: the mean part is nonzero only for observations in the subgroup
        # (V_i = v), but the beta-estimation part uses beta_dot(Z_i) from ALL
        # observations, because everyone contributes to estimating beta.
        Phi_list <- vector("list", num_groups)

        for (g in seq_len(num_groups)) {
            w <- as.numeric(by_vals == group_levels[g])
            w_sum <- sum(w)
            w_tilde <- w / w_sum

            Phi_g <- compute_phi_for_weights(
                mu_hat = mu_hat,
                dmu_dbeta = dmu_dbeta,
                beta_dot = beta_dot,
                w_tilde = w_tilde,
                contrast_mat = contrast_mat
            )

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

        # Eq. (14): covariance via outer products of stacked IF contributions.
        # Gamma_full includes cross-group terms (groups share beta_hat).
        # Var = Gamma_full / n.
        Gamma_full <- crossprod(Phi_full) / n
        V_out <- Gamma_full / n
    }

    return(V_out)
}


#' Compute influence-function contributions for a given set of averaging weights
#'
#' @param mu_hat n x L matrix of counterfactual predictions
#' @param dmu_dbeta List of length L with n x p gradient matrices
#' @param beta_dot n x p influence-function contributions for beta_hat
#' @param w_tilde Length-n normalized weights
#' @param contrast_mat Optional K x L contrast matrix
#' @return n x L (or n x K when `contrast_mat` is supplied) matrix of IF terms
#' @noRd
compute_phi_for_weights <- function(mu_hat, dmu_dbeta, beta_dot, w_tilde, contrast_mat = NULL) {
    Theta <- colSums(w_tilde * mu_hat)
    G_hat <- do.call(rbind, lapply(dmu_dbeta, function(D) colSums(w_tilde * D)))
    mean_part <- sweep(mu_hat, 2, Theta, "-") * w_tilde
    beta_part <- beta_dot %*% t(G_hat)
    Phi <- mean_part + beta_part
    if (!is.null(contrast_mat)) {
        Phi <- Phi %*% t(contrast_mat)
    }
    return(Phi)
}


#' Validate inputs for unconditional variance and compute derived constants
#'
#' @param mfx A `marginaleffects_internal` S4 object (fully built)
#' @return Named list used by `get_vcov_unconditional()`
#' @noRd
validate_unconditional_inputs <- function(mfx) {
    if (is.null(mfx)) {
        stop_sprintf(
            'vcov = "unconditional" is not supported in this context. Use avg_predictions() or avg_comparisons().'
        )
    }

    model <- mfx@model
    if (!class(model)[1] %in% c("lm", "glm")) {
        stop_sprintf('vcov = "unconditional" is only supported for lm and glm models.')
    }

    if (length(mfx@variables) != 1) {
        stop_sprintf('vcov = "unconditional" requires a `variables` argument of length 1.')
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
    insight::check_if_installed("sandwich")

    modeldata <- mfx@modeldata
    Y <- insight::get_response(model)
    n <- length(Y)

    trt_name <- mfx@variables[[1]]$name
    trt_levels <- sort(unique(mfx@newdata[[trt_name]]))
    L <- length(trt_levels)
    if (L < 2) {
        stop_sprintf(
            'vcov = "unconditional" requires at least 2 levels in the treatment variable "%s".',
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
        G <- length(group_levels)
    } else {
        by_vals <- NULL
        group_levels <- NULL
        G <- 1
    }

    # For avg_comparisons(), the estimand is Delta = C * Theta where C is a
    # K x L contrast matrix. The IF propagates linearly:
    #   Phi_i^Delta = C * Phi_i  (in code: Phi %*% t(C))
    # and the covariance propagates as Var(Delta) = C Var(Theta) C'.
    contrast_mat <- NULL
    if (mfx@calling_function == "comparisons") {
        contrast_mat <- build_contrast_matrix_unconditional(trt_levels, mfx)
    }

    return(list(
        model = model,
        modeldata = modeldata,
        Y = Y,
        n = n,
        trt_name = trt_name,
        trt_levels = trt_levels,
        L = L,
        fam = fam,
        group_var = group_var,
        by_vals = by_vals,
        group_levels = group_levels,
        G = G,
        contrast_mat = contrast_mat
    ))
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
