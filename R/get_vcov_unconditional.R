#' Influence-function (unconditional) variance estimator
#'
#' Computes the variance of averaged predictions or comparisons using the
#' influence-function approach of Hansen & Overgaard (2025, Metrika). This
#' accounts for both parameter estimation uncertainty and the sampling
#' variability of the averaging, providing robustness to model misspecification.
#'
#' @param mfx A `marginaleffects_internal` S4 object (fully built)
#' @return A square covariance matrix (L×L for predictions, K×K for
#'   comparisons).
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
                # Reconstruct offset for counterfactual data
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

    # --- Compute weighted averages ---
    w_tilde <- rep(1 / n, n)

    Theta_hat <- colSums(w_tilde * M) # L-vector
    G_hat <- do.call(rbind, lapply(D_list, function(D) colSums(w_tilde * D))) # L×p

    # --- Compute influence function for beta_hat: IF_i = -M^{-1} psi_i ---
    # For M-estimators, -M^{-1} = bread (sandwich notation).
    # Works for both lm and glm via sandwich::estfun/bread.
    psi <- sandwich::estfun(model)
    bread_mat <- sandwich::bread(model)
    B <- psi %*% bread_mat # n×p

    # --- Assemble influence contributions Phi (n×L) ---
    mean_part <- sweep(M, 2, Theta_hat, "-") * w_tilde # n×L
    beta_part <- B %*% t(G_hat) # n×L

    Phi <- mean_part + beta_part

    # --- Covariance ---
    Gamma_hat <- crossprod(Phi) / n # L×L
    V_pred <- Gamma_hat / n # L×L: Var(Theta_hat)

    # --- Apply contrast matrix for comparisons ---
    if (mfx@calling_function == "comparisons") {
        C <- build_contrast_matrix_unconditional(trt_levels, mfx)
        V_out <- C %*% V_pred %*% t(C)
    } else {
        V_out <- V_pred
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
