get_jacobian_analytic <- function(
  plan,
  mfx,
  kind,
  type,
  estimate,
  contrast_data = NULL
) {
  # NULL means that this estimand is not safely eligible. This is an expected
  # result which preserves the existing autodiff and finite-difference paths.
  tryCatch(
    {
      if (is.null(plan) || is.null(mfx)) {
        return(NULL)
      }

      model <- mfx@model
      model_class <- class(model)[1]

      # Keep this whitelist independent of autodiff: the R and JAX implementations
      # can support different models and operations without drifting together.
      eligible_type <-
        (identical(model_class, "lm") && type %in% c("response", "link")) ||
          (identical(model_class, "glm") && identical(type, "link"))
      if (!isTRUE(eligible_type) || !is.null(model[["offset"]])) {
        return(NULL)
      }

      # GLM comparisons are exact here only on the linear-predictor scale, where
      # the derivative of each prediction with respect to beta is its X row.
      beta <- get_coef(model)
      beta_names <- names(beta)
      if (
        !is.numeric(beta) || any(!is.finite(beta)) || is.null(beta_names) ||
          anyDuplicated(beta_names) > 0L
      ) {
        return(NULL)
      }

      # Start with the case where the complete estimand is a row-wise linear map.
      # Aggregation and hypotheses remain on the established fallback until a
      # matrix-native implementation shows a measurable end-to-end benefit.
      if (
        !identical(kind, "comparisons") ||
          !identical(plan$kind, "comparisons") ||
          !is.null(plan$agg) || !is.null(plan$hyp) ||
          is.null(contrast_data) || isTRUE(plan$need_y) ||
          length(plan$groups) == 0L
      ) {
        return(NULL)
      }

      align_matrix <- function(X) {
        if (
          !isTRUE(checkmate::check_matrix(X, mode = "numeric")) ||
            ncol(X) != length(beta) || any(!is.finite(X))
        ) {
          return(NULL)
        }
        xnames <- colnames(X)
        if (
          is.null(xnames) || anyDuplicated(xnames) > 0L ||
            !setequal(xnames, beta_names)
        ) {
          return(NULL)
        }
        if (identical(xnames, beta_names)) {
          X
        } else {
          X[, beta_names, drop = FALSE]
        }
      }

      group_ok <- vapply(plan$groups, function(g) {
        identical(g$fun_key, "difference") &&
          !isTRUE(g$uses_y) &&
          length(g$idx) == length(g$out_idx)
      }, logical(1))
      if (!all(group_ok)) {
        return(NULL)
      }
      X_hi <- align_matrix(attr(
        contrast_data$hi,
        "marginaleffects_model_matrix"
      ))
      X_lo <- align_matrix(attr(
        contrast_data$lo,
        "marginaleffects_model_matrix"
      ))
      if (is.null(X_hi) || is.null(X_lo) || !identical(dim(X_hi), dim(X_lo))) {
        return(NULL)
      }
      # Apply the recorded row operations to the whole Jacobian. This is the core
      # optimization: X_hi - X_lo is formed once, with no replay per coefficient.
      J <- X_hi - X_lo
      raw_difference <- drop(J %*% beta)
      replay <- comparison_plan_apply(plan, raw_difference, numeric(nrow(J)))

      # This is a fail-closed correctness guard, not a debugging assertion. It
      # rejects stale matrices, offsets, prediction arguments, and future semantic
      # changes which are not captured by the static whitelist above.
      if (!isTRUE(all.equal(
        replay,
        estimate,
        tolerance = sqrt(.Machine$double.eps),
        check.attributes = FALSE
      ))) {
        return(NULL)
      }

      if (!is.null(plan$na_keep)) {
        J <- J[plan$na_keep, , drop = FALSE]
      }
      if (!is.null(plan$perm)) {
        J <- J[plan$perm, , drop = FALSE]
      }

      idx <- unlist(lapply(plan$groups, function(g) g$idx), use.names = FALSE)
      out_idx <- unlist(lapply(plan$groups, function(g) g$out_idx), use.names = FALSE)
      if (
        !identical(idx, seq_len(nrow(J))) ||
          !identical(out_idx, seq_len(plan$n_comp))
      ) {
        return(NULL)
      }
      if (!is.null(plan$est_keep)) {
        J <- J[plan$est_keep, , drop = FALSE]
      }

      if (nrow(J) != length(estimate)) {
        return(NULL)
      }
      # Cached model matrices may carry terms metadata such as `assign` and
      # `contrasts`. A Jacobian's contract includes only dimensions and names.
      attributes(J) <- list(
        dim = dim(J),
        dimnames = list(NULL, beta_names)
      )

      J
    },
    error = function(e) NULL
  )
}
