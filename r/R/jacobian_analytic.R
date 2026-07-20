jacobian_analytic_comparison_groups <- function(J, plan) {
  out <- matrix(NA_real_, nrow = plan$n_comp, ncol = ncol(J))

  for (g in plan$groups) {
    block <- J[g$idx, , drop = FALSE]
    value <- switch(g$fun_key,
      difference = block,
      differenceavg = matrix(colMeans(block), nrow = 1L),
      differenceavgwts = {
        w <- g$args$w
        if (
          !is.numeric(w) || length(w) != nrow(block) ||
            any(!is.finite(w)) || sum(w) == 0
        ) {
          return(NULL)
        }
        matrix(colSums(block * w) / sum(w), nrow = 1L)
      },
      return(NULL)
    )
    if (nrow(value) != length(g$out_idx)) {
      return(NULL)
    }
    out[g$out_idx, ] <- value
  }

  out
}


jacobian_analytic_aggregate <- function(J, agg) {
  if (is.null(agg)) {
    return(J)
  }
  out <- matrix(NA_real_, nrow = agg$n, ncol = ncol(J))

  # Each block stores equal-size groups as columns of an index matrix. Flatten
  # once, then rowsum() every Jacobian column simultaneously by recorded group.
  for (block in agg$blocks) {
    idx <- block$idx
    if (
      !is.matrix(idx) || nrow(idx) == 0L ||
        any(idx < 1L | idx > nrow(J))
    ) {
      return(NULL)
    }
    group <- rep(seq_len(ncol(idx)), each = nrow(idx))
    value <- J[as.vector(idx), , drop = FALSE]

    if (isTRUE(agg$weighted)) {
      w <- block$w
      if (
        !is.numeric(w) || !identical(dim(w), dim(idx)) ||
          any(!is.finite(w))
      ) {
        return(NULL)
      }
      denominator <- colSums(w)
      if (any(denominator == 0)) {
        return(NULL)
      }
      value <- rowsum(value * as.vector(w), group, reorder = FALSE)
      value <- value / denominator
    } else {
      value <- rowsum(value, group, reorder = FALSE) / nrow(idx)
    }
    out[block$cols, ] <- value
  }

  out
}


jacobian_analytic_hypothesis <- function(J, hyp) {
  if (is.null(hyp)) {
    return(J)
  }
  H <- hyp$H
  if (
    !identical(hyp$kind, "matrix") || is.null(H) ||
      nrow(H) != nrow(J) || any(!is.finite(H))
  ) {
    return(NULL)
  }

  # Linear hypotheses map estimates with crossprod(H, estimate), so the same
  # multiplication maps every coefficient column of the Jacobian at once.
  as.matrix(Matrix::crossprod(H, J))
}


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

      if (
        !kind %in% c("comparisons", "predictions") ||
          !identical(plan$kind, kind) ||
          (!is.null(plan$hyp) && !identical(plan$hyp$kind, "matrix"))
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

      if (identical(kind, "comparisons")) {
        if (
          is.null(contrast_data) || isTRUE(plan$need_y) ||
            length(plan$groups) == 0L
        ) {
          return(NULL)
        }
        group_ok <- vapply(plan$groups, function(g) {
          g$fun_key %in% c("difference", "differenceavg", "differenceavgwts") &&
            !isTRUE(g$uses_y)
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

        # Form the complete raw derivative once. Recorded groups and aggregation
        # then operate on all coefficient columns, never one column at a time.
        J <- X_hi - X_lo
        raw_difference <- drop(J %*% beta)
        replay <- comparison_plan_apply(plan, raw_difference, numeric(nrow(J)))
      } else {
        X <- align_matrix(attr(
          plan$predict_args$newdata,
          "marginaleffects_model_matrix"
        ))
        if (is.null(X)) {
          return(NULL)
        }
        J <- X
        replay <- prediction_plan_apply(plan, drop(X %*% beta))
      }

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

      if (identical(kind, "comparisons")) {
        if (!is.null(plan$na_keep)) {
          J <- J[plan$na_keep, , drop = FALSE]
        }
        if (!is.null(plan$perm)) {
          J <- J[plan$perm, , drop = FALSE]
        }
        J <- jacobian_analytic_comparison_groups(J, plan)
        if (is.null(J)) {
          return(NULL)
        }
        if (!is.null(plan$est_keep)) {
          J <- J[plan$est_keep, , drop = FALSE]
        }
      } else if (!is.null(plan$keep)) {
        J <- J[plan$keep, , drop = FALSE]
      }

      J <- jacobian_analytic_aggregate(J, plan$agg)
      if (is.null(J)) {
        return(NULL)
      }
      J <- jacobian_analytic_hypothesis(J, plan$hyp)
      if (is.null(J)) {
        return(NULL)
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
