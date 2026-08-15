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


jacobian_analytic_weighted_columns <- function(X, idx, w) {
  if (identical(idx, seq_len(nrow(X)))) {
    return(drop(crossprod(w, X)))
  }
  drop(crossprod(w, X[idx, , drop = FALSE]))
}


# Average simple differences directly from cached matrices. This composes the
# comparison-row mapping with the recorded aggregation weights, avoiding an
# intermediate n-observation Jacobian for avg_comparisons().
jacobian_analytic_comparison_aggregate <- function(
  X_hi,
  X_lo,
  d_hi,
  d_lo,
  plan
) {
  avg_keys <- c("differenceavg", "differenceavgwts")
  group_keys <- vapply(plan$groups, `[[`, character(1), "fun_key")

  # avg_comparisons() normally records the averaging operation directly in
  # each comparison group. Compute those small output rows without ever
  # allocating the corresponding observation-level derivative matrix.
  if (length(group_keys) > 0L && all(group_keys %in% avg_keys)) {
    out <- matrix(NA_real_, nrow = plan$n_comp, ncol = ncol(X_hi))
    for (g in plan$groups) {
      if (length(g$out_idx) != 1L || length(g$idx) == 0L) {
        return(NULL)
      }
      if (identical(g$fun_key, "differenceavgwts")) {
        w <- g$args$w
        if (
          !is.numeric(w) || length(w) != length(g$idx) ||
            any(!is.finite(w))
        ) {
          return(NULL)
        }
        denominator <- sum(w)
        if (denominator == 0) {
          return(NULL)
        }
        w <- w / denominator
      } else {
        w <- rep.int(1 / length(g$idx), length(g$idx))
      }
      w_hi <- if (is.null(d_hi)) w else w * d_hi[g$idx]
      w_lo <- if (is.null(d_lo)) w else w * d_lo[g$idx]
      out[g$out_idx, ] <-
        jacobian_analytic_weighted_columns(X_hi, g$idx, w_hi) -
        jacobian_analytic_weighted_columns(X_lo, g$idx, w_lo)
    }
    if (!is.null(plan$est_keep)) {
      out <- out[plan$est_keep, , drop = FALSE]
    }
    return(list(J = out, aggregated = FALSE))
  }

  if (is.null(plan$agg) || length(plan$agg$blocks) == 0L) {
    return(NULL)
  }
  group_ok <- vapply(plan$groups, function(g) {
    identical(g$fun_key, "difference") &&
      length(g$idx) == length(g$out_idx)
  }, logical(1))
  if (!all(group_ok)) {
    return(NULL)
  }

  raw_index <- integer(plan$n_comp)
  for (g in plan$groups) {
    raw_index[g$out_idx] <- g$idx
  }
  if (!is.null(plan$est_keep)) {
    raw_index <- raw_index[plan$est_keep]
  }
  if (length(raw_index) == 0L || any(raw_index == 0L)) {
    return(NULL)
  }

  out <- matrix(NA_real_, nrow = plan$agg$n, ncol = ncol(X_hi))
  for (block in plan$agg$blocks) {
    idx <- block$idx
    if (
      !is.matrix(idx) || nrow(idx) == 0L ||
        any(idx < 1L | idx > length(raw_index))
    ) {
      return(NULL)
    }
    for (j in seq_len(ncol(idx))) {
      raw <- raw_index[idx[, j]]
      if (isTRUE(plan$agg$weighted)) {
        w <- block$w[, j]
        denominator <- sum(w)
        if (any(!is.finite(w)) || denominator == 0) {
          return(NULL)
        }
        w <- w / denominator
      } else {
        w <- rep.int(1 / length(raw), length(raw))
      }
      w_hi <- if (is.null(d_hi)) w else w * d_hi[raw]
      w_lo <- if (is.null(d_lo)) w else w * d_lo[raw]
      value <- jacobian_analytic_weighted_columns(X_hi, raw, w_hi) -
        jacobian_analytic_weighted_columns(X_lo, raw, w_lo)
      out[block$cols[j], ] <- value
    }
  }
  list(J = out, aggregated = TRUE)
}


#' Get an exact analytic Jacobian when supported
#' @param model A model object.
#' @param ... Arguments passed to model-specific methods.
#' @return A numeric Jacobian matrix, or `NULL` when the model or estimand is
#'   not eligible for the analytic path.
#' @keywords internal
#' @noRd
get_jacobian_analytic <- function(model, ...) {
  UseMethod("get_jacobian_analytic", model)
}


#' @noRd
#' @export
get_jacobian_analytic.default <- function(model, ...) {
  NULL
}


jacobian_analytic_model_matrix <- function(
  model,
  plan,
  kind,
  type,
  estimate,
  contrast_data = NULL,
  response_scale = FALSE,
  family = NULL
) {
  # NULL means that this estimand is not safely eligible. This is an expected
  # result which preserves the existing autodiff and finite-difference paths.
  tryCatch(
    {
      if (is.null(plan) || model_has_effective_offset(model)) {
        return(NULL)
      }

      if (isTRUE(response_scale)) {
        if (
          !is.list(family) || !is.function(family$linkinv) ||
            !is.function(family$mu.eta)
        ) {
          return(NULL)
        }
      }

      # Link-scale derivatives are X rows. Eligible response-scale models add
      # the inverse-link derivative below.
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

        reuse <- isTRUE(plan$model_matrix_used) &&
          identical(colnames(X_hi), beta_names) &&
          identical(colnames(X_lo), beta_names) &&
          is.numeric(plan$baseline_hi) &&
          length(plan$baseline_hi) == nrow(X_hi) &&
          is.numeric(plan$baseline_lo) &&
          length(plan$baseline_lo) == nrow(X_lo)
        eta_reuse <- reuse &&
          is.numeric(plan$eta_hi) && length(plan$eta_hi) == nrow(X_hi) &&
          is.numeric(plan$eta_lo) && length(plan$eta_lo) == nrow(X_lo)
        if (isTRUE(response_scale) && !eta_reuse) {
          eta_hi <- drop(X_hi %*% beta)
          eta_lo <- drop(X_lo %*% beta)
        } else if (eta_reuse) {
          eta_hi <- plan$eta_hi
          eta_lo <- plan$eta_lo
        }
        if (reuse) {
          pred_hi <- plan$baseline_hi
          pred_lo <- plan$baseline_lo
        } else {
          eta_hi <- drop(X_hi %*% beta)
          eta_lo <- drop(X_lo %*% beta)
          pred_hi <- if (isTRUE(response_scale)) family$linkinv(eta_hi) else eta_hi
          pred_lo <- if (isTRUE(response_scale)) family$linkinv(eta_lo) else eta_lo
        }
        if (isTRUE(response_scale)) {
          d_hi <- as.vector(family$mu.eta(eta_hi))
          d_lo <- as.vector(family$mu.eta(eta_lo))
        } else {
          d_hi <- NULL
          d_lo <- NULL
        }
        # Validate predictions on the effective scale before transforming the
        # derivative matrix with the recorded comparison operations.
        replay <- comparison_plan_apply(plan, pred_hi, pred_lo)
      } else {
        X <- align_matrix(attr(
          plan$predict_args$newdata,
          "marginaleffects_model_matrix"
        ))
        if (is.null(X)) {
          return(NULL)
        }
        reuse <- isTRUE(plan$model_matrix_used) &&
          identical(colnames(X), beta_names) &&
          is.numeric(plan$baseline_prediction) &&
          length(plan$baseline_prediction) == nrow(X)
        eta_reuse <- reuse &&
          is.numeric(plan$linear_predictor) &&
          length(plan$linear_predictor) == nrow(X)
        if (isTRUE(response_scale) && !eta_reuse) {
          eta <- drop(X %*% beta)
        } else if (eta_reuse) {
          eta <- plan$linear_predictor
        }
        if (reuse) {
          pred <- plan$baseline_prediction
        } else {
          eta <- drop(X %*% beta)
          pred <- if (isTRUE(response_scale)) family$linkinv(eta) else eta
        }
        if (isTRUE(response_scale)) {
          J <- X * as.vector(family$mu.eta(eta))
        } else {
          J <- X
        }
        replay <- prediction_plan_apply(plan, pred)
      }

      # This is a fail-closed correctness guard, not a debugging assertion. It
      # rejects stale matrices, offsets, prediction arguments, and future
      # semantic changes which are not captured by the static whitelist above.
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
          X_hi <- X_hi[plan$na_keep, , drop = FALSE]
          X_lo <- X_lo[plan$na_keep, , drop = FALSE]
          if (!is.null(d_hi)) d_hi <- d_hi[plan$na_keep]
          if (!is.null(d_lo)) d_lo <- d_lo[plan$na_keep]
        }
        if (!is.null(plan$perm)) {
          X_hi <- X_hi[plan$perm, , drop = FALSE]
          X_lo <- X_lo[plan$perm, , drop = FALSE]
          if (!is.null(d_hi)) d_hi <- d_hi[plan$perm]
          if (!is.null(d_lo)) d_lo <- d_lo[plan$perm]
        }

        direct <- jacobian_analytic_comparison_aggregate(
          X_hi = X_hi,
          X_lo = X_lo,
          d_hi = d_hi,
          d_lo = d_lo,
          plan = plan
        )
        if (is.null(direct)) {
          aggregated_early <- FALSE
          if (isTRUE(response_scale)) {
            J <- X_hi * d_hi - X_lo * d_lo
          } else {
            J <- X_hi - X_lo
          }
        } else {
          J <- direct$J
          aggregated_early <- direct$aggregated
        }
        if (is.null(direct)) {
          J <- jacobian_analytic_comparison_groups(J, plan)
          if (is.null(J)) {
            return(NULL)
          }
          if (!is.null(plan$est_keep)) {
            J <- J[plan$est_keep, , drop = FALSE]
          }
        }
      } else if (!is.null(plan$keep)) {
        J <- J[plan$keep, , drop = FALSE]
      }

      if (!identical(kind, "comparisons") || !isTRUE(aggregated_early)) {
        J <- jacobian_analytic_aggregate(J, plan$agg)
        if (is.null(J)) {
          return(NULL)
        }
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
