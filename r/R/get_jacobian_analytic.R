# Finiteness of a whole matrix, in one reduction rather than one comparison per
# element. Any NA, NaN, or Inf makes the total non-finite, and no combination of
# them cancels back to a finite value: +Inf and -Inf together give NaN. The one
# false verdict this can return is on a matrix whose finite entries overflow
# when summed, which rejects a matrix the elementwise scan would have accepted
# and so fails in the safe direction. Integers are left to the ordinary scan,
# because summing them overflows to NA with a warning.
matrix_all_finite <- function(x) {
  if (!is.double(x)) {
    return(all(is.finite(x)))
  }
  is.finite(sum(x))
}


# Predictions and inverse-link derivative for one cached model matrix.
#
# Reuses the plan's cached predictions and linear predictor when they are
# trustworthy -- the pipeline's predictions actually came through this matrix
# and the cached vectors have the right shape -- and recomputes from X and
# beta otherwise. The linear predictor is computed at most once, whether it
# is wanted for the inverse-link derivative, for the predictions, or both.
# `family` may be NULL on the link scale, where `d` is NULL as well.
jacobian_analytic_scale <- function(
  X,
  beta,
  cached_pred,
  cached_eta,
  model_matrix_used,
  response_scale,
  family
) {
  reuse <- isTRUE(model_matrix_used) &&
    identical(colnames(X), names(beta)) &&
    is.numeric(cached_pred) && length(cached_pred) == nrow(X)
  eta_reuse <- reuse &&
    is.numeric(cached_eta) && length(cached_eta) == nrow(X)
  eta <- NULL
  if (eta_reuse) {
    eta <- cached_eta
  } else if (isTRUE(response_scale) || !reuse) {
    eta <- drop(X %*% beta)
  }
  pred <- if (reuse) {
    cached_pred
  } else if (isTRUE(response_scale)) {
    family$linkinv(eta)
  } else {
    eta
  }
  d <- if (isTRUE(response_scale)) as.vector(family$mu.eta(eta)) else NULL
  list(pred = pred, d = d)
}


# The exact derivatives of the built-in comparison functions live next to the
# forward definitions they mirror, in `comparison_gradient_exact()` in
# R/sanitize_comparison.R, so that neither can be edited without the other in
# view.


# Comparison groups whose function is not one of the recorded differences.
#
# `X_hi` and `X_lo` are the model matrices of the hi and lo rows, and `d_hi`
# and `d_lo` the inverse-link derivatives at those rows, or NULL on the link
# scale. Their product is the exact derivative of the predictions with respect
# to the coefficients. The comparison stage applies a recorded built-in whose
# derivative is known in closed form, so the stage gradients compose exactly:
# d(comparison)/d(beta) = dc/d(hi) * d(hi)/d(beta) + dc/d(lo) * d(lo)/d(beta).
#
# The inverse-link derivative is folded into the stage gradient before either
# touches a model matrix, because that product is a vector and the matrix is
# not. A group which contracts its rows to a single value then never needs the
# observation-level derivative at all: its row is a pair of weighted column
# sums, which `crossprod()` forms without allocating an n x p matrix that the
# contraction would immediately discard.
#
# Returns NULL whenever any group's function is not a recorded built-in or its
# gradient is not finite, which keeps the whole estimand on its previous path
# rather than mixing methods.
jacobian_analytic_comparison_exact <- function(
  X_hi,
  X_lo,
  d_hi,
  d_lo,
  pred_hi,
  pred_lo,
  plan
) {
  out <- matrix(NA_real_, nrow = plan$n_comp, ncol = ncol(X_hi))

  for (g in plan$groups) {
    key <- g$fun_key
    # A custom closure records fun_key = NA. It must land on the numeric
    # fallback: arbitrary code has no recorded closed form, and probing it
    # cannot prove one.
    if (
      isTRUE(g$uses_y) || !is.character(key) || length(key) != 1L || is.na(key)
    ) {
      return(NULL)
    }
    idx <- g$idx
    n_out <- length(g$out_idx)
    grad <- comparison_gradient_exact(
      fun_key = key,
      hi = pred_hi[idx],
      lo = pred_lo[idx],
      args = g$args
    )
    if (
      is.null(grad) ||
        !stage_probe_finite(grad$hi) || !stage_probe_finite(grad$lo) ||
        length(grad$hi) != length(idx) || length(grad$lo) != length(idx)
    ) {
      return(NULL)
    }

    w_hi <- if (is.null(d_hi)) grad$hi else grad$hi * d_hi[idx]
    w_lo <- if (is.null(d_lo)) grad$lo else grad$lo * d_lo[idx]

    if (isTRUE(g$scalar)) {
      if (n_out != 1L) {
        return(NULL)
      }
      value <- jacobian_analytic_weighted_columns(X_hi, idx, w_hi) +
        jacobian_analytic_weighted_columns(X_lo, idx, w_lo)
    } else {
      if (n_out != length(idx)) {
        return(NULL)
      }
      value <- X_hi[idx, , drop = FALSE] * w_hi +
        X_lo[idx, , drop = FALSE] * w_lo
    }
    out[g$out_idx, ] <- value
  }

  out
}


# The weights of a recorded aggregation, as the entries of a sparse matrix.
#
# An aggregation is a linear map, out = t(W) %*% M. Written densely W is
# n x agg$n and the product costs n * agg$n * p. But the plan records exactly
# one (source row, output row) pair per source row, so W has only n entries and
# the same product costs a single pass over M. Collecting the entries from
# every block at once also avoids aggregating group by group, which would
# subset M once per group and so copy the whole matrix across the loop --
# several times the cost of the arithmetic it feeds.
#
# Returns NULL when the recorded aggregation is not of the expected shape.
jacobian_analytic_aggregate_weights <- function(agg, n_rows) {
  if (is.null(agg) || length(agg$blocks) == 0L) {
    return(NULL)
  }
  n_entries <- sum(vapply(agg$blocks, function(b) length(b$idx), integer(1)))
  if (!is.finite(n_entries) || n_entries == 0L) {
    return(NULL)
  }

  rows <- integer(n_entries)
  cols <- integer(n_entries)
  vals <- numeric(n_entries)
  at <- 0L

  for (block in agg$blocks) {
    idx <- block$idx
    if (
      !is.matrix(idx) || nrow(idx) == 0L ||
        any(idx < 1L | idx > n_rows)
    ) {
      return(NULL)
    }
    if (length(block$cols) != ncol(idx)) {
      return(NULL)
    }

    if (isTRUE(agg$weighted)) {
      w <- block$w
      if (
        !is.numeric(w) || !identical(dim(w), dim(idx)) ||
          any(!is.finite(w))
      ) {
        return(NULL)
      }
      denominator <- colSums(w)
      if (any(!is.finite(denominator)) || any(denominator == 0)) {
        return(NULL)
      }
      # `idx` is column-major, so each group's entries are contiguous and the
      # per-group denominators line up by repetition.
      wts <- as.vector(w) / rep(denominator, each = nrow(idx))
    } else {
      wts <- rep.int(1 / nrow(idx), length(idx))
    }

    span <- at + seq_len(length(idx))
    rows[span] <- as.vector(idx)
    cols[span] <- rep(block$cols, each = nrow(idx))
    vals[span] <- wts
    at <- at + length(idx)
  }

  list(rows = rows, cols = cols, vals = vals)
}


# Contract a matrix by a recorded aggregation. A single output row -- an
# unstratified average, the common case -- is one weighted column sum, which
# crossprod() forms without building any matrix at all.
jacobian_analytic_aggregate_contract <- function(M, agg, rows, cols, vals) {
  if (identical(as.integer(agg$n), 1L)) {
    return(matrix(jacobian_analytic_weighted_columns(M, rows, vals), nrow = 1L))
  }
  W <- Matrix::sparseMatrix(
    i = rows,
    j = cols,
    x = vals,
    dims = c(nrow(M), agg$n)
  )
  as.matrix(Matrix::crossprod(W, M))
}


# Aggregated prediction Jacobians, straight from the model matrix.
#
# The observation-level Jacobian of a prediction is X scaled row-wise by the
# inverse-link derivative, and an aggregating estimand immediately contracts
# those rows away. Forming the n x p product first is wasted work in both
# directions: it allocates a matrix the size of X only to reduce it to a
# handful of rows. Folding the row scaling into the aggregation weights -- a
# vector -- lets the contraction read straight off X instead.
#
# `d` is the inverse-link derivative, or NULL on the link scale.
jacobian_analytic_aggregate_direct <- function(X, d, agg) {
  weights <- jacobian_analytic_aggregate_weights(agg, nrow(X))
  if (is.null(weights)) {
    return(NULL)
  }
  vals <- if (is.null(d)) {
    weights$vals
  } else {
    weights$vals * d[weights$rows]
  }
  if (!all(is.finite(vals))) {
    return(NULL)
  }
  jacobian_analytic_aggregate_contract(
    X, agg, weights$rows, weights$cols, vals
  )
}


jacobian_analytic_aggregate <- function(J, agg) {
  if (is.null(agg)) {
    return(J)
  }
  weights <- jacobian_analytic_aggregate_weights(agg, nrow(J))
  if (is.null(weights)) {
    return(NULL)
  }
  jacobian_analytic_aggregate_contract(
    J, agg, weights$rows, weights$cols, weights$vals
  )
}


jacobian_analytic_hypothesis <- function(J, hyp, estimate_pre = NULL) {
  if (is.null(hyp)) {
    return(J)
  }

  res <- hypothesis_stage_pullback(hyp, J, at = estimate_pre)
  if (is.null(res)) {
    return(NULL)
  }
  out <- res$jacobian
  # Only a probed stage costs the result its analytic provenance.
  if (!isTRUE(res$exact)) {
    attr(out, "marginaleffects_numeric_stage") <- TRUE
  }
  out
}


jacobian_analytic_weighted_columns <- function(X, idx, w) {
  if (identical(idx, seq_len(nrow(X)))) {
    return(drop(crossprod(w, X)))
  }
  drop(crossprod(w, X[idx, , drop = FALSE]))
}


# Rowwise simple differences with a recorded aggregation, composed directly
# from cached matrices: the comparison-row mapping folds into the aggregation
# weights, so the intermediate n-observation Jacobian of avg_comparisons() is
# never allocated. Scalar-aggregating difference keys do not come here; their
# per-group gradients in jacobian_analytic_comparison_exact() contract with
# crossprod() and skip the observation-level matrix just the same.
jacobian_analytic_comparison_aggregate <- function(
  X_hi,
  X_lo,
  d_hi,
  d_lo,
  plan
) {
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

  # The aggregation weights are recorded against comparison rows, so map them
  # through to the model matrix rows those comparisons were built from and
  # contract hi and lo separately. Group by group this would subset X_hi and
  # X_lo once each per group, copying both matrices in full across the loop;
  # as a single sparse contraction it is one pass over each.
  weights <- jacobian_analytic_aggregate_weights(plan$agg, length(raw_index))
  if (is.null(weights)) {
    return(NULL)
  }
  rows <- raw_index[weights$rows]
  vals_hi <- if (is.null(d_hi)) weights$vals else weights$vals * d_hi[rows]
  vals_lo <- if (is.null(d_lo)) weights$vals else weights$vals * d_lo[rows]
  if (!all(is.finite(vals_hi)) || !all(is.finite(vals_lo))) {
    return(NULL)
  }
  jacobian_analytic_aggregate_contract(
    X_hi, plan$agg, rows, weights$cols, vals_hi
  ) -
    jacobian_analytic_aggregate_contract(
      X_lo, plan$agg, rows, weights$cols, vals_lo
    )
}


#' How this model's predictions depend on its coefficients
#'
#' The whole model-specific half of the analytic Jacobian contract. A method
#' answers three questions and nothing else: is this exact class eligible,
#' does this prediction `type` live on the link scale or the response scale,
#' and -- on the response scale -- which family supplies `linkinv()` and
#' `mu.eta()`. Everything downstream of that (matrix alignment, comparison
#' gradients, aggregation, hypothesis pullback, replay validation) is
#' model-agnostic and belongs to `compute_analytic_plan_jacobian()`.
#'
#' The derivative of the predictions with respect to the coefficients is the
#' model matrix `X` on the link scale, and `X` scaled row-wise by
#' `family$mu.eta(eta)` on the response scale. A spec is exactly the
#' information needed to say which of the two applies.
#'
#' @param model A model object.
#' @param type The prediction type requested by the estimand.
#' @param ... Unused; present for method extension.
#' @return `NULL` when the model or the requested `type` is not eligible,
#'   otherwise a list with `response_scale` (a scalar logical) and `family`
#'   (a family object on the response scale, `NULL` on the link scale).
#' @keywords internal
#' @noRd
get_prediction_jacobian_spec <- function(model, type, ...) {
  UseMethod("get_prediction_jacobian_spec", model)
}


#' @noRd
#' @export
get_prediction_jacobian_spec.default <- function(model, type, ...) {
  NULL
}


# Shared bodies for the per-model get_prediction_jacobian_spec() methods. Each
# method gates on its exact class -- subclasses may override prediction
# behavior the eligibility whitelist knows nothing about -- and on the
# prediction types whose scale it can differentiate.
prediction_jacobian_spec_linear <- function(model, class_expected, type, types_ok) {
  if (
    !identical(class(model)[1], class_expected) ||
      !isTRUE(type %in% types_ok)
  ) {
    return(NULL)
  }
  list(response_scale = FALSE, family = NULL)
}


prediction_jacobian_spec_glm_family <- function(
  model,
  class_expected,
  type,
  response_type = "response",
  link_type = "link",
  family = NULL
) {
  if (
    !identical(class(model)[1], class_expected) ||
      !isTRUE(type %in% c(response_type, link_type))
  ) {
    return(NULL)
  }
  if (!identical(type, response_type)) {
    return(list(response_scale = FALSE, family = NULL))
  }
  if (is.null(family)) {
    family <- stats::family(model)
  }
  list(response_scale = TRUE, family = family)
}


#' Get an exact analytic Jacobian when supported
#' @param model A model object.
#' @param type The prediction type requested by the estimand.
#' @param ... Arguments passed on to the composer.
#' @return A numeric Jacobian matrix, or `NULL` when the model or estimand is
#'   not eligible for the analytic path.
#' @keywords internal
#' @noRd
get_jacobian_analytic <- function(model, type, ...) {
  UseMethod("get_jacobian_analytic", model)
}


# The entry point is model-agnostic: it asks the model how its predictions
# depend on its coefficients, and hands that answer to the composer. The
# generic is kept so that a model class which needs to bypass the spec
# contract entirely still can, but no method in this package does.
#' @noRd
#' @export
get_jacobian_analytic.default <- function(model, type, ...) {
  spec <- get_prediction_jacobian_spec(model, type = type)
  if (is.null(spec)) {
    return(NULL)
  }
  compute_analytic_plan_jacobian(spec = spec, model = model, type = type, ...)
}


# Model-agnostic composition of a prediction-derivative spec with an estimand
# plan. `spec` is whatever get_prediction_jacobian_spec() returned; nothing
# else here knows anything about the model class.
compute_analytic_plan_jacobian <- function(
  spec,
  model,
  plan,
  kind,
  type,
  estimate,
  contrast_data = NULL
) {
  response_scale <- isTRUE(spec$response_scale)
  family <- if (response_scale) spec$family else NULL
  # NULL means that this estimand is not safely eligible. This is an expected
  # result which preserves the existing finite-difference path.
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

      # A non-matrix hypothesis no longer disqualifies the estimand: it is a
      # separate stage, composed after the fact by jacobian_analytic_hypothesis().
      if (
        !kind %in% c("comparisons", "predictions") ||
          !identical(plan$kind, kind)
      ) {
        return(NULL)
      }

      align_matrix <- function(X) {
        if (
          !isTRUE(checkmate::check_matrix(X, mode = "numeric")) ||
            ncol(X) != length(beta) || !matrix_all_finite(X)
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
        if (is.null(contrast_data) || length(plan$groups) == 0L) {
          return(NULL)
        }
        # `plan$need_y` is set for every custom comparison function, because a
        # user function might accept `y`. Whether it actually does is recorded
        # per group, and that is the condition which matters here: a group
        # which ignores the observed outcome has a derivative that does not
        # depend on it.
        if (plan_groups_use_y(plan)) {
          return(NULL)
        }
        # Recorded differences have closed-form derivatives. Any other
        # comparison function is handled as its own stage further down.
        difference_groups <- all(vapply(plan$groups, function(g) {
          isTRUE(g$fun_key %in% c("difference", "differenceavg", "differenceavgwts"))
        }, logical(1)))
        # A group whose function has no recorded closed form -- a custom
        # closure, or any key outside the derivative registry -- can only end
        # in rejection, so reject it here, before the model matrices are
        # scanned and aligned. This is the statically obvious case; every
        # data-dependent rejection stays where the data is.
        if (!difference_groups) {
          supported <- vapply(plan$groups, function(g) {
            key <- g$fun_key
            is.character(key) && length(key) == 1L && !is.na(key)
          }, logical(1))
          if (!all(supported)) {
            return(NULL)
          }
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

        scale_hi <- jacobian_analytic_scale(
          X_hi,
          beta,
          plan$baseline_hi,
          plan$eta_hi,
          plan$model_matrix_used,
          response_scale,
          family
        )
        scale_lo <- jacobian_analytic_scale(
          X_lo,
          beta,
          plan$baseline_lo,
          plan$eta_lo,
          plan$model_matrix_used,
          response_scale,
          family
        )
        pred_hi <- scale_hi$pred
        pred_lo <- scale_lo$pred
        d_hi <- scale_hi$d
        d_lo <- scale_lo$d
        # The replay stages tolerate missing predictions (their aggregation
        # averages with na.rm = TRUE), but the sparse Jacobian aggregation
        # divides by full group counts. Those two conventions agree only when
        # nothing is missing, so missing predictions disqualify the analytic
        # path outright.
        if (anyNA(pred_hi) || anyNA(pred_lo)) {
          return(NULL)
        }
        # Validate predictions on the effective scale before transforming the
        # derivative matrix with the recorded comparison operations.
        replay <- comparison_plan_apply_stages(plan, pred_hi, pred_lo)
      } else {
        X <- align_matrix(attr(
          plan$predict_args$newdata,
          "marginaleffects_model_matrix"
        ))
        if (is.null(X)) {
          return(NULL)
        }
        # The row scaling stays a vector for now. Whether it ever needs to
        # meet X as a full matrix depends on the aggregation, decided below.
        scale_pred <- jacobian_analytic_scale(
          X,
          beta,
          plan$baseline_prediction,
          plan$linear_predictor,
          plan$model_matrix_used,
          response_scale,
          family
        )
        pred <- scale_pred$pred
        d_pred <- scale_pred$d
        # Same rejection as the comparisons branch: replay aggregation drops
        # missing predictions, the sparse weights do not.
        if (isTRUE(plan$has_na) || anyNA(pred)) {
          return(NULL)
        }
        replay <- prediction_plan_apply_stages(plan, pred)
      }

      # This is a fail-closed correctness guard, not a debugging assertion. It
      # rejects stale matrices, offsets, prediction arguments, and future
      # semantic changes which are not captured by the static whitelist above.
      # The comparison is element-wise: a mean-relative check would let one
      # wrong row hide among many correct ones.
      if (!plan_replay_agrees(replay$post, estimate)) {
        return(NULL)
      }

      # Set by whichever branch below folds the recorded aggregation into the
      # Jacobian it builds, so that it is not applied a second time.
      aggregated_early <- FALSE

      if (identical(kind, "comparisons")) {
        if (!is.null(plan$na_keep)) {
          X_hi <- X_hi[plan$na_keep, , drop = FALSE]
          X_lo <- X_lo[plan$na_keep, , drop = FALSE]
          if (!is.null(d_hi)) d_hi <- d_hi[plan$na_keep]
          if (!is.null(d_lo)) d_lo <- d_lo[plan$na_keep]
          pred_hi <- pred_hi[plan$na_keep]
          pred_lo <- pred_lo[plan$na_keep]
        }
        if (!is.null(plan$perm)) {
          X_hi <- X_hi[plan$perm, , drop = FALSE]
          X_lo <- X_lo[plan$perm, , drop = FALSE]
          if (!is.null(d_hi)) d_hi <- d_hi[plan$perm]
          if (!is.null(d_lo)) d_lo <- d_lo[plan$perm]
          pred_hi <- pred_hi[plan$perm]
          pred_lo <- pred_lo[plan$perm]
        }

        # Rowwise differences feeding a recorded aggregation fold straight
        # into the aggregation weights and come back already contracted.
        # Every other group shape goes through the exact per-group gradients:
        # they hand the stage its model matrices and inverse-link derivatives
        # separately rather than the pair of prediction Jacobians, so a group
        # which aggregates its rows composes them without ever forming an
        # observation-level derivative either.
        direct <- if (isTRUE(difference_groups)) {
          jacobian_analytic_comparison_aggregate(
            X_hi = X_hi,
            X_lo = X_lo,
            d_hi = d_hi,
            d_lo = d_lo,
            plan = plan
          )
        } else {
          NULL
        }
        if (!is.null(direct)) {
          J <- direct
          aggregated_early <- TRUE
        } else {
          J <- jacobian_analytic_comparison_exact(
            X_hi = X_hi,
            X_lo = X_lo,
            d_hi = d_hi,
            d_lo = d_lo,
            pred_hi = pred_hi,
            pred_lo = pred_lo,
            plan = plan
          )
          if (is.null(J)) {
            return(NULL)
          }
          if (!is.null(plan$est_keep)) {
            J <- J[plan$est_keep, , drop = FALSE]
          }
          aggregated_early <- FALSE
        }
      } else {
        # An aggregating estimand contracts the prediction rows away, so the
        # observation-level Jacobian is an intermediate the answer never needs.
        # Row subsetting would renumber the recorded aggregation indices, so
        # the shortcut is taken only when there is nothing to subset.
        direct <- if (is.null(plan$keep)) {
          jacobian_analytic_aggregate_direct(X, d_pred, plan$agg)
        } else {
          NULL
        }
        if (is.null(direct)) {
          J <- if (is.null(d_pred)) X else X * d_pred
          if (!is.null(plan$keep)) {
            J <- J[plan$keep, , drop = FALSE]
          }
        } else {
          J <- direct
          aggregated_early <- TRUE
        }
      }

      if (!isTRUE(aggregated_early)) {
        J <- jacobian_analytic_aggregate(J, plan$agg)
        if (is.null(J)) {
          return(NULL)
        }
      }
      J <- jacobian_analytic_hypothesis(J, plan$hyp, replay$pre)
      if (is.null(J)) {
        return(NULL)
      }

      if (nrow(J) != length(estimate)) {
        return(NULL)
      }
      # Final guard on the output. The inputs were already screened by
      # align_matrix(), so this is a backstop for non-finite values produced
      # by the composition itself (an inverse-link derivative overflowing, a
      # gradient at a pole). It is a backstop and not the sole guard because
      # IEEE propagation is a property of IEEE arithmetic, not of every BLAS
      # kernel: implementations which skip zero multiplicands can turn
      # 0 * Inf into 0 instead of NaN, so an Inf in a zero-weighted input row
      # is not guaranteed to surface here.
      if (!matrix_all_finite(J)) {
        return(NULL)
      }
      # Cached model matrices may carry terms metadata such as `assign` and
      # `contrasts`. A Jacobian's contract includes only dimensions and names.
      numeric_stage <- isTRUE(attr(J, "marginaleffects_numeric_stage"))
      attributes(J) <- list(
        dim = dim(J),
        dimnames = list(NULL, beta_names)
      )
      if (numeric_stage) {
        attr(J, "marginaleffects_numeric_stage") <- TRUE
      }

      J
    },
    error = function(e) NULL
  )
}
