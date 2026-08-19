# Stage-by-stage Jacobian composition.
#
# Every estimand marginaleffects computes is a pipeline:
#
#   beta -> eta = X beta -> predictions -> comparison -> aggregation -> hypothesis
#
# The Jacobian of the pipeline is the product of the stage Jacobians. The
# expensive link is the first one, because it is the only stage which touches
# the model and the full data. Every later stage is cheap arithmetic on numbers
# which have already been computed.
#
# Differentiating the whole composition at once forces one model evaluation per
# coefficient, and it also destroys accuracy: a stage which averages n
# predictions carries roundoff of order n * .Machine$double.eps, and dividing
# that by a coefficient-scaled finite-difference step amplifies it by many
# orders of magnitude (#1750).
#
# Composing instead lets an exact first-stage Jacobian survive a downstream
# hypothesis stage which happens to be opaque. Such a hypothesis may be
# differentiated numerically through its own arithmetic, never by re-running
# the model, and the resulting method is labeled accordingly. Arbitrary custom
# comparison functions are not probed: without an explicit trusted derivative
# they fall back to differentiating the full pipeline.


# `plan$need_y` is set conservatively: it is TRUE for every custom comparison
# function, because such a function might accept the observed outcome as an
# argument. Whether any of them actually does is recorded group by group.
plan_groups_use_y <- function(plan) {
    if (!isTRUE(plan$need_y)) {
        return(FALSE)
    }
    if (is.null(plan$groups) || length(plan$groups) == 0L) {
        return(TRUE)
    }
    any(vapply(plan$groups, function(g) isTRUE(g$uses_y), logical(1)))
}


# Central-difference step. Scaled to the value being perturbed so that the step
# is meaningful for both tiny and large estimates, and eps^(1/3) because that
# balances truncation against roundoff for a central difference.
stage_probe_step <- function(x) {
    pmax(abs(x), 1) * .Machine$double.eps^(1 / 3)
}


# Probing a hypothesis stage column by column costs one call per estimate and
# each call is itself linear in the number of estimates. That is trivial next
# to a model evaluation at ordinary sizes and quadratic at extreme ones, so
# very wide stages keep their previous path.
stage_probe_max_dim <- function() {
    getOption("marginaleffects_stage_probe_max_dim", default = 500L)
}


stage_probe_finite <- function(x) {
    is.numeric(x) && length(x) > 0L && all(is.finite(x))
}


#' Numeric Jacobian of a cheap hypothesis map, by central differences
#'
#' Differentiates `f` column by column. This is only used for downstream
#' hypothesis arithmetic. It is never used to infer that arbitrary comparison
#' code is analytic or structurally linear.
#'
#' @param f function taking a numeric vector and returning a numeric vector
#' @param x numeric vector, the point at which to differentiate
#' @param n_out expected length of `f(x)`, or NULL to infer
#' @return a `n_out x length(x)` matrix, or NULL when the probe is unusable
#' @keywords internal
#' @noRd
stage_jacobian_dense <- function(f, x, n_out = NULL) {
    if (!stage_probe_finite(x)) {
        return(NULL)
    }
    if (length(x) > stage_probe_max_dim()) {
        return(NULL)
    }
    base <- tryCatch(f(x), error = function(e) NULL)
    if (!stage_probe_finite(base)) {
        return(NULL)
    }
    if (is.null(n_out)) {
        n_out <- length(base)
    } else if (length(base) != n_out) {
        return(NULL)
    }

    step <- stage_probe_step(x)
    out <- matrix(NA_real_, nrow = n_out, ncol = length(x))
    for (j in seq_along(x)) {
        hi <- lo <- x
        hi[j] <- hi[j] + step[j]
        lo[j] <- lo[j] - step[j]
        fhi <- tryCatch(f(hi), error = function(e) NULL)
        flo <- tryCatch(f(lo), error = function(e) NULL)
        if (
            !stage_probe_finite(fhi) || !stage_probe_finite(flo) ||
                length(fhi) != n_out || length(flo) != n_out
        ) {
            return(NULL)
        }
        out[, j] <- (fhi - flo) / (2 * step[j])
    }
    if (!all(is.finite(out))) {
        return(NULL)
    }
    out
}
