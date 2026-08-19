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
# stage which happens to be opaque. When a stage derivative is not known in
# closed form it is probed numerically, but only ever through the stage's own
# arithmetic, never by re-running the model. Every probe which relies on a
# structural assumption is verified against the stage function before it is
# trusted, and an unverified stage returns NULL so that callers fall back to
# differentiating the entire pipeline exactly as before.


# `plan$need_y` is set conservatively: it is TRUE for every custom comparison
# function, because such a function might accept the observed outcome as an
# argument. Whether any of them actually does is recorded group by group, and
# that is the question a Jacobian needs answered.
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


# Probing a stage column by column costs one closure call per estimate, and
# each call is itself linear in the number of estimates. That is trivial next
# to a model evaluation at ordinary sizes and quadratic at extreme ones, so
# very wide stages keep their previous path rather than risk being slower than
# the fallback they replace. Linear hypotheses built directly as matrices, such
# as the `~pairwise` shortcut, never reach a probe and are unaffected.
stage_probe_max_dim <- function() {
    getOption("marginaleffects_stage_probe_max_dim", default = 500L)
}


stage_probe_finite <- function(x) {
    is.numeric(x) && length(x) > 0L && all(is.finite(x))
}


# Deterministic pseudo-random directions. Tests and package results must not
# depend on the state of the user's RNG, so verification directions are
# generated from a fixed low-discrepancy sequence rather than from runif().
stage_probe_direction <- function(n, seed) {
    # Additive recurrence with an irrational increment: equidistributed, cheap,
    # and free of the accidental structure that a repeating pattern would have.
    phi <- 0.61803398874989484820458683436564
    x <- (seed * phi + seq_len(n) * phi * phi) %% 1
    2 * x - 1
}


#' Numeric Jacobian of a cheap map, by central differences
#'
#' Differentiates `f` column by column. Intended only for stages whose input
#' dimension is small (the number of estimates, not the number of rows) and
#' whose evaluation is pure arithmetic.
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


#' Probe the derivative of a comparison-group function
#'
#' A comparison group maps the `hi` and `lo` predictions of its rows to either
#' one value per row or a single aggregated value. Both shapes have structure
#' worth exploiting, because probing 2n inputs one at a time would cost more
#' than the model evaluations this is meant to avoid:
#'
#' * row-wise groups have a diagonal Jacobian, so perturbing every input at
#'   once reads off the whole diagonal in a single pair of evaluations;
#' * aggregated groups which depend on their inputs only through a mean have a
#'   uniform gradient, so the same pair of evaluations determines it.
#'
#' Every perturbation is scaled element by element, so a vector mixing very
#' large and very small predictions never steps any element by more than a
#' small fraction of its own magnitude. The recovered gradient is re-probed at
#' half the step and must agree, which catches magnitude error from curvature
#' or roundoff, and pseudo-random subsets of the inputs are re-probed to check
#' the structural assumption, which fails in the presence of off-diagonal
#' coupling or a non-uniform gradient.
#'
#' No finite set of evaluations can prove structure about arbitrary code, so
#' this is a screen, not a proof: the analytic Jacobian path does not rely on
#' it. It differentiates only recorded built-ins, in closed form.
#'
#' @param fun the comparison function recorded in the plan
#' @param args the recorded arguments, excluding `hi` and `lo`
#' @param hi,lo numeric vectors of predictions for this group
#' @param n_out expected output length: `length(hi)` or 1
#' @return list with numeric `hi` and `lo` gradients, or NULL
#' @keywords internal
#' @noRd
stage_comparison_gradient <- function(fun, args, hi, lo, n_out) {
    if (!is.function(fun) || !stage_probe_finite(hi) || !stage_probe_finite(lo)) {
        return(NULL)
    }
    n <- length(hi)
    if (length(lo) != n || n == 0L) {
        return(NULL)
    }
    rowwise <- identical(n_out, n)
    if (!rowwise && !identical(n_out, 1L)) {
        return(NULL)
    }

    eval_fun <- function(h, l) {
        a <- args
        a$hi <- h
        a$lo <- l
        out <- tryCatch(do_call(fun, a), error = function(e) NULL)
        if (!stage_probe_finite(out) || length(out) != n_out) {
            return(NULL)
        }
        out
    }

    if (is.null(eval_fun(hi, lo))) {
        return(NULL)
    }

    # Element-wise steps: each entry is perturbed relative to its own
    # magnitude, with a small absolute floor so exact zeros still move. A
    # single shared step would let the largest prediction dictate a
    # perturbation bigger than the smallest one, which pushes ratio-type
    # functions across poles and corrupts the recovered gradient.
    elem_step <- function(x) {
        abs(x) * .Machine$double.eps^(1 / 3) + 1e-9
    }
    step_hi <- elem_step(hi)
    step_lo <- elem_step(lo)

    # Central difference along an element-wise masked direction. The response
    # is returned in function units, not divided by any step, so callers
    # compare responses directly.
    probe <- function(which, mask, scale) {
        if (identical(which, "hi")) {
            s <- step_hi * mask * scale
            up <- eval_fun(hi + s, lo)
            dn <- eval_fun(hi - s, lo)
        } else {
            s <- step_lo * mask * scale
            up <- eval_fun(hi, lo + s)
            dn <- eval_fun(hi, lo - s)
        }
        if (is.null(up) || is.null(dn)) {
            return(NULL)
        }
        (up - dn) / 2
    }

    ones <- rep(1, n)
    recover <- function(which, scale) {
        r <- probe(which, ones, scale)
        if (is.null(r)) {
            return(NULL)
        }
        step <- if (identical(which, "hi")) step_hi else step_lo
        if (rowwise) {
            # Diagonal Jacobian: each response entry is its own gradient times
            # its own step.
            r / (step * scale)
        } else {
            # Uniform gradient: the scalar response is the gradient times the
            # sum of the steps.
            rep(r / sum(step * scale), n)
        }
    }

    grad_hi <- recover("hi", 1)
    grad_lo <- recover("lo", 1)
    if (!stage_probe_finite(grad_hi) || !stage_probe_finite(grad_lo)) {
        return(NULL)
    }

    # Magnitude check: the same recovery at half the step must agree. A
    # central difference converges as the square of the step, so agreement
    # here means the step is inside the function's linear regime; the shared
    # failure mode of a too-large step -- stepping across a pole or into
    # curvature -- moves the answer between scales and is caught.
    for (which in c("hi", "lo")) {
        g1 <- if (identical(which, "hi")) grad_hi else grad_lo
        g2 <- recover(which, 0.5)
        if (!stage_probe_finite(g2)) {
            return(NULL)
        }
        tol <- 1e-5 * pmax(abs(g1), max(abs(g1)) * 1e-3, .Machine$double.eps)
        if (any(abs(g1 - g2) > tol)) {
            return(NULL)
        }
    }

    # Structural check against pseudo-random subsets. Under the assumed
    # structure the response to a masked perturbation is fully determined by
    # the recovered gradient, so a mismatch means the structure does not hold.
    # Three subsets of differing density: a uniform gradient is only claimed,
    # never derived, so a non-uniform one which happened to sum correctly on a
    # single subset would otherwise be accepted.
    thresholds <- c(0, 0.4, -0.4)
    for (seed in seq_along(thresholds)) {
        mask <- as.numeric(stage_probe_direction(n, seed) > thresholds[[seed]])
        if (all(mask == 0) || all(mask == 1)) {
            mask <- as.numeric(seq_len(n) %% 2L == 0L)
        }
        if (all(mask == 0) || all(mask == 1)) {
            next
        }
        for (which in c("hi", "lo")) {
            got <- probe(which, mask, 1)
            if (is.null(got)) {
                return(NULL)
            }
            step <- if (identical(which, "hi")) step_hi else step_lo
            grad <- if (identical(which, "hi")) grad_hi else grad_lo
            want <- if (rowwise) grad * step * mask else sum(grad * step * mask)
            # Responses are compared in function units, row by row. The
            # tolerance reflects central-difference truncation on a cheap
            # arithmetic map, with a floor for rows whose exact response is
            # zero because the mask does not touch them.
            tol <- 1e-5 * pmax(abs(want), max(abs(grad) * step) * 1e-3)
            if (any(abs(got - want) > tol)) {
                return(NULL)
            }
        }
    }

    list(hi = grad_hi, lo = grad_lo)
}
