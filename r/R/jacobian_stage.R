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


#' Recover the matrix of a linear hypothesis stage
#'
#' A hypothesis which happens to be linear in the estimates can be represented
#' exactly by a matrix, which is both faster and more accurate than any
#' numerical probe, and which the autodiff lowering rules can consume. This
#' recovers that matrix from an opaque closure and then *proves* linearity
#' before returning it: a wrong verdict here would silently corrupt standard
#' errors, so the check uses full-size directions rather than a Taylor
#' argument, and any disagreement rejects the promotion.
#'
#' Only strictly linear maps are promoted. An affine map such as `"b1 - 0.5"`
#' has a well-defined derivative but cannot be written as `crossprod(H, est)`,
#' so promoting it would change the estimates.
#'
#' @param apply function mapping the vector of estimates to the tested quantities
#' @param estimate numeric vector of estimates at which to probe
#' @return an `length(estimate) x n_out` matrix, or NULL when not linear
#' @keywords internal
#' @noRd
stage_hypothesis_matrix <- function(apply, estimate) {
    if (!is.function(apply) || !stage_probe_finite(estimate)) {
        return(NULL)
    }
    m <- length(estimate)
    if (m == 0L || m > stage_probe_max_dim()) {
        return(NULL)
    }

    base <- tryCatch(apply(estimate), error = function(e) NULL)
    if (!stage_probe_finite(base)) {
        return(NULL)
    }
    p <- length(base)

    # A strictly linear map sends zero to zero. Testing this first also rejects
    # affine maps cheaply, before the more expensive column probes.
    zero <- tryCatch(apply(rep(0, m)), error = function(e) NULL)
    if (!stage_probe_finite(zero) || length(zero) != p) {
        return(NULL)
    }
    if (max(abs(zero)) > 1e-12) {
        return(NULL)
    }

    # For a linear map the columns are exactly the images of the basis vectors.
    G <- matrix(NA_real_, nrow = p, ncol = m)
    for (j in seq_len(m)) {
        e <- rep(0, m)
        e[j] <- 1
        col <- tryCatch(apply(e), error = function(e) NULL)
        if (!stage_probe_finite(col) || length(col) != p) {
            return(NULL)
        }
        G[, j] <- col
    }

    # Verification. Linearity is an exact algebraic claim, so it is checked
    # with directions of ordinary magnitude rather than with small steps: a
    # nonlinear map disagrees at first order, not merely in the remainder.
    scale <- max(1, max(abs(estimate)), max(abs(base)))
    for (seed in seq_len(4L)) {
        d <- stage_probe_direction(m, seed) * scale
        got <- tryCatch(apply(d), error = function(e) NULL)
        if (!stage_probe_finite(got) || length(got) != p) {
            return(NULL)
        }
        want <- drop(G %*% d)
        if (max(abs(got - want)) > 1e-9 * max(1, max(abs(want)))) {
            return(NULL)
        }
    }

    # Also confirm the promotion reproduces the estimates it will replace.
    want <- drop(G %*% estimate)
    if (max(abs(want - base)) > 1e-9 * max(1, max(abs(base)))) {
        return(NULL)
    }

    # The package convention is `crossprod(H, estimate)`, so H is m x p.
    t(G)
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
#'   constant gradient, so the same pair of evaluations determines it.
#'
#' Neither structure is assumed. Both are verified by perturbing pseudo-random
#' subsets of the inputs and checking that the response matches what the
#' assumed structure predicts, which fails immediately in the presence of
#' off-diagonal coupling or a non-uniform gradient.
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

    base <- eval_fun(hi, lo)
    if (is.null(base)) {
        return(NULL)
    }

    # A single step for the whole vector keeps the perturbation uniform, which
    # is what makes the subset verification below a clean structural test.
    step_hi <- max(stage_probe_step(hi))
    step_lo <- max(stage_probe_step(lo))

    probe <- function(which, mask, step) {
        h <- hi
        l <- lo
        if (identical(which, "hi")) {
            h <- hi + step * mask
        } else {
            l <- lo + step * mask
        }
        up <- eval_fun(h, l)
        h <- hi
        l <- lo
        if (identical(which, "hi")) {
            h <- hi - step * mask
        } else {
            l <- lo - step * mask
        }
        dn <- eval_fun(h, l)
        if (is.null(up) || is.null(dn)) {
            return(NULL)
        }
        (up - dn) / (2 * step)
    }

    ones <- rep(1, n)
    d_hi <- probe("hi", ones, step_hi)
    d_lo <- probe("lo", ones, step_lo)
    if (is.null(d_hi) || is.null(d_lo)) {
        return(NULL)
    }

    if (rowwise) {
        # Diagonal Jacobian: the full-vector probe already is the diagonal.
        grad_hi <- d_hi
        grad_lo <- d_lo
    } else {
        # Uniform gradient: the full-vector probe is the sum of n equal terms.
        grad_hi <- rep(d_hi / n, n)
        grad_lo <- rep(d_lo / n, n)
    }
    if (!stage_probe_finite(grad_hi) || !stage_probe_finite(grad_lo)) {
        return(NULL)
    }

    # Verification against pseudo-random subsets. Under the assumed structure
    # the response to a masked perturbation is fully determined by the gradient
    # already recovered, so a mismatch means the structure does not hold.
    tol_scale <- max(1, max(abs(grad_hi)), max(abs(grad_lo)))
    # Three subsets of differing density. A uniform gradient is only claimed,
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
            step <- if (identical(which, "hi")) step_hi else step_lo
            got <- probe(which, mask, step)
            if (is.null(got)) {
                return(NULL)
            }
            grad <- if (identical(which, "hi")) grad_hi else grad_lo
            want <- if (rowwise) grad * mask else sum(grad * mask)
            # The tolerance reflects central-difference truncation error on a
            # cheap arithmetic map, not the accuracy of the final Jacobian.
            if (max(abs(got - want)) > 1e-5 * tol_scale) {
                return(NULL)
            }
        }
    }

    list(hi = grad_hi, lo = grad_lo)
}
