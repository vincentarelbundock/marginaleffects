source("helpers.R")
using("marginaleffects")

# Jacobians are computed by one of two paths: an exact analytic derivative or
# finite differences. This file compares them against each other and, more
# importantly, against oracles which share no code with either.
#
# Two design points matter and are easy to get wrong.
#
# First, setting an option does not mean a path ran. get_jacobian_analytic()
# returns NULL on any unsupported stage and falls through to finite
# differences. A suite which only set options could be entirely green while
# testing the same fallback twice over, which is precisely what a regression
# narrowing the analytic path would produce. Every helper below asserts the
# path which actually produced the Jacobian.
#
# Second, the two paths are not oracles of equal quality. Finite differences
# are the less accurate of them, so they are never used as the reference: they
# are screened loosely, and the absolute scale is pinned by closed-form delta
# method calculations written out by hand.


# ---------------------------------------------------------------------------
# Path forcing, with provenance assertions
# ---------------------------------------------------------------------------

jacobian_path_run <- function(method, f) {
    old_analytic <- getOption("marginaleffects_analytic_jacobian", default = TRUE)
    on.exit(
        options(marginaleffects_analytic_jacobian = old_analytic),
        add = TRUE
    )
    options(marginaleffects_analytic_jacobian = identical(method, "analytic"))
    f()
}

jacobian_method <- function(x) {
    out <- components(x, "jacobian_method")
    if (is.null(out)) NA_character_ else out
}

# Run `f` on a path and fail loudly if some other path answered.
on_path <- function(method, f) {
    out <- jacobian_path_run(method, f)
    expect_equal(jacobian_method(out), method)
    out
}


# ---------------------------------------------------------------------------
# Fixtures
#
# Total binomial information is held fixed while the row count varies, so any
# drift across `N` is numerical rather than statistical.
# ---------------------------------------------------------------------------

two_arm_data <- function(N, total = 2e7) {
    n <- total / (2 * N)
    data.frame(
        trt = factor(rep(c("ctrl", "test"), each = N), levels = c("ctrl", "test")),
        n = n,
        y = c(rep(round(n * 0.50), N), rep(round(n * 0.55), N))
    )
}

two_arm_model <- function(N) {
    glm(cbind(y, n - y) ~ trt, data = two_arm_data(N), family = binomial)
}


# ---------------------------------------------------------------------------
# Oracle 1: risk difference between two arms of a grouped binomial model.
#
# This is the calculation which adjudicated #1750. Neither avg_comparisons()
# nor avg_predictions() could settle which of two disagreeing answers was
# right; only an independent sqrt(g' V g) could.
# ---------------------------------------------------------------------------

oracle_two_arm_difference <- function(model) {
    b <- coef(model)
    V <- vcov(model)
    p0 <- plogis(b[[1]])
    p1 <- plogis(b[[1]] + b[[2]])
    g <- c(p1 * (1 - p1) - p0 * (1 - p0), p1 * (1 - p1))
    sqrt(drop(t(g) %*% V %*% g))
}

mod <- two_arm_model(1000)
truth <- oracle_two_arm_difference(mod)

cmp <- on_path("analytic", function() {
    avg_comparisons(mod, variables = "trt")
})
expect_equal(cmp$std.error, truth, tolerance = 1e-10)

# The same estimand reached through predictions and a hypothesis, on all three
# hypothesis representations. Each must land on the analytic path and agree
# with the oracle, not merely with each other.
H <- matrix(c(-1, 1), ncol = 1, dimnames = list(NULL, "test - ctrl"))
for (hyp in list(H, "b2 - b1 = 0", ~pairwise)) {
    gcomp <- on_path("analytic", function() {
        avg_predictions(mod, by = "trt", hypothesis = hyp)
    })
    expect_equal(gcomp$std.error, truth, tolerance = 1e-10)
}


# ---------------------------------------------------------------------------
# Oracle 2: for a linear model the Jacobian of a contrast is a difference of
# model matrix rows, with no derivative machinery involved at all.
# ---------------------------------------------------------------------------

mod_lm <- lm(mpg ~ hp + wt, data = mtcars)
cmp_lm <- on_path("analytic", function() {
    comparisons(mod_lm, variables = list(hp = c(100, 110)), newdata = "mean")
})
expect_equivalent(
    components(cmp_lm, "jacobian"),
    matrix(c(0, 10, 0), nrow = 1)
)


# ---------------------------------------------------------------------------
# Oracle 3: a ratio of averaged predictions, differentiated by hand.
#
# avg_comparisons(comparison = "ratio") reports mean(hi) / mean(lo), so
#   d/db = mean(mu'(eta_hi) X_hi) / mean(lo)
#          - mean(hi) / mean(lo)^2 * mean(mu'(eta_lo) X_lo)
# ---------------------------------------------------------------------------

set.seed(1024)
n_ratio <- 400
dat_ratio <- data.frame(
    g = factor(sample(letters[1:3], n_ratio, replace = TRUE)),
    x = rnorm(n_ratio)
)
dat_ratio$y <- rbinom(
    n_ratio, 1,
    plogis(0.3 * dat_ratio$x + as.numeric(dat_ratio$g) / 10)
)
mod_ratio <- glm(y ~ g + x, data = dat_ratio, family = binomial)

oracle_ratio <- function(model, data) {
    b <- coef(model)
    V <- vcov(model)
    d_hi <- d_lo <- data
    d_hi$x <- 1
    d_lo$x <- 0
    X_hi <- model.matrix(model, data = d_hi)
    X_lo <- model.matrix(model, data = d_lo)
    eta_hi <- drop(X_hi %*% b)
    eta_lo <- drop(X_lo %*% b)
    p_hi <- plogis(eta_hi)
    p_lo <- plogis(eta_lo)
    m_hi <- mean(p_hi)
    m_lo <- mean(p_lo)
    g_hi <- colMeans(X_hi * (p_hi * (1 - p_hi)))
    g_lo <- colMeans(X_lo * (p_lo * (1 - p_lo)))
    g <- g_hi / m_lo - m_hi / m_lo^2 * g_lo
    list(estimate = m_hi / m_lo, std.error = sqrt(drop(t(g) %*% V %*% g)))
}

want <- oracle_ratio(mod_ratio, dat_ratio)
got <- on_path("analytic", function() {
    avg_comparisons(
        mod_ratio,
        variables = list(x = c(0, 1)),
        comparison = "ratio"
    )
})
expect_equal(got$estimate, want$estimate, tolerance = 1e-10)
expect_equal(got$std.error, want$std.error, tolerance = 1e-8)


# ---------------------------------------------------------------------------
# Oracle 4: a genuinely nonlinear hypothesis, which cannot be promoted to a
# matrix and therefore exercises the probed hypothesis stage.
# ---------------------------------------------------------------------------

oracle_two_arm_exp <- function(model) {
    b <- coef(model)
    V <- vcov(model)
    p0 <- plogis(b[[1]])
    p1 <- plogis(b[[1]] + b[[2]])
    # Jacobian of the two group-average predictions.
    J <- rbind(
        c(p0 * (1 - p0), 0),
        c(p1 * (1 - p1), p1 * (1 - p1))
    )
    # exp(e2 - e1) composes with a gradient of exp(.) * (row2 - row1).
    g <- exp(p1 - p0) * (J[2, ] - J[1, ])
    list(estimate = exp(p1 - p0), std.error = sqrt(drop(t(g) %*% V %*% g)))
}

want <- oracle_two_arm_exp(mod)
got <- jacobian_path_run("analytic", function() {
    avg_predictions(mod, by = "trt", hypothesis = "exp(b2 - b1) = 0")
})
expect_equal(jacobian_method(got), "analytic+numeric_stage")
expect_equal(got$estimate, want$estimate, tolerance = 1e-10)
expect_equal(got$std.error, want$std.error, tolerance = 1e-6)


# ---------------------------------------------------------------------------
# Regression for #1750: accuracy must not decay with the number of rows.
#
# The manual g-computation and the pairwise comparison are the same estimand.
# Under whole-pipeline differentiation their standard errors diverged by tens
# of percent once the row count grew, because averaging a million predictions
# and then differencing amplifies roundoff by the inverse of the step size.
# This is invisible at N = 100, so the row count is the axis under test.
# ---------------------------------------------------------------------------

for (N in c(100, 10000, 100000)) {
    m <- two_arm_model(N)
    ref <- oracle_two_arm_difference(m)

    a <- on_path("analytic", function() avg_comparisons(m, variables = "trt"))
    b <- on_path("analytic", function() {
        avg_predictions(m, by = "trt", hypothesis = "b2 - b1 = 0")
    })
    expect_equal(a$std.error, ref, tolerance = 1e-10)
    expect_equal(b$std.error, ref, tolerance = 1e-10)

    # The numeric path is screened, not asserted equal: it is the least
    # accurate of the three and is the method under suspicion, not the
    # reference. It must still stay within striking distance of the truth.
    # The bound is 1% because the regression under test produced tens of
    # percent. Anything tighter measures the host's floating-point roundoff
    # rather than the code: the same forward difference lands at 2e-5 on
    # Linux and past 1e-4 on macOS.
    d <- on_path("numeric", function() {
        avg_predictions(m, by = "trt", hypothesis = "b2 - b1 = 0")
    })
    expect_true(abs(d$std.error - ref) / ref < 1e-2)
}


# ---------------------------------------------------------------------------
# Cross-path comparison over the pipeline stages.
#
# The grid crosses the things the stage composition touches: the kind of
# estimand, the comparison function, aggregation, and the hypothesis
# representation.
# ---------------------------------------------------------------------------

set.seed(99)
n_grid <- 300
dat <- data.frame(
    g = factor(sample(letters[1:3], n_grid, replace = TRUE)),
    z = factor(sample(c("a", "b"), n_grid, replace = TRUE)),
    x = rnorm(n_grid),
    w = runif(n_grid, 0.5, 2)
)
dat$y <- rbinom(n_grid, 1, plogis(0.4 * dat$x + as.numeric(dat$g) / 5))
mod_grid <- glm(y ~ g * z + x, data = dat, family = binomial)

cases <- list(
    predictions_plain = function() avg_predictions(mod_grid, by = "g"),
    predictions_string = function() {
        avg_predictions(mod_grid, by = "g", hypothesis = "b2 - b1 = 0")
    },
    predictions_nonlinear = function() {
        avg_predictions(mod_grid, by = "g", hypothesis = "b2 / b1 = 0")
    },
    predictions_formula = function() {
        avg_predictions(mod_grid, by = "g", hypothesis = ~pairwise)
    },
    predictions_matrix = function() {
        avg_predictions(
            mod_grid,
            by = "g",
            hypothesis = matrix(c(-1, 1, 0), ncol = 1)
        )
    },
    comparisons_difference = function() {
        avg_comparisons(mod_grid, variables = "x")
    },
    comparisons_ratio = function() {
        avg_comparisons(mod_grid, variables = "x", comparison = "ratio")
    },
    comparisons_lnratio = function() {
        avg_comparisons(mod_grid, variables = "x", comparison = "lnratio")
    },
    comparisons_by = function() {
        avg_comparisons(mod_grid, variables = "x", by = "g")
    },
    comparisons_wts = function() {
        avg_comparisons(mod_grid, variables = "x", wts = "w")
    },
    comparisons_rowwise = function() {
        comparisons(mod_grid, variables = "x", newdata = head(dat, 5))
    },
    comparisons_link = function() {
        avg_comparisons(mod_grid, variables = "x", type = "link")
    },
    comparisons_hypothesis = function() {
        avg_comparisons(
            mod_grid,
            variables = "x",
            by = "g",
            hypothesis = "b2 - b1 = 0"
        )
    },
    slopes_plain = function() avg_slopes(mod_grid, variables = "x")
)

for (nm in names(cases)) {
    f <- cases[[nm]]
    analytic <- jacobian_path_run("analytic", f)
    numeric <- on_path("numeric", f)

    # Estimates never depend on the differentiation method.
    expect_equal(analytic$estimate, numeric$estimate, tolerance = 1e-12)

    # Screen the numeric path against whichever exact path answered. A loose
    # tolerance is deliberate: forward differences on a full pipeline carry
    # real error, and tightening this would only produce noise failures.
    expect_true(
        max(abs(analytic$std.error - numeric$std.error)) <
            1e-4 * max(1, max(abs(numeric$std.error)))
    )
}


# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# Issue #1735: promoting a linear hypothesis is what makes the hypothesis stage
# exact.
#
# The analytic Jacobian composes a hypothesis exactly only when it is a matrix.
# An opaque R closure still keeps the exact Jacobian of everything upstream
# (#1750), so the fallback is invisible in the label and shows up only in the
# last digits -- which is precisely why this asserts the mechanism rather than
# the path name. A promoted string must agree with the same contrast written
# out as a matrix to the last bit; an unpromoted one must not.
# ---------------------------------------------------------------------------

mod_promote <- glm(am ~ hp + wt, family = binomial, data = mtcars)
f <- function() {
    avg_predictions(mod_promote, by = "cyl", hypothesis = "b2 - b1 = 0")
}

by_matrix <- on_path(
    "analytic",
    function() {
        avg_predictions(
            mod_promote,
            by = "cyl",
            hypothesis = matrix(c(-1, 1, 0), ncol = 1)
        )
    }
)

old <- getOption("marginaleffects_hypothesis_promote")
options(marginaleffects_hypothesis_promote = TRUE)
with_promotion <- on_path("analytic", f)
options(marginaleffects_hypothesis_promote = FALSE)
without <- suppressWarnings(jacobian_path_run("analytic", f))
options(marginaleffects_hypothesis_promote = old)

# Exact, to the last bit: the compiled contrast matrix is the same object the
# explicit matrix argument produces.
expect_identical(with_promotion$std.error, by_matrix$std.error)

# Losing promotion is not free. The closure stage is probed rather than
# differentiated in closed form, so the result moves -- a little, but it moves.
expect_false(identical(without$std.error, by_matrix$std.error))
expect_equal(without$std.error, by_matrix$std.error, tolerance = 1e-8)

# None of this touches the point estimates.
expect_equal(with_promotion$estimate, without$estimate, tolerance = 1e-12)
expect_equal(with_promotion$estimate, by_matrix$estimate, tolerance = 1e-12)


# ---------------------------------------------------------------------------
# Stage helpers, in isolation
# ---------------------------------------------------------------------------

# Linear hypotheses are recovered exactly, as integers and rationals rather
# than as approximations.
skeleton <- data.table::data.table(
    term = c("a", "b", "c", "d"),
    estimate = c(0.50, 0.525, 0.55, 0.575)
)
compile_string <- function(txt) {
    lab <- txt
    attr(lab, "label") <- txt
    marginaleffects:::hypothesis_compile(lab, skeleton)$hyp
}

hyp <- compile_string("b2 - b1")
expect_equal(hyp$kind, "matrix")
expect_equivalent(as.matrix(hyp$H), matrix(c(-1, 1, 0, 0), ncol = 1))

hyp <- compile_string("(b2 + b3) / 2 - b4")
expect_equal(hyp$kind, "matrix")
expect_equivalent(as.matrix(hyp$H), matrix(c(0, 0.5, 0.5, -1), ncol = 1))

# Nonlinear and affine maps must not be promoted: the first has no matrix
# representation at all, and the second cannot be written as crossprod(H, est).
expect_equal(compile_string("b2 / b1")$kind, "string")
expect_equal(compile_string("exp(b2 - b1)")$kind, "string")
expect_equal(compile_string("b1 - 0.5")$kind, "string")

# Promotion is defeatable, which matters because a wrong linearity verdict
# would silently corrupt standard errors rather than fail.
old <- getOption("marginaleffects_hypothesis_promote")
options(marginaleffects_hypothesis_promote = FALSE)
expect_equal(compile_string("b2 - b1")$kind, "string")
options(marginaleffects_hypothesis_promote = old)

# A dense stage probe reproduces derivatives which are known in closed form.
G <- marginaleffects:::stage_jacobian_dense(
    function(e) c(e[2] / e[1], exp(e[2] - e[1])),
    c(0.5, 0.525)
)
expect_equal(G[1, ], c(-0.525 / 0.25, 1 / 0.5), tolerance = 1e-6)
expect_equal(
    G[2, ],
    c(-exp(0.025), exp(0.025)),
    tolerance = 1e-6
)

# ---------------------------------------------------------------------------
# Estimates are never altered by any of this
# ---------------------------------------------------------------------------

expect_equal(
    avg_predictions(mod_grid, by = "g", hypothesis = "b2 - b1 = 0")$estimate,
    diff(rev(avg_predictions(mod_grid, by = "g")$estimate[1:2])) * -1,
    tolerance = 1e-12
)

p <- avg_predictions(mod_grid, by = "g")
expect_equal(
    avg_predictions(mod_grid, by = "g", hypothesis = "b2 / b1 = 0")$estimate,
    p$estimate[2] / p$estimate[1],
    tolerance = 1e-12
)
