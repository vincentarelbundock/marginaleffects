source("helpers.R")
using("marginaleffects")
requiet("mhurdle")

if (packageVersion("insight") <= "1.4.0") {
    exit_file("Insight Issue #1114")
}

tol <- 0.001
tol_se <- 0.001

quiet_mhurdle_fit <- function(expr) {
    withCallingHandlers(
        force(expr),
        warning = function(w) {
            msg <- conditionMessage(w)
            noisy <- c(
                "subscript out of bounds, not all 'rhs' available",
                "NaNs produced"
            )
            if (isTRUE(msg %in% noisy)) {
                invokeRestart("muffleWarning")
            }
        }
    )
}

# Basic expectation tests
data("Interview", package = "mhurdle")
mod_simple <- quiet_mhurdle_fit(
    mhurdle(shows ~ educ + size | linc,
        data = Interview, h2 = TRUE, dist = "n", method = "bhhh")
)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

data("Interview", package = "mhurdle")
m1 <- quiet_mhurdle_fit(
    mhurdle(shows ~ 0 | linc + smsa + age + educ + size,
        data = Interview, h2 = TRUE, dist = "n", method = "bhhh")
)
m2 <- quiet_mhurdle_fit(
    mhurdle(
        shows ~ educ + size | linc | smsa + age,
        data = Interview,
        h2 = FALSE,
        method = "bhhh",
        corr = TRUE,
        finalHessian = TRUE
    )
)

# marginaleffects vs. margins (unit-level SEs)
set.seed(1024)
nd <- Interview[sample(seq_len(nrow(Interview)), 10), ]
mfx <- slopes(m2, newdata = nd, type = "E")
mar <- margins(m2, type = "response", data = nd, unit_ses = TRUE)

expect_equivalent(mfx[mfx$term == "linc", "estimate"], as.numeric(mar$dydx_linc), tolerance = tol)
expect_equivalent(mfx[mfx$term == "educ", "estimate"], as.numeric(mar$dydx_educ), tolerance = tol)
expect_equivalent(mfx[mfx$term == "age", "estimate"], as.numeric(mar$dydx_age), tolerance = tol)

# `margins` builds its Jacobian without varying the scale parameter of an
# `mhurdle` model, so its standard errors are not a valid reference. Compare
# against a delta method built on a Richardson-extrapolated Jacobian of the
# full parameter vector instead (see `set_coef.mhurdle()`).
requiet("numDeriv")
mhurdle_eps <- 1e-4
mhurdle_dydx <- function(model, variable, data) {
    hi <- lo <- data
    hi[[variable]] <- data[[variable]] + mhurdle_eps / 2
    lo[[variable]] <- data[[variable]] - mhurdle_eps / 2
    p_hi <- stats::predict(model, newdata = hi, what = "E")
    p_lo <- stats::predict(model, newdata = lo, what = "E")
    (p_hi - p_lo) / mhurdle_eps
}
mhurdle_delta_se <- function(model, fun) {
    J <- numDeriv::jacobian(
        function(x) {
            m <- model
            m$coefficients[] <- x
            fun(m)
        },
        model$coefficients,
        method = "Richardson"
    )
    sqrt(diag(J %*% stats::vcov(model) %*% t(J)))
}

for (v in c("linc", "educ", "age")) {
    ref <- mhurdle_delta_se(m2, function(m) mhurdle_dydx(m, v, nd))
    expect_equivalent(mfx[mfx$term == v, "std.error"], ref, tolerance = 0.01)
}

# marginaleffects vs. margins: AME
mfx <- avg_slopes(m2, type = "E")
mfx <- mfx[match(c("age", "educ", "linc", "size", "smsa"), mfx$term), ]
mar <- margins(m2)
mar <- summary(mar)
expect_equivalent(mfx$estimate, mar$AME, tolerance = tol)
for (v in c("age", "educ", "linc", "size")) {
    ref <- mhurdle_delta_se(m2, function(m) mean(mhurdle_dydx(m, v, Interview)))
    expect_equivalent(mfx[mfx$term == v, "std.error"], ref, tolerance = 0.01)
}


# set_coef.mhurdle(): `coef()` renames the scale parameter from `sd` to
# `sd.sd`, so name-matching against `model$coefficients` used to append a stray
# element and leave the real scale parameter frozen.
mod_sd <- quiet_mhurdle_fit(
    mhurdle(shows ~ educ + size | linc,
        data = Interview, h2 = TRUE, dist = "n", method = "bhhh")
)
b <- get_coef(mod_sd)
mod_roundtrip <- set_coef(mod_sd, b)
expect_equivalent(mod_roundtrip$coefficients, mod_sd$coefficients)
expect_equivalent(names(mod_roundtrip$coefficients), names(mod_sd$coefficients))

b["sd.sd"] <- b["sd.sd"] * 1.5
mod_bigger_sd <- set_coef(mod_sd, b)
expect_equivalent(unname(coef(mod_bigger_sd)["sd.sd"]), unname(b["sd.sd"]))
expect_false(isTRUE(all.equal(
    predict(mod_sd, newdata = Interview, what = "E"),
    predict(mod_bigger_sd, newdata = Interview, what = "E"))))
