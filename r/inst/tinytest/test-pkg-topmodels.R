source("helpers.R")
using("marginaleffects")

if (!requireNamespace("topmodels", quietly = TRUE)) exit_file("topmodels is not on CRAN")
if (!requireNamespace("distributions3", quietly = TRUE)) exit_file("distributions3")
requiet("topmodels")

data("FIFA2018", package = "distributions3")
mod <- glm(goals ~ difference, data = FIFA2018, family = poisson)
p <- promodel(mod)

procast <- function(nd = FIFA2018, type = "mean", ...) {
    topmodels::procast(mod, newdata = nd, type = type, ...)[[1L]]
}

# unsupported `type` raises an informative error
expect_error(avg_slopes(p, type = "junk"), pattern = "element of set")

# predictions() matches procast()
pre <- predictions(p)
expect_equivalent(pre$estimate, procast())
expect_true(all(pre$std.error > 0))
expect_equivalent(predictions(p, type = "variance")$estimate, procast(type = "variance"))

# `...` (here: `at`) is forwarded to procast()
expect_equivalent(
    predictions(p, type = "probability", at = 3)$estimate,
    procast(type = "probability", at = 3))

# slopes match a finite difference on procast()
eps <- 1e-5
lo <- transform(FIFA2018, difference = difference - eps / 2)
hi <- transform(FIFA2018, difference = difference + eps / 2)
expect_equivalent(
    slopes(p, eps = eps)$estimate,
    (procast(hi) - procast(lo)) / eps)
expect_equivalent(
    slopes(p, type = "probability", at = 3, eps = eps)$estimate,
    (procast(hi, "probability", at = 3) - procast(lo, "probability", at = 3)) / eps)

# comparisons on a user-specified step
cmp <- comparisons(p, variables = list(difference = 2), type = "probability", at = 3)
expect_equivalent(
    cmp$estimate,
    procast(transform(FIFA2018, difference = difference + 2), "probability", at = 3) -
        procast(type = "probability", at = 3))
expect_true(all(cmp$std.error > 0))

# all supported types run
types <- c(
    "mean", "variance", "quantile", "probability",
    "density", "loglikelihood", "kurtosis", "skewness")
for (ty in types) {
    expect_inherits(avg_predictions(p, type = ty), "predictions")
}
