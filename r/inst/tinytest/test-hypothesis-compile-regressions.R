source("helpers.R")
using("marginaleffects")

# Review of #1787: constants from the user's environment must not break
# compilation; they decline matrix promotion and use the closure instead.
# String hypotheses resolve free symbols in the global environment, as on
# main, so the constant is defined there.
mod <- lm(mpg ~ wt, mtcars)
assign("k_hypothesis_regression", 2, envir = .GlobalEnv)
h <- hypotheses(mod, "wt + k_hypothesis_regression = 0")
ref <- hypotheses(mod, "wt + 2 = 0")
rm("k_hypothesis_regression", envir = .GlobalEnv)
expect_equivalent(h$estimate, ref$estimate)
expect_equivalent(h$std.error, ref$std.error, tolerance = 1e-6)

# Review of #1787: a missing estimate which the hypothesis does not use must
# not leak into its standard error (0 * NA = NA in a contrast matrix).
p <- suppressWarnings(predictions(
    mod,
    newdata = data.frame(wt = c(2, NA_real_)),
    hypothesis = "b1 = 0"
))
ref <- predictions(mod, newdata = data.frame(wt = 2))
expect_false(is.na(p$std.error))
expect_equivalent(p$std.error, ref$std.error, tolerance = 1e-6)
set.seed(1)
s <- inferences(p, method = "simulation", R = 10)
expect_false(anyNA(c(s$conf.low, s$conf.high)))

# Same guard for formula hypotheses: a missing estimate in one group must not
# contaminate the other group's contrast.
nd <- data.frame(wt = c(2, 3, NA_real_, 4), grp = c(1, 1, 2, 2))
p <- predictions(mod, newdata = nd, hypothesis = ~ pairwise | grp)
ref <- predictions(mod, newdata = nd[1:2, ], hypothesis = ~pairwise)
expect_false(is.na(p$std.error[1]))
expect_equivalent(p$std.error[1], ref$std.error, tolerance = 1e-6)
