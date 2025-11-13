testthat::skip_if_not_installed("robustbase")
testthat::skip_if_not_installed("margins")
requiet("robustbase")
requiet("margins")

# Basic expectation tests
mod_simple <- robustbase::lmrob(mpg ~ wt + am, data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# lmrob vs. margins
data(coleman, package = "robustbase")
model <- lmrob(Y ~ ., data = coleman, setting = "KS2014")
expect_slopes2(model, n_unique = 1)
mar <- margins::margins(model, unit_ses = TRUE)
mfx <- slopes(model)
expect_margins2(mar, mfx)

# glmrob vs. margins
data(epilepsy, package = "robustbase")
model <- glmrob(
    Ysum ~ Age10 + Base4 * Trt,
    family = poisson,
    data = epilepsy,
    method = "Mqle",
    control = glmrobMqle.control(tcc = 1.2)
)
expect_slopes2(model)
mar <- margins::margins(model, unit_ses = TRUE)
mfx <- slopes(model)
expect_margins2(mar, mfx)
