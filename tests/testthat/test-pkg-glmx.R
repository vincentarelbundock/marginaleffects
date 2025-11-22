testthat::skip_if_not_installed("glmx")
testthat::skip_if_not_installed("MASS")
testthat::skip_if_not_installed("margins")

requiet("glmx")
requiet("MASS")
requiet("margins")

# Basic expectation tests
tmp <- data.frame(x = runif(200, -1, 1))
tmp$y <- rnbinom(200, mu = exp(0 + 3 * tmp$x), size = 1)
d_glmx <- tmp
mod_simple <- glmx(y ~ x, data = d_glmx, family = negative.binomial, xlink = "log", xstart = 0)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# glmx: marginaleffects vs. margins
tmp <- data.frame(x = runif(200, -1, 1))
tmp$y <- rnbinom(200, mu = exp(0 + 3 * tmp$x), size = 1)
d_glmx <- tmp
model <- glmx(y ~ x, data = d_glmx, family = negative.binomial, xlink = "log", xstart = 0)
expect_slopes2(model)
# margins produces all zeros for se
mar <- margins(model, unit_ses = TRUE)
mfx <- slopes(model)
expect_margins2(mfx, mar, se = FALSE, tolerance = .001)


# predictions: glmx: no validity check
# skip_if_not_installed("insight", minimum_version = "0.17.1")
tmp <- data.frame(x = runif(200, -1, 1))
tmp$y <- rnbinom(200, mu = exp(0 + 3 * tmp$x), size = 1)
d_glmx <- tmp
dhead <- head(d_glmx)
model <- glmx(y ~ x, data = d_glmx, family = negative.binomial, xlink = "log", xstart = 0)
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = dhead)
expect_predictions2(model, n_row = nrow(d_glmx))
expect_predictions2(model, newdata = dhead, n_row = 6)
