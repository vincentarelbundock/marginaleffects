source("helpers.R")
using("marginaleffects")

exit_if_not(requiet("glmx"))
exit_if_not(requiet("MASS"))
exit_if_not(requiet("margins"))

# glmx: marginaleffects vs. margins
d <- data.frame(x = runif(200, -1, 1))
d$y <- rnbinom(200, mu = exp(0 + 3 * d$x), size = 1)
model <- glmx(y ~ x, data = d, family = negative.binomial, 
          xlink = "log", xstart = 0)
expect_slopes(model)
# margins produces all zeros for se
mar <- margins(model, unit_ses = TRUE)
mfx <- slopes(model)
expect_true(expect_margins(mfx, mar, se = FALSE, tolerance = .001))


# predictions: glmx: no validity check
#skip_if_not_installed("insight", minimum_version = "0.17.1")
d <- data.frame(x = runif(200, -1, 1))
d$y <- rnbinom(200, mu = exp(0 + 3 * d$x), size = 1)
model <- glmx(y ~ x, data = d, family = negative.binomial,
          xlink = "log", xstart = 0)
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(d))
expect_predictions(pred1, n_row = nrow(d))
expect_predictions(pred2, n_row = 6)
