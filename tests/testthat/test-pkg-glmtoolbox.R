testthat::skip_if_not_installed("glmtoolbox")
requiet("glmtoolbox")

# Basic expectation tests
data(spruces, package = "glmtoolbox")
mod_simple <- glmtoolbox::glmgee(size ~ days + treat, id = tree, family = Gamma(log), data = spruces)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

data(spruces)
mod <- size ~ poly(days, 4) + treat
fit <- glmgee(mod, id = tree, family = Gamma(log), corstr = "AR-M-dependent(1)", data = spruces)

s <- avg_slopes(fit)
expect_s3_class(s, "slopes")
p <- avg_predictions(fit, by = "treat")
expect_s3_class(p, "predictions")
k <- avg_comparisons(fit)
expect_s3_class(k, "comparisons")
