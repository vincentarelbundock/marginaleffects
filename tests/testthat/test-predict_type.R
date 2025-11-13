# sanity gives informative error for all the functions
dat_predict_type <- mtcars
dat_predict_type$cyl <- factor(dat_predict_type$cyl)
dat_predict_type <- dat_predict_type
mod <- lm(mpg ~ hp + cyl, data = dat_predict_type)
expect_error(comparisons(mod, type = "junk"), regexp = "Must be element")
expect_error(predictions(mod, type = "junk"), regexp = "Must be element")
expect_error(slopes(mod, type = "junk"), regexp = "Must be element")


# error: multivariate
testthat::skip_if_not_installed("pscl")
requiet("pscl")
dat2_predict_type <- get_dataset("bioChemists", "pscl")
model <- hurdle(art ~ phd + fem | ment, data = dat2_predict_type, dist = "negbin")
mfx <- slopes(model, type = "prob")
expect_true(all(as.character(0:19) %in% mfx$group))


# Issue #1123: invlink(link) not default for avg_predictions()
mod <- glm(am ~ hp, data = mtcars, family = binomial)
p1 <- avg_predictions(mod)
p2 <- avg_predictions(mod, type = "response")
p3 <- avg_predictions(mod, type = "invlink(link)")
expect_equal(p1$estimate, p2$estimate, ignore_attr = TRUE)
expect_false(isTRUE(all.equal(p1$estimate, p3$estimate)))
