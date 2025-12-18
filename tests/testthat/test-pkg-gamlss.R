testthat::skip_if_not_installed("margins")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")
testthat::skip_if_not_installed("gamlss")
testthat::skip_if_not_installed("titanic")

requiet("margins")
requiet("emmeans")
requiet("broom")
requiet("gamlss")
requiet("titanic")

# Basic expectation tests
mod_simple <- gamlss::gamlss(mpg ~ wt + am,
    data = mtcars,
    family = "NO", trace = FALSE)
expect_slopes2(mod_simple, what = "mu")
expect_predictions2(mod_simple, what = "mu")
expect_comparisons2(mod_simple, what = "mu")
expect_error(hypotheses(mod_simple), regexp = "does not support")

# Beta regression
tmp <- get_dataset("GasolineYield", "betareg")
tmp$batch <- factor(tmp$batch)
dat <<- tmp
mod <- gamlss::gamlss(yield ~ batch + temp, family = "BE", data = dat, trace = FALSE)


expect_error(predictions(mod, newdata = head(dat)), regexp = "what. argument")
p1 <- predictions(mod, newdata = head(dat), what = "mu")
p2 <- predictions(mod, newdata = head(dat), what = "sigma")
expect_s3_class(p1, "predictions")
expect_s3_class(p2, "predictions")


# EMMeans provides the same results whether regrid = "response" or
# regrid = "link"

# marginaleffects
mfx <- slopes(
    mod,
    type = "link",
    newdata = datagrid(batch = 1),
    variables = "temp",
    what = "mu"
)

# emtrends
em <- emtrends(mod, ~temp, "temp", at = list("batch" = tmp$batch[1]))
em <- data.frame(em)

# We do expect that they will be equivalent
expect_equal(mfx$estimate, em$temp.trend, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx$std.error, em$SE, tolerance = .001, ignore_attr = TRUE)

# predictions: no validity
pred <- suppressWarnings(predictions(mod, what = "mu"))
expect_predictions2(mod, what = "mu", n_row = nrow(tmp))
pred <- predictions(mod, newdata = datagrid(batch = 1:3, temp = c(300, 350)), what = "mu")
expect_predictions2(mod, newdata = datagrid(batch = 1:3, temp = c(300, 350)), what = "mu", n_row = 6)


# marginalmeans: vs. emmeans
mm <- predictions(mod, by = "batch", newdata = datagrid(grid_type = "balanced"), what = "mu")
em <- broom::tidy(emmeans::emmeans(mod, "batch", type = "response"))
expect_equal(mm$estimate, em$response, tolerance = 0.001, ignore_attr = TRUE)
expect_equal(mm$std.error, em$std.error, tolerance = 0.01, ignore_attr = TRUE)

# Logistic regression
data("titanic_train", package = "titanic")
tmp <- titanic_train
tmp$Pclass <- as.factor(tmp$Pclass)
dat <<- na.omit(tmp)

mod <- gamlss::gamlss(Survived ~ Age + Pclass, family = "BI", data = dat, trace = FALSE)


# The R-package margins does not provide support to gamlss.
# Error in tmp[["fit"]] : subscript out of bounds
# In addition: Warning message:
#   In predict.gamlss(model, newdata = out, type = type, se.fit = TRUE,  :
#                       se.fit = TRUE is not supported for new data values at the moment

# emtrends
mfx <- slopes(mod, type = "link", newdata = datagrid(Pclass = "1"), variables = "Age", what = "mu")
em <- emtrends(mod, ~Age, "Age", at = list("Pclass" = "1"))
em <- tidy(em)
expect_equal(mfx$estimate, em$Age.trend, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)

# predictions: no validity
pred <- predictions(mod, what = "mu")

expect_predictions2(mod, what = "mu", n_row = nrow(na.omit(titanic_train)))
pred <- predictions(
    mod,
    newdata = datagrid(Pclass = 1:3, Age = c(25, 50)),
    what = "mu"
)
expect_predictions2(mod, newdata = datagrid(Pclass = 1:3, Age = c(25, 50)), what = "mu", n_row = 6)


# marginalmeans: vs. emmeans
mm <- predictions(mod, by = "Pclass", newdata = datagrid(grid_type = "balanced"), what = "mu")
em <- broom::tidy(emmeans::emmeans(mod, "Pclass", type = "response"))
expect_equal(mm$estimate, em$response, tolerance = 0.001, ignore_attr = TRUE)
expect_equal(mm$std.error, em$std.error, tolerance = 0.01, ignore_attr = TRUE)
