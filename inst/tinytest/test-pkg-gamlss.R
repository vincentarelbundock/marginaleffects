source("helpers.R")
using("marginaleffects")

requiet("margins")
requiet("emmeans")
requiet("broom")
requiet("gamlss")
requiet("titanic")

# Beta regression
tmp <- get_dataset("GasolineYield", "betareg")
tmp$batch <- factor(tmp$batch)
dat <<- tmp
mod <- gamlss::gamlss(yield ~ batch + temp, family = "BE", data = dat, trace = FALSE)


expect_error(predictions(mod, newdata = head(dat)), pattern = "what. argument")
p1 <- predictions(mod, newdata = head(dat), what = "mu")
p2 <- predictions(mod, newdata = head(dat), what = "sigma")
expect_inherits(p1, "predictions")
expect_inherits(p2, "predictions")


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
expect_equivalent(mfx$estimate, em$temp.trend, tolerance = .001)
expect_equivalent(mfx$std.error, em$SE, tolerance = .001)

# predictions: no validity
pred <- suppressWarnings(predictions(mod, what = "mu"))
expect_predictions(pred, n_row = nrow(tmp))
pred <- predictions(mod, newdata = datagrid(batch = 1:3, temp = c(300, 350)), what = "mu")
expect_predictions(pred, n_row = 6)


# marginalmeans: vs. emmeans
mm <- predictions(mod, by = "batch", newdata = datagrid(grid_type = "balanced"), what = "mu")
em <- broom::tidy(emmeans::emmeans(mod, "batch", type = "response"))
expect_equivalent(mm$estimate, em$response, tol = 0.001)
expect_equivalent(mm$std.error, em$std.error, tolerance = 0.01)

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
expect_equivalent(mfx$estimate, em$Age.trend, tolerance = .001)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)

# predictions: no validity
pred <- predictions(mod, what = "mu")

expect_predictions(pred, n_row = nrow(na.omit(titanic_train)))
pred <- predictions(
    mod,
    newdata = datagrid(Pclass = 1:3, Age = c(25, 50)),
    what = "mu"
)
expect_predictions(pred, n_row = 6)


# marginalmeans: vs. emmeans
mm <- predictions(mod, by = "Pclass", what = "mu", newdata = datagrid(grid_type = "balanced")) |>
    dplyr::arrange(Pclass)
mm <- tidy(mm)
em <- broom::tidy(emmeans::emmeans(mod, "Pclass", type = "response"))
expect_equivalent(mm$estimate, em$response)
expect_equivalent(mm$std.error, em$std.error, tolerance = 0.01)


# Issue #933
dat <- get_dataset("penguins", "palmerpenguins")
dat <- dat |>
    transform(prop = rBE(nrow(dat), mu = 0.5, sigma = 0.2)) |>
    na.omit()
mod <- gamlss::gamlss(
    prop ~ sex * body_mass_g + year + re(random = list(~ 1 | species, ~ 1 | island)),
    family = BE(),
    data = dat,
    trace = FALSE
)
cmp <- avg_comparisons(mod, what = "mu") |> suppressWarnings()
expect_inherits(cmp, "comparisons")

# end.
