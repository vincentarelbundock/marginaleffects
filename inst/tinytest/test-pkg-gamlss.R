# bug fix submitted for this version of insight
source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("margins")
requiet("emmeans")
requiet("broom")
requiet("gamlss")
requiet("titanic")

# Beta regression
data("GasolineYield", package = "betareg")
tmp <- GasolineYield
tmp$batch <- factor(tmp$batch)
dat <- tmp
mod <- gamlss::gamlss(yield ~ batch + temp,
    family = "BE",
    data = dat,
    trace = FALSE)


expect_error(predictions(mod, newdata = head(dat)), pattern = "what. argument")
p1 <- predictions(mod, newdata = head(dat), what = "mu")
p2 <- predictions(mod, newdata = head(dat), what = "sigma")
expect_inherits(p1, "predictions")
expect_inherits(p2, "predictions")


# EMMeans provides the same results whether regrid = "response" or
# regrid = "link"

# marginaleffects
mfx <- marginaleffects(
    mod,
    type = "link",
    newdata = datagrid(batch = 1),
    variables = "temp",
    what = "mu")

# emtrends
em <- emtrends(mod, ~temp, "temp", at = list("batch" = tmp$batch[1]))
em <- data.frame(em)

# We do expect that they will be equivalent
expect_equivalent(mfx$dydx, em$temp.trend, tolerance = .001)
expect_equivalent(mfx$std.error, em$SE, tolerance = .001)

# predictions: no validity
pred <- suppressWarnings(predictions(mod, what = "mu"))
expect_predictions(pred, n_row = nrow(GasolineYield))
pred <- predictions(mod, newdata = datagrid(batch = 1:3, temp = c(300, 350)),
                    what = "mu")
expect_predictions(pred, n_row = 6)


# marginalmeans: vs. emmeans
mm <- marginalmeans(mod, what = "mu")
expect_marginalmeans(mm, n_row = 10)
mm <- tidy(mm)
em <- broom::tidy(emmeans::emmeans(mod, "batch", type = "response"))
expect_equivalent(mm$estimate, em$response)
expect_equivalent(mm$std.error, em$std.error, tolerance = 0.01)

# Logistic regression
tmp <- titanic_train
tmp$Pclass <- as.factor(tmp$Pclass)
dat <- na.omit(tmp)

mod <- gamlss::gamlss(Survived ~ Age + Pclass, 
                      family = "BI", data = dat, trace = FALSE)


# The R-package margins does not provide support to gamlss.
# Error in tmp[["fit"]] : subscript out of bounds
# In addition: Warning message:
#   In predict.gamlss(model, newdata = out, type = type, se.fit = TRUE,  :
#                       se.fit = TRUE is not supported for new data values at the moment 


# emtrends
mfx <- marginaleffects(mod,
    type = "link",
    newdata = datagrid(Pclass = "1"),
    variables = "Age",
    what = "mu")
em <- emtrends(mod, ~Age, "Age", at = list("Pclass" = "1"))
em <- tidy(em)
expect_equivalent(mfx$dydx, em$Age.trend, tolerance = .001)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)

# predictions: no validity
pred <- predictions(mod, what = "mu")
expect_predictions(pred, n_row = nrow(na.omit(titanic_train)))
pred <- predictions(
    mod,
    newdata = datagrid(Pclass = 1:3, Age = c(25, 50)),
    what = "mu")
expect_predictions(pred, n_row = 6)


# marginalmeans: vs. emmeans
mm <- marginalmeans(mod, variables = "Pclass", what = "mu")
expect_marginalmeans(mm, n_row = 3)
mm <- tidy(mm)
em <- broom::tidy(emmeans::emmeans(mod, "Pclass", type = "response"))
expect_equivalent(mm$estimate, em$response)
expect_equivalent(mm$std.error, em$std.error, tolerance = 0.01)
