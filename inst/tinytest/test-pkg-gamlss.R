# bug fix submitted for this version of insight
source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
if (minver("insight", "0.17.1")) exit_file("insight 0.17.1")
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
mod <- gamlss::gamlss(yield ~ batch + temp, family = "BE", data = dat)

# marginaleffects: vs. margins vs. emmeans
set.seed(1024)
res <- marginaleffects(mod, variables = "temp", what = "mu")
mar <- data.frame(margins::margins(mod, unit_ses = TRUE))

# The R-package margins does not provide support to gamlss.
# Error in jacobian %*% vcov : non-conformable arguments
# expect_true(expect_margins(res, mar, tolerance = 0.1))


# EMMeans provides the same results whether regrid = "response" or
# regrid = "link"

# marginaleffects
mfx <- marginaleffects(mod, type = "link", 
                       newdata = datagrid(batch = 1), variables = "temp",
                       what = "mu")

# emtrends with regrid = "link"
em <- suppressWarnings(
emtrends(mod, ~temp, "temp",  regrid = "link", 
         at = list("batch" = tmp$batch[1])))
em <- tidy(em)

# emtrends with regrid = "response"
em2 <- suppressWarnings(
  emtrends(mod, ~temp, "temp",  regrid = "response", 
           at = list("batch" = tmp$batch[1])))
em2 <- tidy(em2)

# We do not expect that they will be equivalent
!expect_equivalent(em2$temp.trend, em$temp.trend, tolerance = .001)
!expect_equivalent(em2$std.error, em$std.error, tolerance = .001)

# We do expect that they will be equivalent
expect_equivalent(mfx$dydx, em$temp.trend, tolerance = .001)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)

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
                      family = "BI", data = dat)

# marginaleffects: vs. margins vs. emmeans
set.seed(1024)
res <- marginaleffects(mod, variables = "Pclass", what = "mu")
mar <- data.frame(margins::margins(mod, unit_ses = TRUE))

# The R-package margins does not provide support to gamlss.
# Error in tmp[["fit"]] : subscript out of bounds
# In addition: Warning message:
#   In predict.gamlss(model, newdata = out, type = type, se.fit = TRUE,  :
#                       se.fit = TRUE is not supported for new data values at the moment 


# emtrends
mfx <- marginaleffects(mod, type = "link", 
                       newdata = datagrid(Pclass = "1"), variables = "Age",
                       what = "mu")
em <- suppressWarnings(
  emtrends(mod, ~Age, "Age",  regrid = "link", 
           at = list("Pclass" = "1")))
em <- tidy(em)
expect_equivalent(mfx$dydx, em$Age.trend, tolerance = .001)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)

# predictions: no validity
pred <- suppressWarnings(predictions(mod, what = "mu"))
expect_predictions(pred, n_row = nrow(na.omit(titanic_train)))
pred <- predictions(mod, newdata = datagrid(Pclass = 1:3, Age = c(25, 50)),
                    what = "mu")
expect_predictions(pred, n_row = 6)


# marginalmeans: vs. emmeans
mm <- marginalmeans(mod, variables = "Pclass", what = "mu")
expect_marginalmeans(mm, n_row = 3)
mm <- tidy(mm)
em <- broom::tidy(emmeans::emmeans(mod, "Pclass", type = "response"))
expect_equivalent(mm$estimate, em$response)
expect_equivalent(mm$std.error, em$std.error, tolerance = 0.01)
