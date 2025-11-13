testthat::skip_if_not_installed("fixest")
testthat::skip_if_not_installed("data.table")

requiet("fixest")
requiet("data.table")
fixest::setFixest_nthreads(1)
fixest::setFixest_notes(FALSE)

# Basic expectation tests
mod_simple <- fixest::feols(mpg ~ wt + am, data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# Issue #375: friendly warning when sandwich fails
options(marginaleffects_safe = TRUE)
mod <- feols(y ~ x1 + i(period, treat, 5) | id + period, base_did)
hyp <- as.numeric(1:10 %in% 6:10)
# not supported
expect_warning(hypotheses(mod, hypothesis = hyp, vcov = "HC0"), regexp = "sandwich")
# supported
d <- hypotheses(mod, hypothesis = hyp, vcov = "HC1")
expect_s3_class(d, "data.frame")
options(marginaleffects_safe = FALSE)

# bugs stay dead: logit with transformations
dat <- mtcars
dat$gear <- as.factor(dat$gear)
dat <- dat
mod1 <- suppressMessages(feglm(am ~ mpg + mpg^2 | gear, family = binomial(link = "logit"), data = dat, warn = FALSE))
mod2 <- suppressMessages(feglm(am ~ mpg | gear, family = binomial(link = "logit"), data = dat, warn = FALSE))
mod3 <- suppressMessages(feglm(am ~ mpg + mpg^2 | gear, family = binomial(link = "logit"), data = mtcars, warn = FALSE))
mod4 <- suppressMessages(feglm(am ~ mpg | gear, family = binomial(link = "logit"), data = mtcars, warn = FALSE))

# skip_if_not_installed("fixest", minimum_version = "0.10.2")
expect_s3_class(insight::get_data(mod1), "data.frame")
expect_s3_class(insight::get_data(mod2), "data.frame")
expect_s3_class(insight::get_data(mod3), "data.frame")
expect_s3_class(insight::get_data(mod4), "data.frame")

# 20 observations for which we can't compute results
mfx <- slopes(mod1, variables = "mpg", vcov = FALSE)
expect_s3_class(mfx, "marginaleffects")
expect_equal(nrow(mfx), 12, ignore_attr = TRUE)


# fixest::feols vs. Stata
testthat::skip_if_not_installed("plm")
requiet("plm")
data(EmplUK, package = "plm")
stata <- readRDS(testing_path("stata/stata.rds"))$fixest_feols
model <- feols(wage ~ capital * output | firm, EmplUK, se = "cluster")
mfx <- merge(avg_slopes(model), stata)
expect_slopes2(model)
expect_equal(mfx$estimate, mfx$estimate, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .00001, ignore_attr = TRUE)


# fixest::fepois vs. Stata
requiet("plm")
data(EmplUK, package = "plm")
stata <- readRDS(testing_path("stata/stata.rds"))$fixest_fepois
model <- fepois(log(wage) ~ capital * output | firm, EmplUK)
mfx <- merge(tidy(slopes(model, type = "link", vcov = FALSE)), stata)
expect_equal(mfx$estimate, mfx$estimate, tolerance = .000001, ignore_attr = TRUE)
# expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001, ignore_attr = TRUE)

# fixest::feols: predictions
data(trade, package = "fixest")
dat <- trade
model <- feols(Euros ~ dist_km | Destination + Origin, data = dat)
pred1 <- predictions(model, vcov = FALSE)
pred2 <- predictions(model, newdata = head(dat), vcov = FALSE)
expect_s3_class(pred1, "predictions")
expect_s3_class(pred2, "predictions")


# numeric cluster variable raises warning
fe <- data.frame(unit = 1:25, fe = rnorm(25))
dat <- expand.grid(unit = 1:25, time = 1:50)
dat <- merge(dat, fe, by = "unit")
dat$x <- rnorm(nrow(dat)) + dat$fe
dat$w <- rnorm(nrow(dat))
dat$y <- dat$x + dat$w + dat$x * dat$w + dat$fe + rnorm(nrow(dat), sd = 10)
dat <- dat
dat2 <- dat
dat2$unit <- as.factor(dat2$unit)
dat2 <- dat2
mod1 <- feols(y ~ x * w | unit, data = dat)
mod2 <- fixest::feols(y ~ x * w | unit, data = dat2)
p <- plot_slopes(mod2, variables = "x", condition = "w")
expect_s3_class(p, "ggplot")


# plot_slopes: extracts all required data
fe <- data.frame(unit = 1:25, fe = rnorm(25))
dat <- expand.grid(unit = 1:25, time = 1:50)
dat <- merge(dat, fe, by = "unit")
dat$x <- rnorm(nrow(dat)) + dat$fe
dat$w <- rnorm(nrow(dat))
dat$y <- dat$x + dat$w + dat$x * dat$w + dat$fe + rnorm(nrow(dat), sd = 10)
