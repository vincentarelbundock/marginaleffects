source("helpers.R", local = TRUE)
exit_file("problems testing fixest because of `get_data` from environment")
if (utils::packageVersion("fixest") < "0.10.5") exit_file("fixest version")
if (ON_CRAN) exit_file("on cran")
requiet("fixest")
fixest::setFixest_nthreads(1)


# Issue #375: friendly warning when sandwich fails
mod <- feols(y ~ x1 + i(period, treat, 5) | id + period, base_did)
hyp <- as.numeric(1:10 %in% 6:10)
expect_warning(deltamethod(mod, hypothesis = hyp, vcov = "HC1"), pattern = "sandwich")

# bugs stay dead: logit with transformations
dat <- mtcars
dat$gear <- as.factor(dat$gear)
dat <<- dat
mod1 <- suppressMessages(feglm(am ~ mpg + mpg^2 | gear, family = binomial(link = "logit"), data = dat, warn = FALSE))
mod2 <- suppressMessages(feglm(am ~ mpg | gear, family = binomial(link = "logit"), data = dat, warn = FALSE))
mod3 <- suppressMessages(feglm(am ~ mpg + mpg^2 | gear, family = binomial(link = "logit"), data = mtcars, warn = FALSE))
mod4 <- suppressMessages(feglm(am ~ mpg | gear, family = binomial(link = "logit"), data = mtcars, warn = FALSE))

#skip_if_not_installed("fixest", minimum_version = "0.10.2")
expect_inherits(insight::get_data(mod1), "data.frame")
expect_inherits(insight::get_data(mod2), "data.frame")
expect_inherits(insight::get_data(mod3), "data.frame")
expect_inherits(insight::get_data(mod4), "data.frame")

expect_marginaleffects(mod1, pct_na = 62.5)
expect_marginaleffects(mod2, pct_na = 62.5)
expect_marginaleffects(mod3, pct_na = 62.5)
expect_marginaleffects(mod4, pct_na = 62.5)

mfx <- marginaleffects(mod1, variables = "mpg")
expect_inherits(mfx, "marginaleffects")
expect_equivalent(sum(is.na(mfx$dydx)), 20)
expect_equivalent(sum(is.na(mfx$std.error)), 20)



# fixest::feols vs. Stata
requiet("plm")
data(EmplUK, package = "plm")
stata <- readRDS(testing_path("stata/stata.rds"))$fixest_feols
model <- feols(wage ~ capital * output | firm, EmplUK)
mfx <- merge(tidy(marginaleffects(model)), stata)
expect_marginaleffects(model)
expect_equivalent(mfx$estimate, mfx$dydx)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .00001)


# fixest::fepois vs. Stata
requiet("plm")
data(EmplUK, package = "plm")
stata <- readRDS(testing_path("stata/stata.rds"))$fixest_fepois
model <- fepois(log(wage) ~ capital * output | firm, EmplUK)
mfx <- merge(tidy(marginaleffects(model, type = "link")), stata)
expect_marginaleffects(model)
expect_equivalent(mfx$estimate, mfx$dydx, tolerance = .000001)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .001)


# fixest::feols: predictions
data(trade, package = "fixest")
model <- feols(Euros ~ dist_km | Destination + Origin, data = trade)
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(trade))
expect_predictions(pred1)
expect_predictions(pred2, n_row = 6)


# numeric cluster variable raises warning
fe <- data.frame(unit = 1:25, fe = rnorm(25))
dat <- expand.grid(unit = 1:25, time = 1:50)
dat <- merge(dat, fe, by = "unit")
dat$x <- rnorm(nrow(dat)) + dat$fe
dat$w <- rnorm(nrow(dat))
dat$y <- dat$x + dat$w + dat$x * dat$w + dat$fe + rnorm(nrow(dat), sd = 10)
dat <<- dat
dat2 <- dat
dat2$unit <- as.factor(dat2$unit)
dat2 <<- dat2
mod1 <- feols(y ~ x * w | unit, data = dat)
mod2 <- fixest::feols(y ~ x * w | unit, data = dat2)
expect_warning(plot_cme(mod1, effect = "x", condition = "w", draw = FALSE))
p <- plot_cme(mod2, effect = "x", condition = "w")
expect_inherits(p, "ggplot")



# plot_cme: extracts all required data
fe <- data.frame(unit = 1:25, fe = rnorm(25))
dat <- expand.grid(unit = 1:25, time = 1:50)
dat <- merge(dat, fe, by = "unit")
dat$x <- rnorm(nrow(dat)) + dat$fe
dat$w <- rnorm(nrow(dat))
dat$y <- dat$x + dat$w + dat$x * dat$w + dat$fe + rnorm(nrow(dat), sd = 10)
dat <<- dat
mod1 <- fixest::feols(y ~ x * w | unit, data = dat)
dat2 <- dat
dat2$unit <- as.factor(dat2$unit)
dat2 <<- dat2
mod2 <- fixest::feols(y ~ x * w | unit, data = dat2)
k <- plot_cme(mod2, effect = "x", condition = "w", draw = FALSE)
expect_inherits(k, "data.frame")
expect_false(anyNA(k$dydx))
expect_false(any(k$dydx == 0))



# predictions: bugs stay dead: Issue #203
dat <- mtcars
dat$factor_am = factor(dat$am)
dat <<- dat
m1 <- feols(mpg ~ hp * am, data = dat)
m2 <- feols(mpg ~ hp * factor_am, data = dat)
m3 <- feols(mpg ~ hp * wt, data = dat)
m4 <- feols(mpg ~ i(am, hp), data = dat)
m5 <- feglm(am ~ hp | gear, data = dat)
pred1 <- predictions(m1)
pred2 <- predictions(m2)
pred3 <- predictions(m3)
pred4 <- predictions(m4)
pred5 <- predictions(m5)
expect_predictions(pred1)
expect_predictions(pred2)
expect_predictions(pred3)
expect_predictions(pred4)
expect_predictions(pred5)
# vdiffr::expect_doppelganger("fixest plot_cap with i()",
#                         plot_cap(m4, condition = c("hp", "am")))



# bug stay dead: insight::get_data doesn't get all columns
reg <- feols(
Sepal.Width ~ Petal.Length | Species | Sepal.Length ~ Petal.Width, 
data = iris)
mfx1 <- marginaleffects(reg, newdata = iris)
mfx2 <- marginaleffects(reg)
expect_inherits(mfx1, "marginaleffects")
expect_inherits(mfx2, "marginaleffects")


# bug stays dead
dt <- mtcars
dt$cyl <- factor(dt$cyl)
fit1 <- suppressMessages(feols(mpg ~ 0 | carb | vs ~ am, data = dt))
fit2 <- suppressMessages(feols(mpg ~ cyl | carb | vs ~ am, data = dt))
fit3 <- suppressMessages(feols(mpg ~ 0 | carb | vs:cyl ~ am:cyl, data = dt))
mfx1 <- marginaleffects(fit1)
mfx2 <- marginaleffects(fit2)
mfx3 <- marginaleffects(fit3)
expect_inherits(mfx1, "marginaleffects")
expect_inherits(mfx2, "marginaleffects")
expect_inherits(mfx3, "marginaleffects")

# TODO: works interactively
# expect_false(expect_warning(marginaleffects(fit3)))

# # feols linear plot_cap includes confidence intervals
# mod <- feols(mpg ~ hp, data = mtcars)
# p <- plot_cap(mod, condition = "hp", conf.level = .5)
# vdiffr::expect_doppelganger("plot_cap: feols small conf.level", p)
# p <- plot_cap(mod, condition = "hp", conf.level = .99)
# vdiffr::expect_doppelganger("plot_cap: feols large conf.level", p)



# works interactively
# regression test Issue #232: namespace collision with `rep()`
# can't override global binding for `rep()`
# skip("works interactively") 
# rep <- data.frame(Y = runif(100) > .5, X = rnorm(100))
# mod <- feglm(Y ~ X, data = rep, family = binomial)
# mfx <- marginaleffects(mod)
# expect_inherits(mfx, "marginaleffects")
#


# Issue #229
#skip_if_not_installed("fixest", minimum_version = "0.10.5")
# data(trade)
# dat <<- trade
# mod <- feNmlm(Euros ~ log(dist_km) | Product, data = dat)
# expect_marginaleffects(mod)


