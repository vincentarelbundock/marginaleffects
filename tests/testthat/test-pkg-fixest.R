requiet("fixest")

test_that("bugs stay dead: logit with transformations", {
    skip("works interactively")
    dat <- mtcars
    dat$gear <- as.factor(dat$gear)
    mod1 <- suppressMessages(feglm(am ~ mpg + mpg^2 | gear, family = binomial(link = "logit"), data = dat, warn = FALSE))
    mod2 <- suppressMessages(feglm(am ~ mpg | gear, family = binomial(link = "logit"), data = dat, warn = FALSE))
    mod3 <- suppressMessages(feglm(am ~ mpg + mpg^2 | gear, family = binomial(link = "logit"), data = mtcars, warn = FALSE))
    mod4 <- suppressMessages(feglm(am ~ mpg | gear, family = binomial(link = "logit"), data = mtcars, warn = FALSE))

    skip_if_not_installed("fixest", minimum_version = "0.10.2")
    expect_s3_class(insight::get_data(mod1), "data.frame")
    expect_s3_class(insight::get_data(mod2), "data.frame")
    expect_s3_class(insight::get_data(mod3), "data.frame")
    expect_s3_class(insight::get_data(mod4), "data.frame")

    expect_marginaleffects(mod1, pct_na = 62.5)
    expect_marginaleffects(mod2, pct_na = 62.5)
    expect_marginaleffects(mod3, pct_na = 62.5)
    expect_marginaleffects(mod4, pct_na = 62.5)

    mfx <- marginaleffects(mod1, variables = "mpg")
    expect_s3_class(mfx, "marginaleffects")
    expect_equal(sum(is.na(mfx$dydx)), 20)
    expect_equal(sum(is.na(mfx$std.error)), 20)
})


test_that("fixest::feols vs. Stata", {
    requiet("plm")
    data(EmplUK, package = "plm")
    stata <- readRDS(test_path("stata/stata.rds"))$fixest_feols
    model <- feols(wage ~ capital * output | firm, EmplUK)
    mfx <- merge(tidy(marginaleffects(model)), stata)
    expect_marginaleffects(model)
    expect_equal(mfx$estimate, mfx$dydx)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .00001)
})

test_that("fixest::fepois vs. Stata", {
    requiet("plm")
    data(EmplUK, package = "plm")
    stata <- readRDS(test_path("stata/stata.rds"))$fixest_fepois
    model <- fepois(log(wage) ~ capital * output | firm, EmplUK)
    mfx <- merge(tidy(marginaleffects(model, type = "link")), stata)
    expect_marginaleffects(model)
    expect_equal(mfx$estimate, mfx$dydx, tolerance = .000001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
})

test_that("fixest::feols: predictions", {
    skip_if_not_installed("insight", minimum_version = "0.14.4.1")
    data(trade, package = "fixest")
    model <- feols(Euros ~ dist_km | Destination + Origin, data = trade)
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(trade))
    expect_predictions(pred1)
    expect_predictions(pred2, n_row = 6)
})

test_that("numeric cluster variable raises warning", {
    skip("works interactively")
    fe <- data.frame(unit = 1:25, fe = rnorm(25))
    dat <- expand.grid(unit = 1:25, time = 1:50)
    dat <- merge(dat, fe, by = "unit")
    dat$x <- rnorm(nrow(dat)) + dat$fe
    dat$w <- rnorm(nrow(dat))
    dat$y <- dat$x + dat$w + dat$x * dat$w + dat$fe + rnorm(nrow(dat), sd = 10)
    mod1 <- feols(y ~ x * w | unit, data = dat)
    dat2 <- dat
    dat2$unit <- as.factor(dat2$unit)
    mod2 <- fixest::feols(y ~ x * w | unit, data = dat2)
    expect_warning(plot_cme(mod1, effect = "x", condition = "w", draw = FALSE))
    expect_warning(plot_cme(mod2, effect = "x", condition = "w", draw = FALSE), NA)
})


test_that("plot_cme: extracts all required data", {
    skip("works interactively")
    fe <- data.frame(unit = 1:25, fe = rnorm(25))
    dat <- expand.grid(unit = 1:25, time = 1:50)
    dat <- merge(dat, fe, by = "unit")
    dat$x <- rnorm(nrow(dat)) + dat$fe
    dat$w <- rnorm(nrow(dat))
    dat$y <- dat$x + dat$w + dat$x * dat$w + dat$fe + rnorm(nrow(dat), sd = 10)
    mod1 <- fixest::feols(y ~ x * w | unit, data = dat)
    dat2 <- dat
    dat2$unit <- as.factor(dat2$unit)
    mod2 <- fixest::feols(y ~ x * w | unit, data = dat2)
    k <- plot_cme(mod2, effect = "x", condition = "w", draw = FALSE)
    expect_s3_class(k, "data.frame")
    expect_false(anyNA(k$dydx))
    expect_false(any(k$dydx == 0))
})


test_that("predictions: bugs stay dead: Issue #203", {
    skip("works interactively")
    dat <- mtcars
    dat$factor_am = factor(dat$am)
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
    expect_warning(plot_cap(m1, condition = c("hp", "am")), NA)
    expect_warning(plot_cap(m2, condition = c("hp", "factor_am")), NA)
    expect_warning(plot_cap(m3, condition = c("hp", "wt")), NA)
    expect_warning(plot_cap(m4, condition = c("hp", "am")), NA)
    vdiffr::expect_doppelganger("fixest plot_cap with i()",
                                plot_cap(m4, condition = c("hp", "am")))
})


test_that("bug stay dead: insight::get_data doesn't get all columns", {
    skip_if_not_installed("insight", minimum_version = "0.15.1")
    reg <- feols(
        Sepal.Width ~ Petal.Length | Species | Sepal.Length ~ Petal.Width, 
        data = iris)
    mfx1 <- marginaleffects(reg, newdata = iris)
    mfx2 <- marginaleffects(reg)
    expect_s3_class(mfx1, "marginaleffects")
    expect_s3_class(mfx2, "marginaleffects")
})


test_that("feols linear plot_cap includes confidence intervals", {
    mod <- feols(mpg ~ hp, data = mtcars)
    p <- plot_cap(mod, condition = "hp", conf.level = .5)
    vdiffr::expect_doppelganger("plot_cap: feols small conf.level", p)
    p <- plot_cap(mod, condition = "hp", conf.level = .99)
    vdiffr::expect_doppelganger("plot_cap: feols large conf.level", p)
})
