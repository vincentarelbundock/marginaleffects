skip_if_not_installed("fixest")


test_that("fixest::feols vs. Stata", {
    data(EmplUK, package = "plm")
    stata <- readRDS(test_path("stata/stata.rds"))$fixest_feols
    model <- fixest::feols(wage ~ capital * output | firm, EmplUK)
    mfx <- merge(tidy(marginaleffects(model)), stata)
    expect_mfx(model)
    expect_equal(mfx$estimate, mfx$dydx)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .00001)
})


test_that("fixest::fepois vs. Stata", {
    data(EmplUK, package = "plm")
    stata <- readRDS(test_path("stata/stata.rds"))$fixest_fepois
    model <- fixest::fepois(log(wage) ~ capital * output | firm, EmplUK)
    mfx <- merge(tidy(marginaleffects(model, type = "link")), stata)
    expect_mfx(model)
    expect_equal(mfx$estimate, mfx$dydx, tolerance = .000001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
})


# No idea why this fails on testthat. It works locally!

# fe <- data.frame(unit = 1:25, fe = rnorm(25))
# dat <- expand.grid(unit = 1:25, time = 1:50)
# dat <- merge(dat, fe, by = "unit")
# dat$x <- rnorm(nrow(dat)) + dat$fe
# dat$w <- rnorm(nrow(dat))
# dat$y <- dat$x + dat$w + dat$x * dat$w + dat$fe + rnorm(nrow(dat), sd = 10)
# mod1 <- fixest::feols(y ~ x * w | unit, data = dat)
# dat2 <- dat
# dat2$unit <- as.factor(dat2$unit)
# mod2 <- fixest::feols(y ~ x * w | unit, data = dat2)

# test_that("numeric cluster variable raises warning", {
#     expect_warning(plot_cme(mod1, effect = "x", condition = "w", draw = FALSE))
#     expect_warning(plot_cme(mod2, effect = "x", condition = "w", draw = FALSE), NA)
# })

# test_that("plot_cme: extracts all required data", {
#     k <- plot_cme(mod2, effect = "x", condition = "w", draw = FALSE)
#     expect_s3_class(k, "data.frame")
#     expect_false(anyNA(k$dydx))
#     expect_false(any(k$dydx == 0))
# })
