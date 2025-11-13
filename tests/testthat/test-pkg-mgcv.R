testthat::skip_if(!EXPENSIVE, "EXPENSIVE")

# exit_file("environment?")

testthat::skip_if_not_installed("mgcv")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")
testthat::skip_if_not_installed("tibble")
testthat::skip_if_not_installed("tsModel")

requiet("mgcv")
requiet("emmeans")
requiet("broom")
requiet("tibble")
requiet("tsModel")

# Basic expectation tests
mod_simple <- mgcv::gam(mpg ~ s(wt), data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# marginaleffects vs. emtrends
set.seed(2)
void <- capture.output(dat_mgcv <- gamSim(1, n = 400, dist = "normal", scale = 2))
void <- capture.output(dat2_mgcv <- gamSim(1, n = 2000, dist = "poisson", scale = .1))
dat_mgcv <- dat_mgcv
dat2_mgcv <- dat2_mgcv
m1 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat_mgcv)
m2 <- mgcv::gam(y ~ te(x0, x1, k = 7) + s(x2) + s(x3), data = dat_mgcv, method = "REML")
m3 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3) + ti(x1, x2, k = 6), data = dat_mgcv, method = "REML")
m4 <- mgcv::gam(y ~ s(x0, x1, k = 40) + s(x2) + s(x3), data = dat_mgcv, method = "REML")
m5 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat_mgcv, method = "REML", select = TRUE)
m6 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), sp = c(0.01, -1, -1, -1), data = dat_mgcv)
m7 <- mgcv::gam(y ~ s(x0, sp = .01) + s(x1) + s(x2) + s(x3), data = dat_mgcv)
m8 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), min.sp = c(0.001, 0.01, 0, 10), data = dat_mgcv)
m9 <- mgcv::gam(
    y ~ s(x0, bs = "cr") + s(x1, bs = "cr") + s(x2, bs = "cr") + s(x3, bs = "cr"),
    family = poisson,
    data = dat2_mgcv,
    method = "REML"
)
expect_slopes2(m1)
expect_slopes2(m2)
expect_slopes2(m3)
expect_slopes2(m4)
expect_slopes2(m5)
expect_slopes2(m6)
expect_slopes2(m7)
expect_slopes2(m8)
expect_slopes2(m9)


# emtrends: not sure this works anymore
mfx <- slopes(
    m1,
    variables = "x1",
    newdata = datagrid(
        x1 = 0,
        x2 = 0,
        x3 = 0,
        FUN_integer = mean
    ),
    type = "link"
)
em <- emtrends(m1, specs = ~x1, var = "x1", at = list(x1 = 0, x2 = 0, x3 = 0))
em <- tidy(em)
expect_equal(mfx$estimate, em$x1.trend, ignore_attr = TRUE)
expect_equal(mfx$std.error, em$std.error, tolerance = .01, ignore_attr = TRUE)


# predictions: no validity
set.seed(2)
void <- capture.output(dat_mgcv2 <- gamSim(1, n = 400, dist = "normal", scale = 2))
dat_mgcv2 <<- dat_mgcv2
mod <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat_mgcv2)
pred1 <- predictions(mod)
pred2 <- predictions(mod, newdata = head(dat_mgcv2))
expect_predictions2(mod, n_row = nrow(dat_mgcv2))
expect_predictions2(mod, newdata = head(dat_mgcv2), n_row = 6)

# Issue #364: predictions confidence intervals for binomial models
void <- capture.output(
    dat_mgcv3 <- suppressMessages(gamSim(1, n = 400, dist = "binary", scale = .33))
)
dat_mgcv3 <<- dat_mgcv3
m <- bam(
    y ~ s(x0) + s(x1) + s(x2) + s(x3),
    family = binomial,
    data = dat_mgcv3,
    method = "REML"
)
p <- predictions(m)
expect_true("conf.low" %in% colnames(p))
expect_true("conf.high" %in% colnames(p))


# Issue #363: matrix column in predictors
test1 <- function(x, z, sx = 0.3, sz = 0.4) {
    x <- x * 20
    (pi**sx * sz) *
        (1.2 * exp(-(x - 0.2)^2 / sx^2 - (z - 0.3)^2 / sz^2) + 0.8 * exp(-(x - 0.7)^2 / sx^2 - (z - 0.8)^2 / sz^2))
}
