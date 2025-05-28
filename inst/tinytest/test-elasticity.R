source("helpers.R")
using("marginaleffects")
set.seed(1024)

tol <- tolse <- 1e-3

results <- readRDS(testing_path("stata/stata.rds"))

# lm()
dat <- read.csv(testing_path("stata/databases/stats_lm_01.csv"))
mod <- lm(y ~ x1 * x2, data = dat)

mfx <- slopes(mod, slope = "eyex")
expect_inherits(mfx, "slopes")
mfx <- avg_slopes(mod, slope = "eyex")
sta <- results$stats_lm_elasticity_eyex
expect_equivalent(mfx$estimate, sta$dydxstata)


expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)
mfx <- avg_slopes(mod, slope = "eydx", numderiv = "richardson")
sta <- results$stats_lm_elasticity_eydx
expect_equivalent(mfx$estimate, sta$dydxstata)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

mfx <- avg_slopes(mod, slope = "dyex", numderiv = "richardson")
sta <- results$stats_lm_elasticity_dyex
expect_equivalent(mfx$estimate, sta$dydxstata)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

# glm()
dat <- read.csv(testing_path("stata/databases/stats_glm_01.csv"))
mod <- glm(y ~ x1 * x2, data = dat, family = binomial)

mfx <- avg_slopes(mod, slope = "eyex", numderiv = "richardson")
sta <- results$stats_glm_elasticity_eyex
expect_equivalent(mfx$estimate, sta$dydxstata, tolerance = tol)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

mfx <- avg_slopes(mod, slope = "eydx", numderiv = "richardson")
sta <- results$stats_glm_elasticity_eydx
expect_equivalent(mfx$estimate, sta$dydxstata, tolerance = tol)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)

mfx <- avg_slopes(mod, slope = "dyex", numderiv = "richardson")
sta <- results$stats_glm_elasticity_dyex
expect_equivalent(mfx$estimate, sta$dydxstata, tolerance = 1e-2)
expect_equivalent(mfx$std.error, sta$std.errorstata, tolerance = tolse)


# Manual computation
fit <- lm(formula = mpg ~ cyl * hp + wt, data = mtcars)
p <- predict(fit)
x <- mtcars$wt

dydx <- slopes(fit, variables = "wt")$estimate

eyex_a <- dydx * x / p
eyex_b <- slopes(fit, variables = "wt", slope = "eyex")$estimate
expect_equivalent(eyex_a, eyex_b)

eydx_a <- dydx / p
eydx_b <- slopes(fit, variables = "wt", slope = "eydx")$estimate
expect_equivalent(eydx_a, eydx_b)

dyex_a <- dydx * x
dyex_b <- slopes(fit, variables = "wt", slope = "dyex")$estimate
expect_equivalent(dyex_a, dyex_b)


# Issue #1113: avg_slopes(slope = "eyex") should skip dedup
requiet("nnet")
dat <- structure(
    list(
        y_region2 = c(
            1,
            1,
            1,
            2,
            2,
            2,
            2,
            2,
            1,
            2,
            2,
            2,
            1,
            2,
            2,
            2,
            2,
            2,
            2,
            1,
            2,
            2,
            2,
            1,
            2,
            1,
            2,
            2,
            3,
            3,
            3,
            3,
            3,
            3,
            1,
            3,
            3,
            3,
            1,
            3,
            3,
            1,
            3,
            1,
            3,
            3,
            3,
            1,
            3,
            3
        ),
        male = c(
            0,
            1,
            1,
            1,
            0,
            0,
            0,
            1,
            0,
            1,
            0,
            1,
            1,
            0,
            0,
            0,
            1,
            1,
            0,
            1,
            0,
            0,
            1,
            1,
            0,
            0,
            1,
            1,
            1,
            1,
            1,
            0,
            0,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            0,
            0,
            1,
            0,
            0,
            1
        ),
        frenchlanguage = c(
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            0,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            0,
            0,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1,
            1
        )
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(
        NA,
        -50L
    )
)
dat$male = dat$male + 1e-6
mod <- nnet::multinom(y_region2 ~ male + frenchlanguage, data = dat, Hess = TRUE, trace = FALSE)
s1 <- avg_slopes(mod, slope = "dydx")
s2 <- avg_slopes(mod, slope = "dyex")
s3 <- avg_slopes(mod, slope = "eyex")
s4 <- avg_slopes(mod, slope = "eydx")
expect_inherits(s1, "slopes")
expect_inherits(s2, "slopes")
expect_inherits(s3, "slopes")
expect_inherits(s4, "slopes")
