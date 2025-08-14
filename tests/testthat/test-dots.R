skip_if_not_installed("lme4")

test_that("dots work with lme4::lmer", {
    withr::local_options(list(marginaleffects_safe = TRUE))
    mod <- lme4::lmer(mpg ~ hp + (1 | gear), data = mtcars)
    expect_s3_class(suppressWarnings(slopes(mod)), "marginaleffects")

    expect_warning(
        expect_warning(
            slopes(mod, blah = 2), "fixed-effect"), "blah")
})


test_that("dots work with stats::lm", {
    withr::local_options(list(marginaleffects_safe = TRUE))
    mod <- lm(mpg ~ hp, data = mtcars)
    expect_s3_class(slopes(mod), "marginaleffects")
    expect_warning(slopes(mod, blah = 2), regexp = "blah")
})


test_that("deprecation: p_adjust produces appropriate error", {
    mod <- lm(mpg ~ factor(cyl), data = mtcars)
    expect_error(avg_comparisons(mod, p_adjust = "holm"), regexp = "inferences")
})
