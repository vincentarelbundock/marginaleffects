# lme4::lmer
if (requireNamespace("lme4", quietly = TRUE)) {
    mod <- lme4::lmer(mpg ~ hp + (1 | gear), data = mtcars)
    expect_s3_class(slopes(mod), "marginaleffects")
    expect_warning(slopes(mod, blah = 2), regexp = "Github")
}


# stats::lm
mod <- lm(mpg ~ hp, data = mtcars)
expect_s3_class(slopes(mod), "marginaleffects")
expect_warning(slopes(mod, blah = 2), regexp = "Github")


# deprecation: p_adjust
mod <- lm(mpg ~ factor(cyl), data = mtcars)
expect_error(avg_comparisons(mod, p_adjust = "holm"), regexp = "inferences")
