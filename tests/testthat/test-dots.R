test_that("lme4::lmer", {
    requiet("lme4")
    mod <- lmer(mpg ~ hp + (1 | gear), data = mtcars)
    expect_error(marginaleffects(mod), NA)
    expect_warning(marginaleffects(mod, blah = 2), regexp = "Valid.*Github")
})


test_that("stats::lm", {
    mod <- lm(mpg ~ hp, data = mtcars)
    expect_error(marginaleffects(mod), NA)
    expect_warning(marginaleffects(mod, blah = 2), regexp = "Github")
})

