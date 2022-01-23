requiet("robust")

test_that("no validity", {
    mod <- lmRob(mpg ~ hp + factor(cyl), data = mtcars)
    expect_error(expect_warning(marginaleffects(mod, type = "response"), regexp = "safer"))
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    mod <- lmRob(mpg ~ hp + cyl, data = dat)
    expect_marginaleffects(mod, n_unique = 1)
    expect_predictions(predictions(mod))
})
