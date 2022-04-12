requiet("robust")

test_that("no validity", {
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    mod <- lmRob(mpg ~ hp + cyl, data = mtcars)
    expect_marginaleffects(mod, n_unique = 1)
    expect_predictions(predictions(mod))
})
