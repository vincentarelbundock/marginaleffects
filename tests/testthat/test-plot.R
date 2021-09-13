# I think there are different graphics engines. Do this locally.
skip_on_ci()

test_that("plot_cme(mod, 'hp', 'wt')", {
    mod <- lm(mpg ~ hp * wt, data = mtcars)
    p <- plot_cme(mod, effect = "hp", condition = "wt")
    vdiffr::expect_doppelganger("plot_cme basic", p)
})

test_that("plot(mfx)", {
    mod <- glm(am ~ hp + wt, data = mtcars)
    mfx <- marginaleffects(mod)
    p <- plot(mfx)
    vdiffr::expect_doppelganger("plot basic", p)
})

