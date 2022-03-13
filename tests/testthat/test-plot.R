skip_if(getRversion() < "4.1.0") # different graphics engines
skip_on_cran()

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

test_that("plot(mfx): no CI", {
    mod <- glm(am ~ hp + wt, data = mtcars)
    mfx <- marginaleffects(mod, vcov = FALSE)
    p <- plot(mfx)
    vdiffr::expect_doppelganger("plot no CI", p)
})

test_that("bugfix: contrasts overlap", {
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    mod <- lm(mpg ~ hp + cyl, data = dat)
    mfx <- marginaleffects(mod)
    p <- plot(mfx)
    vdiffr::expect_doppelganger("plot contrast overlap", p)
})


