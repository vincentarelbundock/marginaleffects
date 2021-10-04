skip_if(getRversion() < 4.0)
skip_on_ci() # tolerance on p-value rounding is weirdly off


test_that("simple summary output", {
    mod <- lm(mpg ~ hp + factor(cyl), mtcars)
    mfx <- marginaleffects(mod)
    s <- summary(mfx)
    expect_snapshot(print(summary(mfx), digits = 3))
})


test_that("summary conf.level", {
    mod <- lm(mpg ~ hp + factor(cyl), mtcars)
    mfx <- marginaleffects(mod)
    expect_snapshot(print(summary(mfx, conf.level = .9)))
    expect_snapshot(print(summary(mfx, conf.level = .2)))
})


test_that("summary: marginal means", {
    dat <- mtcars
    dat$am <- as.logical(dat$am)
    dat$vs <- as.logical(dat$vs)
    dat$gear <- as.factor(dat$gear)
    mod <- lm(mpg ~ gear + am + vs, dat)
    mm <- marginalmeans(mod)
    s <- summary(mm)
    expect_snapshot(print(s))
})
