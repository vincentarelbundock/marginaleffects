test_that("marginal effects at the mean", {
    mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
    mfx1 <- marginaleffects(mod, newdata = datagrid())
    mfx2 <- marginaleffects(mod, newdata = "mean")
    expect_equal(mfx1, mfx2, ignore_attr = TRUE)
})


test_that("unsupported arguments", {
    mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
    expect_error(marginaleffects(mod, contrast_numeric = "sd"), regexp = "supported")
    expect_error(marginaleffects(mod, contrast_factor = "pairwise"), regexp = "supported")
    expect_error(marginaleffects(mod, transform_pre = mean), regexp = "supported")
    expect_error(marginaleffects(mod, transform_post = exp), regexp = "supported")
    expect_error(marginaleffects(mod, interaction = TRUE), regexp = "supported")
})
