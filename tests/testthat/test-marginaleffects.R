test_that("marginal effects at the mean", {
    mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
    mfx1 <- marginaleffects(mod, newdata = datagrid())
    mfx2 <- marginaleffects(mod, newdata = "mean")
    expect_equal(mfx1, mfx2, ignore_attr = TRUE)
})
