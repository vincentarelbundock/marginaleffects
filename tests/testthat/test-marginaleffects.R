test_that("marginal effects at the mean work correctly", {
    mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
    mfx1 <- slopes(mod, newdata = datagrid())
    mfx2 <- slopes(mod, newdata = "mean")
    expect_equal(mfx1, mfx2, ignore_attr = TRUE)
})


test_that("unsupported arguments produce appropriate errors", {
    mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
    expect_error(slopes(mod, comparison = mean), regexp = "supported")
    expect_error(slopes(mod, transform = exp), regexp = "supported")
    expect_error(slopes(mod, cross = TRUE), regexp = "supported")
})
