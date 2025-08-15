skip("need to fix this")

test_that("warnings work correctly", {
    withr::local_options(list(marginaleffects_safe = TRUE))
    # factor in formula
    mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
    expect_warning(slopes(mod))

    # Issue #1447: invlink(link) hypothesis scale
    dat <- transform(mtcars, cyl = factor(cyl))
    mod <- glm(am ~ hp + cyl, data = dat, family = binomial)
    expect_warning(predictions(mod, hypothesis = 3), regexp = "unless")
})
