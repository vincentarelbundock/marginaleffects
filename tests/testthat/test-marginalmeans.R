test_that("temporary errors until implementation", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    mod <- lm(mpg ~ hp + wt + factor(cyl) + am, data = tmp)
    mm <- marginalmeans(mod, variables = c("am", "cyl"))
    expect_error(marginalmeans(mod, newdata = mtcars), regexp = "not yet")
    expect_error(marginalmeans(mod), regexp = "Please supply")
})
