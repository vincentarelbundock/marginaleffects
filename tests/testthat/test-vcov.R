skip_if_not_installed("sandwich")

test_that("marginal effects at the 'typical()' value when vcov=FALSE", {
    mod <- lm(hp ~ mpg + drat, data = mtcars)
    res <- tidy(marginaleffects(mod, vcov = FALSE))
    expect_s3_class(res, "data.frame")
    expect_true(all(res$std.error > 0))
})


test_that("working but no validity check", {
    mod <- lm(mpg ~ hp + drat, mtcars)
    a <- tidy(marginaleffects(mod))
    b <- tidy(marginaleffects(mod, vcov = sandwich::vcovHC(mod)))
    expect_true(all(a$estimate == b$estimate))
    expect_true(all(a$std.error != b$std.error))
})
