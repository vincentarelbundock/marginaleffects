requiet("sandwich")

test_that("working but no validity check", {
    mod <- lm(mpg ~ hp + drat, mtcars)
    a <- tidy(marginaleffects(mod))
    b <- tidy(marginaleffects(mod, vcov = sandwich::vcovHC(mod)))
    expect_true(all(a$estimate == b$estimate))
    expect_true(all(a$std.error != b$std.error))
})
