test_that("integer get contrasts", {
    dat <- mtcars
    dat$gear <- as.integer(dat$gear)
    m1 <- lm(mpg ~ hp + cyl + gear, data = dat)
    m2 <- lm(mpg ~ hp + cyl, data = dat)
    t1 <- tidy(marginaleffects(m1))
    t2 <- tidy(marginaleffects(m2))
    expect_true("contrast" %in% colnames(t1))
    expect_false("contrast" %in% colnames(t2))
    expect_equal(t1$contrast, c("dY/dX", "dY/dX", "+1"))
})
