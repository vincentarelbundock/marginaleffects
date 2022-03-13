test_that("find_variable_class: unsupported contrast", {
    dat <- mtcars
    dat$am <- complex(dat$am)
    expect_error(marginaleffects:::find_variable_class(
        newdata = dat,
        variable = "am"))
})


test_that("mean_or_mode", {
    x <- factor(c("a", "a", "b"))
    expect_equal(mean_or_mode(x), x[1])
    expect_equal(mean_or_mode(c("a", "a", "b")), "a")
    expect_equal(mean_or_mode(1:3), 2)
    expect_equal(mean_or_mode(c(FALSE, FALSE, TRUE)), FALSE)
    expect_equal(mean_or_mode(data.frame(x = 1:3))$x, 2)
})
