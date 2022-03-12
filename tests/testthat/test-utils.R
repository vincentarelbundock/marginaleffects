test_that("find_variable_class: unsupported contrast", {
    dat <- mtcars
    dat$am <- complex(dat$am)
    expect_error(marginaleffects:::find_variable_class(
        newdata = dat,
        variable = "am"))
})
