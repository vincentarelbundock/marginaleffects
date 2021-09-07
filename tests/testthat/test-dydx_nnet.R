skip_if_not_installed("nnet")

library("margins")

test_that("multinom", {
    tmp <- mtcars
    tmp$cyl <- as.factor(tmp$cyl)
    void <- capture.output( mod <- 
        nnet::multinom(cyl ~ hp + am + mpg, data = tmp, quiet = true))

    res <- mfx(mod, group_names = c("4", "6"), variance = NULL)
    expect_s3_class(res, "data.frame")

    # TODO: `margins` appears to break with numeric regressors but not factors
    # here it's the opposite: we don't support factors. Fix this and compare results.
    expect_error(margins(mod))
})
