skip_if_offline()

test_that("JSS data is accessible at hosted links", {
    # make sure that the JSS data is always hosted at the same link forever
    dat <- read.csv("https://marginaleffects.com/data/titanic.csv")
    expect_s3_class(dat, "data.frame")

    dat <- read.csv("https://marginaleffects.com/data/impartiality.csv")
    expect_s3_class(dat, "data.frame")
})
