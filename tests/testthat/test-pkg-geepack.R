skip_if_not_installed("geepack")

test_that("geepack::geeglm: no validity", {
    data(dietox, package = "geepack")
    library("geepack")
    dietox$Cu <- as.factor(dietox$Cu)
    mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
    model <- suppressWarnings(geeglm(mf, data=dietox, id=Pig, 
                                     family=poisson("identity"), corstr="ar1"))
    mfx <- suppressMessages(marginaleffects(model))
    expect_s3_class(mfx, "data.frame")
    expect_false(any(mfx$dydx == 0 |  is.na(mfx$dydx)))
    expect_false(any(mfx$std.error == 0 |  is.na(mfx$std.error)))
})
