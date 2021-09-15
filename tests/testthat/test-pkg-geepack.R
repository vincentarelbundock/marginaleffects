skip_if_not_installed("geepack")

test_that("geepack::geeglm: no validity", {
    data(dietox, package = "geepack")
    library("geepack")
    dietox$Cu <- as.factor(dietox$Cu)
    mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
    model <- suppressWarnings(geeglm(mf, data=dietox, id=Pig, 
                                     family=poisson("identity"), corstr="ar1"))
    expect_mfx(model)
})
