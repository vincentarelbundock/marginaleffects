requiet("geepack")
requiet("emmeans")
requiet("broom")

# Stata does not replicate coefficients exactly:
# xtset Pig Time
# xtgee Weight i.Cu, family(poisson) link(identity) corr(ar 1)

test_that("geepack::geeglm: marginaleffects vs. emtrends", {
    data(dietox, package = "geepack")
    dietox$Cu <- as.factor(dietox$Cu)
    mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
    model <- suppressWarnings(geeglm(mf, data=dietox, id=Pig, 
                                     family=poisson("identity"), corstr="ar1"))
    expect_marginaleffects(model)
    # emmeans
    mfx <- marginaleffects(model, variables = "Time", newdata = datagrid(Time = 10, Cu = "Cu000"), type = "link")
    em <- emtrends(model, ~Time, var = "Time", at = list(Time = 10, Cu = "Cu000"))
    em <- tidy(em)
    expect_equal(mfx$dydx, em$Time.trend, tolerance = .001)
    expect_equal(mfx$std.error, em$std.error, tolerance = .01)
})

test_that("predictions: geepack::geeglm: no validity", {
    data(dietox, package = "geepack")
    dietox$Cu <- as.factor(dietox$Cu)
    mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
    model <- suppressWarnings(geeglm(mf, data=dietox, id=Pig, 
                                     family=poisson("identity"), corstr="ar1"))
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(dietox))
    expect_predictions(pred1, n_row = nrow(dietox))
    expect_predictions(pred2, n_row = 6)
})

# TODO: why no support for standard errors?
test_that("marginalmeans: geepack::geeglm: vs. emmeans", {
    data(dietox, package = "geepack")
    dietox$Cu <- as.factor(dietox$Cu)
    mf <- formula(Weight ~ Cu + Time + I(Time^2) + I(Time^3))
    model <- suppressWarnings(geeglm(mf, data=dietox, id=Pig, 
                                     family=poisson("identity"), corstr="ar1"))
    mm <- marginalmeans(model, variables = "Cu")
    expect_marginalmeans(mm, n_row = 3)
    mm <- tidy(mm)
    em <- broom::tidy(emmeans::emmeans(model, "Cu"))
    expect_equal(mm$estimate, em$estimate)
    expect_equal(mm$std.error, em$std.error)
})
