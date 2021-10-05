skip_if_not_installed("geepack")
skip_if_not_installed("emmeans")
skip_if_not_installed("broom")
requiet("geepack")
requiet("emmeans")
requiet("broom")

# Stata does not replicate coefficients exactly:
# xtset Pig Time
# xtgee Weight i.Cu, family(poisson) link(identity) corr(ar 1)

test_that("marginaleffects: geepack::geeglm: no validity", {
    data(dietox, package = "geepack")
    dietox$Cu <- as.factor(dietox$Cu)
    mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
    model <- suppressWarnings(geeglm(mf, data=dietox, id=Pig, 
                                     family=poisson("identity"), corstr="ar1"))
    expect_marginaleffects(model)
})

test_that("predictions: geepack::geeglm: no validity", {
    data(dietox, package = "geepack")
    dietox$Cu <- as.factor(dietox$Cu)
    mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
    model <- suppressWarnings(geeglm(mf, data=dietox, id=Pig, 
                                     family=poisson("identity"), corstr="ar1"))
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(dietox))
    expect_predictions(pred1, n_row = 1)
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
    expect_marginalmeans(mm, n_row = 3, se = FALSE)
    ti <- tidy(mm)
    em <- broom::tidy(emmeans::emmeans(model, "Cu"))
    expect_equal(ti$estimate, em$estimate)
})
