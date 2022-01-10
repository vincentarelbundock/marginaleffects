skip_if_not_installed("numDeriv")

test_that("numDeriv global options", {
    mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)

    m1 <- marginaleffects(mod)
    options(marginaleffects_numDeriv = list("method" = "simple"))
    m2 <- marginaleffects(mod)
    options(marginaleffects_numDeriv = list("method" = "Richardson"))
    m3 <- marginaleffects(mod)
    options(marginaleffects_numDeriv = NULL)

    # close but not exactly equal
    expect_true(all(m1$dydx == m2$dydx))
    expect_true(all(m1$dydx != m3$dydx))
    expect_equal(m1$dydx, m3$dydx, tolerance = .1)
    expect_true(all(m1$std.error == m2$std.error))
    expect_true(all(m1$std.error != m3$std.error))
    expect_equal(m1$std.error, m3$std.error, tolerance = .1)
})
