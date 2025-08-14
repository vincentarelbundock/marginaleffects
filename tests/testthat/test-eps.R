test_that("eps argument affects results as expected", {
    mod <- glm(vs ~ mpg + hp, data = mtcars, family = binomial)
    nd <- datagrid(model = mod)
    cmp0 <- slopes(mod, variables = "mpg", newdata = nd)
    cmp1 <- slopes(mod, variables = "mpg", newdata = nd, eps = 1)
    expect_true(all(cmp0$estimate != cmp1$estimate))
})


test_that("adaptive eps should matter for logit but not ols", {
    mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
    m1 <- slopes(mod, eps = NULL)
    m2 <- slopes(mod, eps = 1)
    m3 <- slopes(mod, eps = 1e-4)
    expect_true(all(m1$estimate != m2$estimate))
    expect_true(all(m1$estimate != m2$estimate))
    expect_true(all(m3$estimate != m2$estimate))

    mod <- lm(am ~ hp + mpg, data = mtcars)
    m1 <- slopes(mod, eps = NULL)
    m2 <- slopes(mod, eps = 1)
    m3 <- slopes(mod, eps = 1e-4)
    expect_equal(m1$estimate, m2$estimate)
    expect_equal(m1$estimate, m3$estimate)
    expect_equal(m2$estimate, m3$estimate)
})


test_that("eps errors and warnings work", {
    mod <- lm(am ~ hp + mpg, data = mtcars)
    expect_error(slopes(mod, eps = 0))
})


test_that("Issue #840: richardson numerical derivative", {
    skip_if_not_installed("causaldata")
    df <- causaldata::restaurant_inspections
    m1 <- glm(Weekend ~ Year, data = df, family = binomial)
    z <- avg_slopes(m1, variables = "Year", numderiv = "richardson")$statistic
    expect_equal(z, -2.0682935630417, tolerance = 1e-5)
})
