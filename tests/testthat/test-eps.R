test_that("eps argument affects results as expected", {
    mod <- glm(vs ~ mpg + hp, data = mtcars, family = binomial)
    nd <- datagrid(model = mod)
    cmp0 <- marginaleffects(mod, variables = "mpg", newdata = nd)
    cmp1 <- marginaleffects(mod, variables = "mpg", newdata = nd, eps = 1)
    cmp2 <- comparisons(mod, newdata = nd, variables = "mpg", contrast_numeric = c(nd$mpg, nd$mpg + 1))
    expect_true(all(cmp1$dydx == cmp2$comparison))
    expect_true(all(cmp0$dydx != cmp1$dydx))
})
