# exit_file("works interactively")
source("helpers.R", local = TRUE)

# eps argument affects results as expected
mod <- glm(vs ~ mpg + hp, data = mtcars, family = binomial)
nd <- datagrid(model = mod)
cmp0 <- marginaleffects(mod, variables = "mpg", newdata = nd)
cmp1 <- marginaleffects(mod, variables = "mpg", newdata = nd, eps = 1)
cmp2 <- comparisons(mod, newdata = nd, variables = "mpg", contrast_numeric = c(nd$mpg, nd$mpg + 1))
expect_true(all(cmp1$dydx == cmp2$comparison))
expect_true(all(cmp0$dydx != cmp1$dydx))

# adaptive eps should matter for logit but not ols
mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
m1 <- marginaleffects(mod, eps = NULL)
m2 <- marginaleffects(mod, eps = 1)
m3 <- marginaleffects(mod, eps = 1e-4)
expect_true(all(m1$dydx != m2$dydx))
expect_true(all(m1$dydx != m2$dydx))
expect_true(all(m3$dydx != m2$dydx))

mod <- lm(am ~ hp + mpg, data = mtcars)
m1 <- marginaleffects(mod, eps = NULL)
m2 <- marginaleffects(mod, eps = 1)
m3 <- marginaleffects(mod, eps = 1e-4)
expect_equivalent(m1$dydx, m2$dydx)
expect_equivalent(m1$dydx, m3$dydx)
expect_equivalent(m2$dydx, m3$dydx)

# errors and warnings
expect_error(marginaleffects(mod, eps = 0))
