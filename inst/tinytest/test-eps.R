source("helpers.R")
using("marginaleffects")

# eps argument affects results as expected
mod <- glm(vs ~ mpg + hp, data = mtcars, family = binomial)
nd <- datagrid(model = mod)
cmp0 <- slopes(mod, variables = "mpg", newdata = nd)
cmp1 <- slopes(mod, variables = "mpg", newdata = nd, eps = 1)
expect_true(all(cmp0$estimate != cmp1$estimate))

# adaptive eps should matter for logit but not ols
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
expect_equivalent(m1$estimate, m2$estimate)
expect_equivalent(m1$estimate, m3$estimate)
expect_equivalent(m2$estimate, m3$estimate)

# errors and warnings
expect_error(slopes(mod, eps = 0))

rm(list = ls())