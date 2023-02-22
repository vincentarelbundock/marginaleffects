source("helpers.R")
using("marginaleffects")

requiet("lme4")
requiet("fixest")


# informative errors
expect_error(datagrid(Petal.Length = 4.6), pattern = "inside")

# numeric clusters no longer produce a warning; selects mode
mod <-lme4::lmer(mpg ~ hp + (1 + drat | cyl), data = mtcars)
expect_true(datagrid(model = mod)$cyl == 8)

# functions
cmp <- comparisons(
    mod,
    newdata = datagrid(hp = range, cyl = unique))
expect_equivalent(nrow(cmp), 6)

cmp <- comparisons(
    mod,
    newdata = datagrid(hp = range))
expect_equivalent(nrow(cmp), 2)

p <- predictions(
    mod,
    newdata = datagrid(hp = fivenum))
expect_equivalent(nrow(p), 5)

nd <- datagrid(newdata = mtcars, hp = range, mpg = fivenum, wt = sd)
expect_equivalent(nrow(nd), 10)

mod <- glm(am ~ factor(gear), data = mtcars)
cmp <- comparisons(mod, newdata = datagrid(am = 0, gear = mtcars$gear))
expect_equivalent(nrow(cmp), 6)
cmp <- comparisons(mod, newdata = datagrid(am = unique, gear = max))
expect_equivalent(nrow(cmp), 4)



source("helpers.R")
rm(list = ls())