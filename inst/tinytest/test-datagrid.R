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



# Issue #721
requiet("haven")
m <- marginaleffects:::hush(read_dta("http://www.stata-press.com/data/r15/margex.dta"))
if (inherits(m, "data.frame")) {
    m <- data.frame(m)
    m$sex <- as.factor(m$sex)
    mod <- lm(y ~ sex + age + distance, data = m)
    expect_error(
        predictions(mod, newdata = datagrid(sex = c("male", "female"))),
        pattern = "must be one of the factor levels"
    )
    expect_error(
        predictions(mod, newdata = datagrid(sex = c("male", "femael"))),
        pattern = "must be one of the factor levels"
    )
}
mod <- lm(mpg ~ qsec + as.factor(gear), data = mtcars)
expect_error(
    predictions(mod, newdata = datagrid(gear = 6)),
    pattern = "must be one of the factor levels"
)
expect_error(
    comparisons(mod, newdata = datagrid(gear = 6)),
    pattern = "must be one of the factor levels"
)



# Issue #688
dat <<- transform(mtcars, cyl = factor(cyl))
mod <- lm(mpg ~ hp, data = dat)
d <- datagrid(model = mod, by = c("carb", "cyl"))
expect_equivalent(nrow(d), 9)




source("helpers.R")
rm(list = ls())