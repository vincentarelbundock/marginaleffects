source("helpers.R")
using("marginaleffects")
tol <- 1e-5

requiet("lme4")
requiet("fixest")


# informative errors
expect_error(datagrid(Petal.Length = 4.6), pattern = "must not be `NULL`")

# numeric clusters no longer produce a warning; selects mode
mod <- lme4::lmer(mpg ~ hp + (1 + drat | cyl), data = mtcars)
expect_true(datagrid(model = mod)$cyl == 8)

# functions
cmp <- comparisons(
    mod,
    newdata = datagrid(hp = range, cyl = unique)
)
expect_equivalent(nrow(cmp), 6, tolerance = tol)

cmp <- comparisons(
    mod,
    newdata = datagrid(hp = range)
)
expect_equivalent(nrow(cmp), 2, tolerance = tol)

p <- predictions(
    mod,
    newdata = datagrid(hp = fivenum)
)
expect_equivalent(nrow(p), 5, tolerance = tol)

nd <- datagrid(newdata = mtcars, hp = range, mpg = fivenum, wt = sd)
expect_equivalent(nrow(nd), 10, tolerance = tol)

mod <- glm(am ~ factor(gear), data = mtcars)
cmp <- comparisons(mod, newdata = datagrid(am = 0, gear = mtcars$gear))
expect_equivalent(nrow(cmp), 6, tolerance = tol)
cmp <- comparisons(mod, newdata = datagrid(am = unique, gear = max))
expect_equivalent(nrow(cmp), 4, tolerance = tol)


# Issue #721
requiet("haven")
m <- marginaleffects:::hush(read_dta(testing_path("modelarchive/data/margex.dta")))
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
d <- datagrid(model = mod, by = c("carb", "cyl"), response = TRUE)
k <- aggregate(cbind(mpg, hp) ~ carb + cyl, data = dat, FUN = mean)
expect_equivalent(k$mpg, d$mpg, tolerance = tol)


# Issue 766: categorical predictors + variables arg + avg
requiet("Matchit")
data("lalonde", package = "MatchIt")
fit <- lm(re78 ~ race * treat, data = lalonde)

a = predict(fit, branewdata = lalonde)
b = predictions(fit, newdata = lalonde)
expect_equivalent(a, b$estimate, tolerance = tol)
nd = rbind(transform(lalonde, treat = 0), transform(lalonde, treat = 1))
a = predict(fit, newdata = nd)
b = predictions(fit, newdata = lalonde, variables = "treat")
expect_equivalent(a, b$estimate, tolerance = tol)

a = tapply(predict(fit, newdata = nd), nd$treat, mean)
b = avg_predictions(fit, newdata = lalonde, variables = "treat") |> suppressWarnings()
expect_equivalent(as.numeric(a), b$estimate, tolerance = tol)

a = predict(fit, newdata = nd)
b = predictions(fit, variables = "treat")
expect_equivalent(a, b$estimate, tolerance = tol)

a = tapply(predict(fit, newdata = nd), nd$treat, mean)
b = avg_predictions(fit, variables = "treat")
expect_equivalent(as.numeric(a), b$estimate, tolerance = tol)

a = tapply(predict(fit, newdata = nd), nd$treat, mean)
b = predictions(fit, variables = "treat")
b = tapply(b$estimate, b$treat, mean)
expect_equivalent(a, b, tolerance = tol)

a = as.numeric(tapply(predict(fit, newdata = nd), nd$treat, mean))
b = predictions(fit, variables = "treat", by = "treat")
expect_equivalent(a, b$estimate, tolerance = tol)


# Issue #1058:  Missing attributes for marginaleffects::datagrid(..., by = ) #1058
tmp <- mtcars
tmp <- tmp[c("mpg", "cyl", "hp")]
tmp$cyl <- as.factor(tmp$cyl)
tmp$hp <- as.factor(tmp$hp)
at1 <- attributes(datagrid(newdata = tmp, by = "cyl", hp = unique))
at2 <- attributes(datagrid(newdata = tmp, cyl = unique, hp = unique))
expect_true(all(names(at1) %in% names(at2)))


# Issue #1584
dat = get_dataset("thornton")
mod = glm(outcome ~ incentive + agecat, data = dat, family = binomial)
d1 <- datagrid(grid_type = "balanced", newdata = dat)
d2 <- datagrid(grid_type = "balanced", model = mod)
expect_true(all(0:1 %in% d1$incentive))
expect_true(all(0:1 %in% d2$incentive))
expect_true(all(0:1 %in% d1$incentive))
