tol <- 1e-5


# informative errors
expect_error(datagrid(Petal.Length = 4.6), regexp = "must not be `NULL`")

# numeric clusters no longer produce a warning; selects mode
if (requireNamespace("lme4", quietly = TRUE)) {
    mod <- lme4::lmer(mpg ~ hp + (1 + drat | cyl), data = mtcars)
    expect_true(datagrid(model = mod)$cyl == 8)
}

# functions
if (requireNamespace("lme4", quietly = TRUE)) {
    mod <- lme4::lmer(mpg ~ hp + (1 + drat | cyl), data = mtcars)

    cmp <- comparisons(
        mod,
        newdata = datagrid(hp = range, cyl = unique)
    )
    expect_equal(nrow(cmp), 6, tolerance = tol, ignore_attr = TRUE)

    cmp <- comparisons(
        mod,
        newdata = datagrid(hp = range)
    )
    expect_equal(nrow(cmp), 2, tolerance = tol, ignore_attr = TRUE)

    p <- predictions(
        mod,
        newdata = datagrid(hp = fivenum)
    )
    expect_equal(nrow(p), 5, tolerance = tol, ignore_attr = TRUE)
}

nd <- datagrid(newdata = mtcars, hp = range, mpg = fivenum, wt = sd)
expect_equal(nrow(nd), 10, tolerance = tol, ignore_attr = TRUE)

mod <- glm(am ~ factor(gear), data = mtcars)
cmp <- comparisons(mod, newdata = datagrid(am = 0, gear = mtcars$gear))
expect_equal(nrow(cmp), 6, tolerance = tol, ignore_attr = TRUE)
cmp <- comparisons(mod, newdata = datagrid(am = unique, gear = max))
expect_equal(nrow(cmp), 4, tolerance = tol, ignore_attr = TRUE)


# Issue #721
if (requireNamespace("haven", quietly = TRUE)) {
    m <- suppressWarnings(haven::read_dta(test_path("../../inst/tinytest/modelarchive/data/margex.dta")))
    if (inherits(m, "data.frame")) {
        m <- data.frame(m)
        m$sex <- as.factor(m$sex)
        mod <- lm(y ~ sex + age + distance, data = m)
        expect_error(
            predictions(mod, newdata = datagrid(sex = c("male", "female"))),
            regexp = "must be one of the factor levels"
        )
        expect_error(
            predictions(mod, newdata = datagrid(sex = c("male", "femael"))),
            regexp = "must be one of the factor levels"
        )
    }
}
mod <- lm(mpg ~ qsec + as.factor(gear), data = mtcars)
expect_error(
    predictions(mod, newdata = datagrid(gear = 6)),
    regexp = "must be one of the factor levels"
)
expect_error(
    comparisons(mod, newdata = datagrid(gear = 6)),
    regexp = "must be one of the factor levels"
)


# Issue #688
dat <<- transform(mtcars, cyl = factor(cyl))
mod <- lm(mpg ~ hp, data = dat)
d <- datagrid(model = mod, by = c("carb", "cyl"), response = TRUE)
k <- aggregate(cbind(mpg, hp) ~ carb + cyl, data = dat, FUN = mean)
expect_equal(k$mpg, d$mpg, tolerance = tol, ignore_attr = TRUE)


# Issue 766: categorical predictors + variables arg + avg
if (requireNamespace("MatchIt", quietly = TRUE)) {
    data("lalonde", package = "MatchIt")
    fit <- lm(re78 ~ race * treat, data = lalonde)

    a = predict(fit, newdata = lalonde)
    b = predictions(fit, newdata = lalonde)
    expect_equal(a, b$estimate, tolerance = tol, ignore_attr = TRUE)
    nd = rbind(transform(lalonde, treat = 0), transform(lalonde, treat = 1))
    a = predict(fit, newdata = nd)
    b = predictions(fit, newdata = lalonde, variables = "treat")
    expect_equal(a, b$estimate, tolerance = tol, ignore_attr = TRUE)

    a = tapply(predict(fit, newdata = nd), nd$treat, mean)
    b = avg_predictions(fit, newdata = lalonde, variables = "treat") |> suppressWarnings()
    expect_equal(as.numeric(a), b$estimate, tolerance = tol, ignore_attr = TRUE)

    a = predict(fit, newdata = nd)
    b = predictions(fit, variables = "treat")
    expect_equal(a, b$estimate, tolerance = tol, ignore_attr = TRUE)

    a = tapply(predict(fit, newdata = nd), nd$treat, mean)
    b = avg_predictions(fit, variables = "treat")
    expect_equal(as.numeric(a), b$estimate, tolerance = tol, ignore_attr = TRUE)

    a = tapply(predict(fit, newdata = nd), nd$treat, mean)
    b = predictions(fit, variables = "treat")
    b = tapply(b$estimate, b$treat, mean)
    expect_equal(a, b, tolerance = tol, ignore_attr = TRUE)

    a = as.numeric(tapply(predict(fit, newdata = nd), nd$treat, mean))
    b = predictions(fit, variables = "treat", by = "treat")
    expect_equal(a, b$estimate, tolerance = tol, ignore_attr = TRUE)
}


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
