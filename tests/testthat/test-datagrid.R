test_that("datagrid: basic functionality", {
    skip_if_not_installed("lme4")

    withr_library("lme4")

    # informative errors
    expect_error(datagrid(Petal.Length = 4.6), pattern = "inside")

    # numeric clusters no longer produce a warning; selects mode
    mod <- lme4::lmer(mpg ~ hp + (1 + drat | cyl), data = mtcars)
    expect_true(datagrid(model = mod)$cyl == 8)
})

test_that("datagrid: function specifications", {
    skip_if_not_installed("lme4")

    withr_library("lme4")

    # functions
    mod <- lme4::lmer(mpg ~ hp + (1 + drat | cyl), data = mtcars)
    cmp <- comparisons(
        mod,
        newdata = datagrid(hp = range, cyl = unique)
    )
    expect_equal(nrow(cmp), 6, ignore_attr = TRUE)

    cmp <- comparisons(
        mod,
        newdata = datagrid(hp = range)
    )
    expect_equal(nrow(cmp), 2, ignore_attr = TRUE)

    p <- predictions(
        mod,
        newdata = datagrid(hp = fivenum)
    )
    expect_equal(nrow(p), 5, ignore_attr = TRUE)

    nd <- datagrid(newdata = mtcars, hp = range, mpg = fivenum, wt = sd)
    expect_equal(nrow(nd), 10, ignore_attr = TRUE)

    mod <- glm(am ~ factor(gear), data = mtcars)
    cmp <- comparisons(mod, newdata = datagrid(am = 0, gear = mtcars$gear))
    expect_equal(nrow(cmp), 6, ignore_attr = TRUE)
    cmp <- comparisons(mod, newdata = datagrid(am = unique, gear = max))
    expect_equal(nrow(cmp), 4, ignore_attr = TRUE)
})

test_that("datagrid: factor level validation", {
    skip_if_not_installed("haven")

    withr_library("haven")

    # Issue #721
    m <- marginaleffects:::hush(read_dta(test_path("modelarchive/data/margex.dta")))
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
})

test_that("datagrid: by argument with response", {
    # Issue #688
    dat <- transform(mtcars, cyl = factor(cyl))
    mod <- lm(mpg ~ hp, data = dat)
    d <- datagrid(model = mod, by = c("carb", "cyl"), response = TRUE)
    k <- aggregate(cbind(mpg, hp) ~ carb + cyl, data = dat, FUN = mean)
    expect_equal(k$mpg, d$mpg, ignore_attr = TRUE)
})

test_that("datagrid: categorical predictors with variables", {
    skip_if_not_installed("MatchIt")

    withr_library("MatchIt")

    # Issue 766: categorical predictors + variables arg + avg
    data("lalonde", package = "MatchIt")
    fit <- lm(re78 ~ race * treat, data = lalonde)

    a = predict(fit, newdata = lalonde)
    b = predictions(fit, newdata = lalonde)
    expect_equal(a, b$estimate, ignore_attr = TRUE)

    nd = rbind(transform(lalonde, treat = 0), transform(lalonde, treat = 1))
    a = predict(fit, newdata = nd)
    b = predictions(fit, newdata = lalonde, variables = "treat")
    expect_equal(a, b$estimate, ignore_attr = TRUE)

    a = tapply(predict(fit, newdata = nd), nd$treat, mean)
    b = avg_predictions(fit, newdata = lalonde, variables = "treat") |> suppressWarnings()
    expect_equal(as.numeric(a), b$estimate, ignore_attr = TRUE)

    a = predict(fit, newdata = nd)
    b = predictions(fit, variables = "treat")
    expect_equal(a, b$estimate, ignore_attr = TRUE)

    a = tapply(predict(fit, newdata = nd), nd$treat, mean)
    b = avg_predictions(fit, variables = "treat")
    expect_equal(as.numeric(a), b$estimate, ignore_attr = TRUE)

    a = tapply(predict(fit, newdata = nd), nd$treat, mean)
    b = predictions(fit, variables = "treat")
    b = tapply(b$estimate, b$treat, mean)
    expect_equal(a, b, ignore_attr = TRUE)

    a = as.numeric(tapply(predict(fit, newdata = nd), nd$treat, mean))
    b = predictions(fit, variables = "treat", by = "treat")
    expect_equal(a, b$estimate, ignore_attr = TRUE)
})

test_that("datagrid: attributes consistency", {
    # Issue #1058:  Missing attributes for marginaleffects::datagrid(..., by = ) #1058
    tmp <- mtcars
    tmp <- tmp[, c("mpg", "cyl", "hp")]
    tmp$cyl <- as.factor(tmp$cyl)
    tmp$hp <- as.factor(tmp$hp)
    at1 <- attributes(datagrid(newdata = tmp, by = "cyl", hp = unique))
    at2 <- attributes(datagrid(newdata = tmp, cyl = unique, hp = unique))
    expect_true(all(names(at1) %in% names(at2)))
})
