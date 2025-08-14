test_that("factor handling works correctly in formulas", {
    withr_library("marginaleffects")

    # factor before fitting or in formula is the same
    tmp <- mtcars
    tmp$cyl <- factor(tmp$cyl)
    mod1 <- lm(mpg ~ hp + factor(cyl), mtcars)
    mod2 <- lm(mpg ~ hp + cyl, tmp)
    mfx1 <- suppressWarnings(slopes(mod1))
    mfx2 <- slopes(mod2)
    expect_equal(mfx1$estimate, mfx2$estimate, ignore_attr = TRUE)
    expect_equal(mfx1$std.error, mfx2$std.error, tolerance = 1e-5, ignore_attr = TRUE)
})

test_that("factor works on LHS and RHS at the same time", {
    withr_library("marginaleffects")
    withr_library("MASS")

    # factor on LHS and RHS at the same time.
    data(housing, package = "MASS")
    mod <- MASS::polr(Infl ~ Sat + Freq, data = housing, Hess = TRUE)
    mfx <- suppressMessages(slopes(mod, type = "probs"))
    expect_s3_class(mfx, "marginaleffects")
    expect_true(all(c("Low", "Medium", "High") %in% mfx$group))
})

test_that("smart detection of factor() in formula works", {
    withr_library("marginaleffects")
    withr_library("estimatr")

    # smart detect factor() in formula
    model <- lm_robust(carb ~ wt + factor(cyl), se_type = "stata", data = mtcars)
    k <- slopes(model)
    expect_true(all(c("dY/dX", "8 - 4") %in% k$contrast))
})

test_that("factor in formula works with incomplete newdata", {
    withr_library("marginaleffects")

    # factor in formula with incomplete newdata
    mod <- lm(mpg ~ factor(cyl), data = mtcars)
    mfx1 <- slopes(mod, newdata = data.frame(cyl = 4))
    mfx2 <- slopes(mod, newdata = datagrid(cyl = 4))
    expect_equal(mfx1[, 1:5], mfx2[, 1:5], ignore_attr = TRUE)
})

test_that("get_data.coxph() works with strata()", {
    withr_library("marginaleffects")
    withr_library("survival")

    # bugs stay dead: get_data.coxph() with strata()
    test1 <- data.frame(
        time = c(4, 3, 1, 1, 2, 2, 3),
        status = c(1, 1, 1, 0, 1, 1, 0),
        x = c(0, 2, 1, 1, 1, 0, 0),
        sex = c(0, 0, 0, 0, 1, 1, 1)
    )
    mod <- coxph(Surv(time, status) ~ x + strata(sex), data = test1, ties = "breslow")

    nd <- datagrid(sex = 0, newdata = test1)
    mfx <- slopes(mod, variables = "x", newdata = nd, type = "lp") |>
        suppressWarnings()
    expect_s3_class(mfx, "marginaleffects")
})

test_that("factor comparisons work with different input formats", {
    withr_library("marginaleffects")

    # Issue #497
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    dat$cyl <- as.factor(dat$cyl)
    mod <- lm(mpg ~ cyl, dat)
    cmp1 <- comparisons(mod, variables = list(cyl = c(6, 4)))
    cmp2 <- comparisons(mod, variables = list(cyl = c("4", "6")))
    cmp3 <- comparisons(mod, variables = list(cyl = dat$cyl[2:3]))
    expect_s3_class(cmp1, "comparisons")
    expect_s3_class(cmp2, "comparisons")
    expect_s3_class(cmp3, "comparisons")
})

test_that("factor comparisons work with minmax and transform", {
    withr_library("marginaleffects")

    # Issue #658
    dat <- transform(mtcars, cyl = factor(cyl))
    mod <- lm(mpg ~ cyl, mtcars)
    cmp <- comparisons(
        mod,
        variables = list(cyl = "minmax"),
        transform = function(x) x / 3
    )
    expect_s3_class(cmp, "comparisons")
})

test_that("factor handling works with hypothesis and newdata", {
    withr_library("marginaleffects")

    # Issue #1105: do not merge `newdata` when `hypothesis` to avoid corner cases
    set.seed(1234)
    dat <- data.frame(
        outcome = rbinom(n = 100, size = 1, prob = 0.35),
        var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.3)),
        var_cont = rnorm(n = 100, mean = 10, sd = 7),
        groups = sample(letters[1:2], size = 100, replace = TRUE)
    )
    m1 <- glm(outcome ~ var_binom * groups + var_cont, data = dat, family = binomial())
    d <- datagrid(model = m1, by = c("var_binom", "groups"))
    p1 <- predictions(
        m1,
        type = "response",
        newdata = d,
        hypothesis = ~pairwise
    )
    set.seed(1234)
    dat <- data.frame(
        outcome = rbinom(n = 100, size = 1, prob = 0.35),
        var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.3)),
        var_cont = rnorm(n = 100, mean = 10, sd = 7),
        groups = factor(sample(letters[1:2], size = 100, replace = TRUE))
    )
    m1 <- glm(outcome ~ var_binom * groups + var_cont, data = dat, family = binomial())
    d <- datagrid(model = m1, by = c("var_binom", "groups"))

    p2 <- predictions(
        m1,
        newdata = d,
        type = "response",
        hypothesis = ~pairwise
    )
    expect_equal(p1$estimate, p2$estimate, ignore_attr = TRUE)
    expect_equal(p1$std.error, p2$std.error, ignore_attr = TRUE)
})
