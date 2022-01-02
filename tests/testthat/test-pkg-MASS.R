# TODO: emtrends not clear what it computes for polr
skip_if_not_installed("MASS")
skip_if_not_installed("emmeans")
skip_if_not_installed("margins")

requiet("margins")
requiet("MASS")
requiet("emmeans")
requiet("broom")


### marginaleffects
test_that("rlm: marginaleffects: vs. margins vs. emmeans", {
    model <- MASS::rlm(mpg ~ hp + drat, mtcars)
    expect_marginaleffects(model, n_unique = 1)

    # margins
    mfx <- tidy(marginaleffects(model))
    mar <- tidy(margins(model))
    expect_equal(mfx$estimate, mar$estimate, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mar$std.error, ignore_attr = TRUE, tolerance = .00001)

    # emmeans
    mfx <- marginaleffects(model, newdata = datagrid(drat = 3.9, hp = 110))
    em1 <- emmeans::emtrends(model, ~hp, "hp", at = list(hp = 110, drat = 3.9))
    em2 <- emmeans::emtrends(model, ~drat, "drat", at = list(hp = 110, drat = 3.9))
    em1 <- tidy(em1)
    em2 <- tidy(em2)
    expect_equal(mfx$dydx[1], em1$hp.trend)
    expect_equal(mfx$std.error[1], em1$std.error, tolerance = .001)
    expect_equal(mfx$dydx[2], em2$drat.trend)
    expect_equal(mfx$std.error[2], em2$std.error, tolerance = .002)
})

test_that("glm.nb: marginaleffects: vs. margins", {
    # margins does not support unit-level standard errors
    model <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    mfx <- tidy(marginaleffects(model))
    mar <- tidy(margins(model))
    expect_equal(mfx$estimate, mar$estimate, ignore_attr = TRUE, tolerance = .0001)
    expect_equal(mfx$std.error, mar$std.error, ignore_attr = TRUE, tolerance = .001)
    # emtrends
    mfx <- marginaleffects(model, newdata = datagrid(wt = 2.6, cyl = 4), type = "link")
    em <- emtrends(model, ~wt, "wt", at = list(wt = 2.6, cyl = 4))
    em <- tidy(em)
    expect_equal(mfx$dydx[1], em$wt.trend)
    expect_equal(mfx$std.error[1], em$std.error)
})

test_that("glm.nb: marginaleffects: vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$mass_glm_nb
    model <- suppressWarnings(
        MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    mfx <- merge(tidy(marginaleffects(model)), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
})

test_that("polr: marginaleffects: vs. Stata", {
    skip("works interactively")
    stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    mod <- MASS::polr(factor(y) ~ x1 + x2, data = dat)
    mfx <- marginaleffects(mod, type = "probs")
    mfx <- tidy(mfx)
    mfx <- merge(mfx, stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
    expect_marginaleffects(mod, type = "probs")
})


### predictions

test_that("polr: predictions: no validity", {
    skip_if_not_installed("insight", minimum_version = "0.14.4.1")
    mod <- MASS::polr(factor(gear) ~ mpg + factor(cyl), data = mtcars)
    pred <- predictions(mod, type = "probs")
    expect_predictions(pred, se = FALSE)
})

test_that("glm.nb: predictions: no validity", {
    model <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    pred <- predictions(model)
    expect_predictions(pred, se = TRUE)
})

test_that("rlm: predictions: no validity", {
    skip_if_not_installed("insight", minimum_version = "0.14.4.1")
    model <- MASS::rlm(mpg ~ hp + drat, mtcars)
    pred <- predictions(model)
    expect_predictions(pred, se = TRUE, n_row = nrow(mtcars))
    pred <- predictions(model, newdata = head(mtcars))
    expect_predictions(pred, se = TRUE, n_row = 6)
})


### marginalmeans

test_that("glm.nb: marginalmeans: vs. emmeans", {
    dat <- mtcars
    dat$cyl <- as.factor(dat$cyl)
    dat$am <- as.logical(dat$am)
    model <- suppressWarnings(MASS::glm.nb(carb ~ am + cyl, data = dat))
    mm <- marginalmeans(model, type = "link", variables = "cyl")
    ti <- tidy(mm)
    em <- tidy(emmeans::emmeans(model, "cyl"))
    expect_marginalmeans(mm, se = TRUE)
    expect_equal(ti$estimate, em$estimate)
    expect_equal(ti$std.error, em$std.error)
})


test_that("rlm: marginalmeans: vs. emmeans", {
    skip_if_not_installed("insight", minimum_version = "0.14.4.1")
    dat <- mtcars
    dat$cyl <- as.factor(dat$cyl)
    dat$am <- as.logical(dat$am)
    model <- MASS::rlm(mpg ~ cyl + am, dat)
    mm <- marginalmeans(model)
    expect_marginalmeans(mm, se = TRUE)
    ti <- tidy(marginalmeans(model, variables = "cyl"))
    em <- tidy(emmeans::emmeans(model, "cyl"))
    expect_equal(ti$estimate, em$estimate)
    expect_equal(ti$std.error, em$std.error)
})


# glmmPQL

test_that("glmmPQL: no validity", {
    tmp <- bacteria
    tmp$week_bin <- tmp$week > 2
    mod <- glmmPQL(y ~ trt + week_bin, random = ~ 1 | ID,
                   family = binomial,
                   verbose = FALSE,
                   data = tmp)
    expect_marginaleffects(mod, type = "link", n_unique = 1)
    expect_marginaleffects(mod, type = "response")
    expect_predictions(predictions(mod), se = FALSE)

    # emtrends
    em <- emmeans::emtrends(mod, ~week_bin, "week_bin", at = list(week_bin = 0))
    em <- tidy(em)
    mfx <- marginaleffects(mod, newdata = datagrid(week_bin = 0), type = "link")
    expect_equal(mfx$dydx[3], em$week_bin.trend)
    expect_equal(mfx$std.error[3], em$std.error, tolerance = .01)
})
