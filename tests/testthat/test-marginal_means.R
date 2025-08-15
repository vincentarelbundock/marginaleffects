test_that("marginal means: backtransforms vs emmeans", {
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("emmeans")
    withr_library("broom")

    # Issue #438: backtransforms allows us to match `emmeans` exactly
    mod <- glm(vs ~ mpg + factor(cyl), data = mtcars, family = binomial)
    em <- emmeans(mod, ~cyl, type = "response")
    mm <- predictions(mod, by = "cyl", newdata = datagrid(grid_type = "balanced"), type = "invlink(link)") |>
        dplyr::arrange(cyl)
    expect_equal(data.frame(em)$prob, mm$estimate, ignore_attr = TRUE)
    expect_equal(data.frame(em)$asymp.LCL, mm$conf.low, tolerance = 1e-5, ignore_attr = TRUE)
    expect_equal(data.frame(em)$asymp.UCL, mm$conf.high, ignore_attr = TRUE)

    mod <- glm(breaks ~ wool * tension, family = Gamma, data = warpbreaks)
    em <- suppressMessages(emmeans(mod, ~wool, type = "response", df = Inf))
    mm <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "wool", type = "invlink(link)")
    expect_equal(data.frame(em)$response, mm$estimate, ignore_attr = TRUE)
    # TODO: 1/eta link function inverts order of CI. Should we clean this up?
    expect_equal(data.frame(em)$asymp.UCL, mm$conf.high, ignore_attr = TRUE)
    expect_equal(data.frame(em)$asymp.LCL, mm$conf.low, ignore_attr = TRUE)
})

test_that("marginal means: poisson models", {
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("emmeans")
    withr_library("broom")

    # marginalmeans vs. emmeans: poisson link or response
    # skip_if_not_installed("emmeans", minimum_version = "1.7.3") # transform -> regrid
    dat <- mtcars
    dat$am <- factor(dat$am)
    dat$cyl <- factor(dat$cyl)
    mod <- glm(gear ~ cyl + am, data = dat, family = poisson)
    # link
    mm <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "cyl", type = "link") |>
        dplyr::arrange(cyl)
    em <- tidy(emmeans(mod, specs = "cyl"))
    expect_equal(mm$estimate, em$estimate, tolerance = 1e-5, ignore_attr = TRUE)
    expect_equal(mm$estimate, em$estimate, tolerance = 1e-5, ignore_attr = TRUE)
    # response
    mm <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "cyl", type = "invlink(link)") |>
        dplyr::arrange(cyl)
    em <- tidy(emmeans(mod, specs = "cyl", type = "response"))
    expect_equal(mm$estimate, em$rate, ignore_attr = TRUE)
    expect_equal(mm$p.value, em$p.value, ignore_attr = TRUE)
})

test_that("marginal means: simple linear models", {
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("emmeans")
    withr_library("broom")

    # simple marginal means
    dat <- mtcars
    dat$am <- as.logical(dat$am)
    dat$cyl <- as.factor(dat$cyl)
    dat$vs <- as.factor(dat$vs)

    mod <- lm(mpg ~ cyl + am + hp, dat)
    em <- broom::tidy(emmeans::emmeans(mod, "cyl"))
    me <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "cyl") |>
        dplyr::arrange(cyl)
    expect_equal(me$estimate, em$estimate, ignore_attr = TRUE)
    expect_equal(me$std.error, em$std.error, tolerance = 1e-5, ignore_attr = TRUE)
    em <- broom::tidy(emmeans::emmeans(mod, "am"))
    me <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "am") |>
        dplyr::arrange(am)
    expect_equal(me$estimate, em$estimate, ignore_attr = TRUE)
    expect_equal(me$std.error, em$std.error, tolerance = 1e-5, ignore_attr = TRUE)
})

test_that("marginal means: interaction models", {
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("emmeans")
    withr_library("broom")

    # interactions
    # standard errors do not match emmeans
    dat <- mtcars
    dat$am <- as.logical(dat$am)
    dat$cyl <- as.factor(dat$cyl)
    dat$vs <- as.factor(dat$vs)

    mod <- lm(mpg ~ cyl * am, dat)
    em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "cyl")))
    me <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "cyl") |>
        dplyr::arrange(cyl)
    expect_equal(me$estimate, em$estimate, ignore_attr = TRUE)
    em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "am")))
    me <- suppressWarnings(predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "am"))
    me <- me[order(me$am), ]
    expect_equal(me$estimate, em$estimate, ignore_attr = TRUE)
})

test_that("marginal means: weights", {
    skip_if_not_installed("emmeans")

    withr_library("emmeans")

    # wts
    mod1 <- lm(vs ~ factor(am) + factor(gear) + factor(cyl), data = mtcars)
    mod2 <- glm(vs ~ factor(am) + factor(gear) + mpg, data = mtcars, family = binomial)

    # wts = "cells"
    em <- data.frame(emmeans(mod1, ~am, weights = "cells"))
    mm <- predictions(mod1, by = "am")
    mm <- mm[order(mm$am), ]
    expect_equal(mm$estimate, em$emmean, ignore_attr = TRUE)
    expect_equal(mm$std.error, em$SE, tolerance = 1e-5, ignore_attr = TRUE)
})

test_that("marginal means: custom by groups", {
    skip_if_not_installed("emmeans")

    withr_library("emmeans")

    # Issue #583
    dat <- mtcars
    dat$am <- factor(dat$am)
    dat$vs <- factor(dat$vs)
    dat$cyl <- factor(dat$cyl)
    mod <- glm(gear ~ cyl + vs + am, data = dat, family = poisson)

    by <- data.frame(
        by = c("(4 & 6)", "(4 & 6)", "(8)"),
        cyl = unique(dat$cyl)
    )
    pre <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = by)
    expect_s3_class(pre, "predictions")
})

test_that("marginal means: multinomial models", {
    skip_if_not_installed("nnet")

    withr_library("nnet")

    # Issue #620
    nom <- nnet::multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)
    by <-
        data.frame(
            carb = c("1", "2", "3", "4", "6", "8"),
            by = c("1", "2", "3,4,6,8" |> rep(4))
        )
    cmp <- comparisons(nom, by = by)
    expect_equal(nrow(cmp), 9, ignore_attr = TRUE)
})
