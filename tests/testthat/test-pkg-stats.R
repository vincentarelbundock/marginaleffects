skip_if_not_installed("margins")
skip_if_not_installed("emmeans")
skip_if_not_installed("dplyr")
skip_if_not_installed("broom")
requiet("margins")
requiet("broom")
requiet("emmeans")
requiet("dplyr")

guerry <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv")

test_that("glm: marginaleffects", {
    set.seed(1024)
    N <- 1e2
    dat <- data.frame(x1 = rnorm(N),
                      x2 = rnorm(N),
                      x3 = rnorm(N),
                      x4 = rnorm(N),
                      e = rnorm(N))
    dat$y <- as.numeric(plogis(
        dat$x1 + dat$x2 + dat$x3 + dat$x4 + dat$x3 * dat$x4 + dat$e) > 0.5)
    mod <- glm(y ~ x1 + x2 + x3 * x4, data = dat, family = binomial)
    res <- marginaleffects(mod)
    mar <- margins(mod, unit_ses = TRUE)
    expect_true(test_against_margins(res, mar, tolerance = 0.1, verbose=TRUE))
})


test_that("glm vs. Stata: marginaleffects", {
    stata <- readRDS(test_path("stata/stata.rds"))[["stats_glm_01"]]
    dat <- read.csv(test_path("stata/databases/stats_glm_01.csv"))
    mod <- glm(y ~ x1 * x2, family = binomial, data = dat)
    ame <- marginaleffects(mod) %>%
           group_by(term) %>%
           summarize(dydx = mean(dydx), std.error = mean(std.error)) %>%
           inner_join(stata, by = "term")
    expect_equal(ame$dydx, ame$dydxstata, tolerance = 0.00001)
})


test_that("lm vs. Stata: marginaleffects", {
    stata <- readRDS(test_path("stata/stata.rds"))[["stats_lm_01"]]
    dat <- read.csv(test_path("stata/databases/stats_lm_01.csv"))
    mod <- lm(y ~ x1 * x2, data = dat)
    ame <- marginaleffects(mod) %>%
           group_by(term) %>%
           summarize(dydx = mean(dydx), std.error = mean(std.error)) %>%
           inner_join(stata, by = "term")
    expect_equal(ame$dydx, ame$dydxstata, tolerance = 0.00001)
})


test_that("lm with interactions vs. margins: marginaleffects", {
    counterfactuals <- expand.grid(hp = 100, am = 0:1)
    mod <- lm(mpg ~ hp * am, data = mtcars)
    res <- marginaleffects(mod, variable = "hp", newdata = counterfactuals)
    mar <- margins(mod, variable = "hp", data = counterfactuals, unit_ses = TRUE)
    expect_true(test_against_margins(res, mar, tolerance = 1e-3))
})


test_that("lm vs. emmeans: marginalmeans", {
    dat <- mtcars
    dat$cyl <- as.factor(dat$cyl)
    dat$am <- as.logical(dat$am)
    mod <- lm(mpg ~ hp + cyl + am, data = dat)
    mm <- tidy(marginalmeans(mod, variables = "cyl"))
    em <- broom::tidy(emmeans::emmeans(mod, specs = "cyl"))
    expect_equal(mm$estimate, em$estimate)
    expect_equal(mm$std.error, em$std.error)
    mm <- tidy(marginalmeans(mod, variables = "am"))
    em <- broom::tidy(emmeans::emmeans(mod, specs = "am"))
    expect_equal(mm$estimate, em$estimate)
    expect_equal(mm$std.error, em$std.error)
})


test_that('glm vs. emmeans: marginalmeans(type = "link")', {
    # factors seem to behave differently in model.matrix
    skip_if(getRversion() == "3.6.3")
    dat <- guerry
    dat$binary <- dat$Crime_prop > median(dat$Crime_prop)
    # character variables sometimes break the order
    mod <- glm(binary ~ Region + MainCity + Commerce, data = dat, family = "binomial")
    expect_error(marginalmeans(mod, type = "link"), regexp = "track of the reference category")
    # factor variables are safer
    dat$Region <- as.factor(dat$Region)
    dat$MainCity <- as.factor(dat$MainCity)
    mod <- glm(binary ~ Region + MainCity + Commerce, data = dat, family = "binomial")
    mm <- tidy(marginalmeans(mod, type = "link", variables = "Region"))
    em <- tidy(emmeans::emmeans(mod, specs = "Region"))
    expect_equal(as.character(mm$group), em$Region)
    expect_equal(mm$estimate, em$estimate)
    expect_equal(mm$std.error, em$std.error)
    mm <- tidy(marginalmeans(mod, type = "link", variables = "MainCity"))
    em <- tidy(emmeans::emmeans(mod, specs = "MainCity"))
    expect_equal(as.character(mm$group), em$MainCity)
    expect_equal(mm$estimate, em$estimate)
    expect_equal(mm$std.error, em$std.error)
})


###################################################
#  note sure if stats::loess should be supported  #
###################################################

# test_that("vcov(loess) does not exist", {
#     mod <- loess(mpg ~ wt, data = mtcars)
#     expect_warning(marginaleffects(mod), regexp = "not yet supported")
# })


# test_that("loess error", {
#     skip("loess produces different results under margins and marginaleffects")
#     mod <- loess(mpg ~ wt, data = mtcars)
#     res <- marginaleffects(mod, vcov = FALSE)
#     mar <- data.frame(margins(mod))
#     expect_true(test_against_margins(res, mar, tolerance = .8))
# })

