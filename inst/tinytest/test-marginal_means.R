source("helpers.R")
using("marginaleffects")

requiet("emmeans")
requiet("broom")
requiet("insight")


# Issue #438: backtransforms allows us to match `emmeans` exactly
mod <- glm(vs ~ mpg + factor(cyl), data = mtcars, family = binomial)
em <- emmeans(mod, ~cyl, type = "response")
mm <- predictions(mod, by = "cyl", newdata = datagrid(grid_type = "balanced"), type = "invlink(link)") |>
    dplyr::arrange(cyl)
expect_equal(data.frame(em)$prob, mm$estimate)
expect_equal(data.frame(em)$asymp.LCL, mm$conf.low, tolerance = 1e-5)
expect_equal(data.frame(em)$asymp.UCL, mm$conf.high)

mod <- glm(breaks ~ wool * tension, family = Gamma, data = warpbreaks)
em <- suppressMessages(emmeans(mod, ~wool, type = "response", df = Inf))
mm <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "wool", type = "invlink(link)")
expect_equal(data.frame(em)$response, mm$estimate)
# TODO: 1/eta link function inverts order of CI. Should we clean this up?
expect_equal(data.frame(em)$asymp.UCL, mm$conf.high)
expect_equal(data.frame(em)$asymp.LCL, mm$conf.low)


# old tests used to require pre-conversion
dat <- mtcars
dat$am <- as.logical(dat$am)
dat$cyl <- as.factor(dat$cyl)
dat$vs <- as.factor(dat$vs)


# marginalmeans vs. emmeans: poisson link or response
#skip_if_not_installed("emmeans", minimum_version = "1.7.3") # transform -> regrid
dat <- mtcars
dat$am <- factor(dat$am)
dat$cyl <- factor(dat$cyl)
mod <- glm(gear ~ cyl + am, data = dat, family = poisson)
# link
mm <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "cyl", type = "link") |>
    dplyr::arrange(cyl)
em <- tidy(emmeans(mod, specs = "cyl"))
expect_equivalent(mm$estimate, em$estimate, tolerance = 1e-5)
expect_equivalent(mm$estimate, em$estimate, tolerance = 1e-5)
# response
mm <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "cyl", type = "invlink(link)") |>
    dplyr::arrange(cyl)
em <- tidy(emmeans(mod, specs = "cyl", type = "response"))
expect_equivalent(mm$estimate, em$rate)
expect_equivalent(mm$p.value, em$p.value)


# simple marginal means
mod <- lm(mpg ~ cyl + am + hp, dat)
em <- broom::tidy(emmeans::emmeans(mod, "cyl"))
me <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "cyl") |>
    dplyr::arrange(cyl)
expect_equivalent(me$estimate, em$estimate)
expect_equivalent(me$std.error, em$std.error, tolerance = 1e-5)
em <- broom::tidy(emmeans::emmeans(mod, "am"))
me <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "am") |>
    dplyr::arrange(am)
expect_equivalent(me$estimate, em$estimate)
expect_equivalent(me$std.error, em$std.error, tolerance = 1e-5)


# interactions
# standard errors do not match emmeans
mod <- lm(mpg ~ cyl * am, dat)
em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "cyl")))
me <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "cyl") |>
    dplyr::arrange(cyl)
expect_equivalent(me$estimate, em$estimate)
em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "am")))
me <- suppressWarnings(predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "am"))
me <- me[order(me$am), ]
expect_equivalent(me$estimate, em$estimate)

# wts
mod1 <- lm(vs ~ factor(am) + factor(gear) + factor(cyl), data = mtcars)
mod2 <- glm(vs ~ factor(am) + factor(gear) + mpg, data = mtcars, family = binomial)

# wts = "cells"
em <- data.frame(emmeans(mod1, ~am, weights = "cells"))
mm <- predictions(mod1, by = "am")
mm <- mm[order(mm$am), ]
expect_equivalent(mm$estimate, em$emmean)
expect_equivalent(mm$std.error, em$SE, tolerance = 1e-5)

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
expect_inherits(predictions(mod, newdata = datagrid(grid_type = "balanced"), by = by), "predictions")


# Issue #620
requiet("nnet")
nom <- nnet::multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)
by <-
    data.frame(
        carb = c("1", "2", "3", "4", "6", "8"),
        by = c("1", "2", "3,4,6,8" |> rep(4))
    )
cmp <- comparisons(nom, by = by)
expect_equivalent(nrow(cmp), 9)
