source("helpers.R")
using("marginaleffects")

requiet("emmeans")
requiet("broom")
requiet("insight")


# Issue #438: backtransforms allows us to match `emmeans` exactly
mod <- glm(vs ~ mpg + factor(cyl), data = mtcars, family = binomial)
em <- emmeans(mod, ~cyl, type = "response")
mm <- marginal_means(mod)
expect_equal(data.frame(em)$prob, mm$estimate)
expect_equal(data.frame(em)$asymp.LCL, mm$conf.low)
expect_equal(data.frame(em)$asymp.UCL, mm$conf.high)

mod <- glm(breaks ~ wool * tension, family = Gamma, data = warpbreaks)
em <- suppressMessages(emmeans(mod, ~wool, type = "response", df = Inf))
mm <- marginal_means(mod, variables = "wool")
expect_equal(data.frame(em)$response, mm$estimate)
# TODO: 1/eta link function inverts order of CI. Should we clean this up?
expect_equal(data.frame(em)$asymp.LCL, mm$conf.high)
expect_equal(data.frame(em)$asymp.UCL, mm$conf.low)


# old tests used to require pre-conversion
dat <- mtcars
dat$am <- as.logical(dat$am)
dat$cyl <- as.factor(dat$cyl)
dat$vs <- as.factor(dat$vs)

# sanity check
mod <- lm(mpg ~ cyl + am + vs + hp, dat)
expect_error(marginal_means(mod, variables_grid = "junk"), pattern = "missing")
expect_error(marginal_means(mod, variables = "mpg"), pattern = "response")


# changing the prediction grid changes marginal means
# remember that the grid is variables + variables_grid
mod <- lm(mpg ~ cyl + am + vs + hp, dat)
mm1 <- marginal_means(mod, variables = "cyl")
mm2 <- marginal_means(mod, variables = "cyl", variables_grid = "vs")
mm3 <- marginal_means(mod, variables = "cyl", variables_grid = "am")
expect_false(all(mm1$estimate == mm2$estimate))
expect_false(all(mm1$estimate == mm3$estimate))
expect_false(all(mm2$estimate == mm3$estimate))


# tidy and glance
mod <- lm(mpg ~ cyl + am + hp, dat)
me <- marginal_means(mod)
ti <- tidy(me)
gl <- glance(me)
expect_equivalent(nrow(gl), 1)
expect_equivalent(nrow(ti), 5)
expect_true(ncol(ti) >= 8)



# marginalmeans vs. emmeans: poisson link or response
#skip_if_not_installed("emmeans", minimum_version = "1.7.3") # transform -> regrid
dat <- mtcars
dat$am <- factor(dat$am)
dat$cyl <- factor(dat$cyl)
mod <- glm(gear ~ cyl + am, data = dat, family = poisson)
# link
mm <- tidy(marginal_means(mod, variables = "cyl", type = "link"))
em <- tidy(emmeans(mod, specs = "cyl"))
expect_equivalent(mm$estimate, em$estimate)
expect_equivalent(mm$std.error, em$std.error)
# response
mm <- tidy(marginal_means(mod, variables = "cyl"))
em <- tidy(emmeans(mod, specs = "cyl", type = "response"))
expect_equivalent(mm$estimate, em$rate)
expect_equivalent(mm$p.value, em$p.value)



# simple marginal means
mod <- lm(mpg ~ cyl + am + hp, dat)
em <- broom::tidy(emmeans::emmeans(mod, "cyl"))
me <- marginal_means(mod, variables = "cyl")
expect_equivalent(me$estimate, em$estimate)
expect_equivalent(me$std.error, em$std.error)
em <- broom::tidy(emmeans::emmeans(mod, "am"))
me <- marginal_means(mod, variables = "am")
expect_equivalent(me$estimate, em$estimate)
expect_equivalent(me$std.error, em$std.error)


# interactions
# standard errors do not match emmeans
mod <- lm(mpg ~ cyl * am, dat)
em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "cyl")))
me <- marginal_means(mod, variables = "cyl")
me <- me[order(me$value),]
expect_equivalent(me$estimate, em$estimate)
em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "am")))
me <- suppressWarnings(marginal_means(mod, variables = "am"))
me <- me[order(me$value),]
expect_equivalent(me$estimate, em$estimate)

# error: no factor
mod <- lm(hp ~ mpg, mtcars)
expect_error(marginal_means(mod), pattern = "was found")

# wts
mod1 <- lm(vs ~ factor(am) + factor(gear) + factor(cyl), data = mtcars)
mod2 <- glm(vs ~ factor(am) + factor(gear) + mpg, data = mtcars, family = binomial)

# wts = "cells"
em <- data.frame(emmeans(mod1, ~am, weights = "cells"))
mm <- marginal_means(mod1, variables = "am", wts = "cells")
expect_equivalent(mm$estimate, em$emmean)
expect_equivalent(mm$std.error, em$SE)

em <- data.frame(emmeans(mod2, ~am, weights = "cells", type = "response"))
mm <- marginal_means(mod2, variables = "am", wts = "cells")
expect_equivalent(mm$estimate, em$prob)
expect_equivalent(mm$conf.low, em$asymp.LCL)
expect_equivalent(mm$conf.high, em$asymp.UCL)

# wts = "proportional"
em <- data.frame(emmeans(mod1, ~am, weights = "proportional"))
mm <- marginal_means(mod1, variables = "am", wts = "proportional")
expect_equivalent(mm$estimate, em$emmean)
expect_equivalent(mm$std.error, em$SE)

em <- data.frame(emmeans(mod2, ~am, weights = "proportional", type = "response"))
mm <- marginal_means(mod2, variables = "am", wts = "proportional")
expect_equivalent(mm$estimate, em$prob)
expect_equivalent(mm$conf.low, em$asymp.LCL)
expect_equivalent(mm$conf.high, em$asymp.UCL)

# Issue #583
dat <- mtcars
dat$am <- factor(dat$am)
dat$vs <- factor(dat$vs)
dat$cyl <- factor(dat$cyl)
mod <- glm(gear ~ cyl + vs + am, data = dat, family = poisson)

by <- data.frame(
    by = c("(4 & 6)", "(4 & 6)", "(8)"),
    cyl = unique(dat$cyl))
expect_inherits(marginal_means(mod, variables = "cyl", by = by), "marginalmeans")
expect_error(marginal_means(mod, by = by), pattern = "common")

# Issue #508
df <- data.frame(id = rep(1:5, each = 2e2))
df$city = ifelse(df$id <= 3, "Denver", "Paris")
df$y <- rbinom(1e3, 1, prob = plogis(-3 + 1/2 * df$id))
df$id <- factor(df$id)
ma <- aggregate(y ~ city, FUN = mean, data = df)

m <- glm(y ~ id, data = df, family = binomial)
by <- data.frame(
  id = 1:5,
  by = ifelse(1:5 <= 3, "Denver", "Paris"))

mm <- marginal_means(m, by = by, type = "response")
expect_equivalent(mm$estimate, ma$y, tol = .1)


# simple marginal means for each level of `cyl`
dat <- mtcars
dat$carb <- factor(dat$carb)
dat$cyl <- factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- lm(mpg ~ carb + cyl + am, dat)
by <- data.frame(
  cyl = c(4, 6, 8),
  by = c("4 & 6", "4 & 6", "8"))
mm <- marginal_means(mod,
  variables = "cyl",
  by = by)
expect_equivalent(nrow(mm), 2)


# Issue #620
requiet("nnet")
nom <- nnet::multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)
by <-
  data.frame(
    carb = c("1", "2", "3", "4", "6", "8"),
    by = c("1", "2", "3,4,6,8" |> rep(4)))
cmp <- comparisons(nom, by = by)
expect_equivalent(nrow(cmp), 9)



# # Issue #637: marginal_means() refactor
# mod <- lm(mpg ~ factor(cyl) + as.logical(am), data = mtcars)
# mm <- marginal_means(mod)

rm(list = ls())