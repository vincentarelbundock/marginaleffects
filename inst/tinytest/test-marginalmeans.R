source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("emmeans")
requiet("broom")
requiet("insight")


# Issue #438: backtransforms allows us to match `emmeans` exactly
mod <- glm(vs ~ mpg + factor(cyl), data = mtcars, family = binomial)
em <- emmeans(mod, ~cyl, type = "response")
mm <- marginalmeans(mod)
expect_equal(data.frame(em)$prob, mm$marginalmean)
expect_equal(data.frame(em)$asymp.LCL, mm$conf.low)
expect_equal(data.frame(em)$asymp.UCL, mm$conf.high)

mod <- glm(breaks ~ wool * tension, family = Gamma, data = warpbreaks)
em <- suppressMessages(emmeans(mod, ~wool, type = "response", df = Inf))
mm <- marginalmeans(mod, variables = "wool")
expect_equal(data.frame(em)$response, mm$marginalmean)
# TODO: 1/eta link function inverts order of CI. Should we clean this up?
expect_equal(data.frame(em)$asymp.LCL, mm$conf.high)
expect_equal(data.frame(em)$asymp.UCL, mm$conf.low)


# as.factor and as.logical in formula
mod <- lm(mpg ~ factor(cyl) + as.factor(gear) + as.logical(am), data = mtcars)
mm <- marginalmeans(mod, variables = "cyl", by = "am")
expect_inherits(mm, "marginalmeans")
expect_equal(nrow(mm), 6)


# old tests used to require pre-conversion
dat <- mtcars
dat$am <- as.logical(dat$am)
dat$cyl <- as.factor(dat$cyl)
dat$vs <- as.factor(dat$vs)

# sanity check
mod <- lm(mpg ~ cyl + am + vs + hp, dat)
expect_error(marginalmeans(mod, variables_grid = "junk"), pattern = "not found")
expect_error(marginalmeans(mod, variables = "mpg"), pattern = "response")


# changing the prediction grid changes marginal means
# remember that the grid is variables + variables_grid
mod <- lm(mpg ~ cyl + am + vs + hp, dat)
mm1 <- marginalmeans(mod, variables = "cyl")
mm2 <- marginalmeans(mod, variables = "cyl", variables_grid = "vs")
mm3 <- marginalmeans(mod, variables = "cyl", variables_grid = "am")
expect_false(all(mm1$marginalmean == mm2$marginalmean))
expect_false(all(mm1$marginalmean == mm3$marginalmean))
expect_false(all(mm2$marginalmean == mm3$marginalmean))


# tidy and glance
mod <- lm(mpg ~ cyl + am + hp, dat)
me <- marginalmeans(mod)
ti <- tidy(me)
gl <- glance(me)
expect_equivalent(nrow(gl), 1)
expect_equivalent(dim(ti), c(5, 8))


# marginalmeans vs. emmeans: poisson link or response
#skip_if_not_installed("emmeans", minimum_version = "1.7.3") # transform -> regrid
dat <- mtcars
dat$am <- factor(dat$am)
dat$cyl <- factor(dat$cyl)
mod <- glm(gear ~ cyl + am, data = dat, family = poisson)
# link
mm <- tidy(marginalmeans(mod, variables = "cyl", type = "link"))
em <- tidy(emmeans(mod, specs = "cyl"))
expect_equivalent(mm$estimate, em$estimate)
expect_equivalent(mm$std.error, em$std.error)
# response
mm <- tidy(marginalmeans(mod, variables = "cyl", type = "response"))
em <- tidy(emmeans(mod, specs = "cyl", type = "response"))
expect_equivalent(mm$estimate, em$rate)
expect_equivalent(mm$p.value, em$p.value)



# simple marginal means
mod <- lm(mpg ~ cyl + am + hp, dat)
em <- broom::tidy(emmeans::emmeans(mod, "cyl"))
me <- marginalmeans(mod, variables = "cyl")
expect_equivalent(me$marginalmean, em$estimate)
expect_equivalent(me$std.error, em$std.error)
em <- broom::tidy(emmeans::emmeans(mod, "am"))
me <- marginalmeans(mod, variables = "am")
expect_equivalent(me$marginalmean, em$estimate)
expect_equivalent(me$std.error, em$std.error)


# interactions
# standard errors do not match emmeans
mod <- lm(mpg ~ cyl * am, dat)
em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "cyl")))
me <- marginalmeans(mod, variables = "cyl")
me <- me[order(me$value),]
expect_equivalent(me$marginalmean, em$estimate)
em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "am")))
me <- suppressWarnings(marginalmeans(mod, variables = "am"))
me <- me[order(me$value),]
expect_equivalent(me$marginalmean, em$estimate)


# error: no factor
mod <- lm(hp ~ mpg, mtcars)
expect_error(marginalmeans(mod), pattern = "was found")


# by argument
dat <- mtcars
dat$am <- as.logical(dat$am)
dat$vs <- factor(dat$vs)
dat$cyl <- factor(dat$cyl)
mod <- glm(gear ~ cyl + vs + am, data = dat, family = poisson)
mm <- marginalmeans(mod, by = "am")
expect_inherits(mm, "marginalmeans")
expect_equal(nrow(mm), 10)


