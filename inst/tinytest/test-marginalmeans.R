source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("emmeans")
requiet("broom")

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
em <- tidy(emmeans(mod, specs = "cyl", regrid = "response"))
expect_equivalent(mm$estimate, em$rate)
expect_equivalent(mm$std.error, em$std.error, tolerance = .0001)



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
me <- me[order(me$cyl),]
expect_equivalent(me$marginalmean, em$estimate)
em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "am")))
me <- suppressWarnings(marginalmeans(mod, variables = "am"))
me <- me[order(me$am),]
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


