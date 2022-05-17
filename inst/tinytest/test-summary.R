source("helpers.R")
if (ON_CRAN) exit_file("on cran")
requiet("dplyr")

# simple summary output
mod <- lm(mpg ~ hp + factor(cyl), mtcars)
mfx <- marginaleffects(mod)
s <- summary(mfx)
# expect_snapshot(print(summary(mfx), digits = 3))


# summary conf.level
mod <- lm(mpg ~ hp + factor(cyl), mtcars)
mfx <- marginaleffects(mod)
# expect_snapshot(print(summary(mfx, conf.level = .9)))
# expect_snapshot(print(summary(mfx, conf.level = .2)))


# summary: marginal means
dat <- mtcars
dat$am <- as.logical(dat$am)
dat$vs <- as.logical(dat$vs)
dat$gear <- as.factor(dat$gear)
mod <- lm(mpg ~ gear + am + vs, dat)
mm <- marginalmeans(mod)
s <- summary(mm)
# expect_snapshot(print(s))


# bugs stay dead: summary manipulation
mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
mfx <- marginaleffects(mod)
# expect_snapshot(
#     summary(mfx) %>%
#     dplyr::select(term, estimate, conf.low, conf.high)
# )
