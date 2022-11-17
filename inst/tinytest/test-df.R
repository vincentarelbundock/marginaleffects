requiet("marginaleffects")
requiet("emmeans")

dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.factor(dat$am)
mod <- lm(mpg ~ cyl, data = dat)

em <- emmeans(mod, ~ cyl)
em <- confint(pairs(em), adjust = "none")

mm <- marginalmeans(
    mod,
    variables = "cyl",
    hypothesis = "pairwise",
    df = insight::get_df(mod),
    conf_level = 0.95)

expect_equivalent(em$estimate, mm$marginalmean)
expect_equivalent(em$lower.CL, mm$conf.low)
expect_equivalent(em$upper.CL, mm$conf.high)
