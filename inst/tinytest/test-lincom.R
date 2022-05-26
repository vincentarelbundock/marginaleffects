source("helpers.R")
requiet("emmeans")

dat <- mtcars
dat$carb <- factor(dat$carb)
dat$cyl <- factor(dat$cyl)

# Marginal means
mod <- lm(mpg ~ carb + cyl, dat)
lc <- c(-2, 1, 1, 0, -1, 1)
em <- emmeans(mod, "carb") 
em <- contrast(em, method = data.frame(custom_contrast = lc))
em <- data.frame(em)
mm <- marginalmeans(m, variables = "carb", lincom = lc)
expect_equivalent(mm$marginalmean, em$estimate)
expect_equivalent(mm$std.error, em$SE)
