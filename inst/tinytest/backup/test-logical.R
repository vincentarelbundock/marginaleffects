source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")

# marginaleffects: logical
dat <- mtcars
dat$am <- as.logical(dat$am)
mod <- glm(vs ~ am + mpg, data = dat, family = binomial)
mfx <- marginaleffects(mod)
expect_inherits(mfx, "marginaleffects")
expect_equivalent(nrow(mfx), nrow(dat) * 2)

