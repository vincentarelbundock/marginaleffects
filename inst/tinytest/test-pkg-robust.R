source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("robust")

# no validity
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod <- lmRob(mpg ~ hp + cyl, data = mtcars)
expect_marginaleffects(mod, n_unique = 1)
expect_predictions(predictions(mod))
