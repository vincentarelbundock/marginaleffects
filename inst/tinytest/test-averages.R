source("helpers.R")
using("marginaleffects")

mod <<- lm(mpg ~ (qsec + drat) * am, data = mtcars)
mfx1 <- slopes(mod) |> averages(by = "am")
mfx2 <- averages(slopes(mod, by = "am"))
expect_equal(nrow(mfx1), nrow(mfx2))
expect_error(averages(slopes(mod, by = "am"), by = "cyl"), pattern = "twice")



dat <- mtcars
dat$am <- as.logical(dat$am)
dat$carb <- as.factor(dat$carb)
mod <- lm(mpg ~ hp + am + carb, data = dat)

# Compute and summarize marginal means
mm <- marginalmeans(mod)
a <- averages(mm)
expect_inherits(a, "marginalmeans.summary")
expect_equivalent(nrow(mm), nrow(a))