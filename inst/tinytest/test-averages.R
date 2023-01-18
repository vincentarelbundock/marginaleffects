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

# hypotheses() & marginalmeans(): averages() are passthrough to summary()
mm <- marginalmeans(mod)
a <- averages(mm)
expect_inherits(a, "marginalmeans.summary")
expect_equivalent(nrow(mm), nrow(a))
hy <- hypotheses(mod, "b1 = b2")
a  <- averages(hy)
expect_inherits(a, "hypotheses.summary")
expect_equivalent(nrow(hy), nrow(a))



# expand arguments
mod <- lm(mpg ~ hp, mtcars)
cmp1 <- comparisons(mod) |> averages()
cmp2 <- comparisons(mod) |> averages(conf_level = .9)
cmp3 <- comparisons(mod) |> averages(conf_l = .9)
expect_true(all(cmp1$conf.low < cmp2$conf.low))
expect_true(all(cmp1$conf.high > cmp2$conf.high))
expect_equivalent(cmp2, cmp3)
