source("helpers.R")
using("marginaleffects")

mod <<- lm(mpg ~ (qsec + drat) * am, data = mtcars)
mfx1 <- slopes(mod) |> tidy(by = "am")
mfx2 <- tidy(slopes(mod, by = "am"))
expect_equal(nrow(mfx1), nrow(mfx2))


dat <- mtcars
dat$am <- as.logical(dat$am)
dat$carb <- as.factor(dat$carb)
mod <- lm(mpg ~ hp + am + carb, data = dat)

# hypotheses() & marginal_means(): tidy() are passthrough to summary()
mm <- marginal_means(mod)
a <- tidy(mm)
expect_inherits(a, "tbl_df")
expect_equivalent(nrow(mm), nrow(a))
hy <- hypotheses(mod, "b1 = b2")
a  <- tidy(hy)
expect_inherits(a, "tbl_df")
expect_equivalent(nrow(hy), nrow(a))



# expand arguments
mod <- lm(mpg ~ hp, mtcars)
cmp1 <- comparisons(mod) |> tidy()
cmp2 <- comparisons(mod) |> tidy(conf_level = .9)
cmp3 <- comparisons(mod) |> tidy(conf_l = .9)
expect_true(all(cmp1$conf.low < cmp2$conf.low))
expect_true(all(cmp1$conf.high > cmp2$conf.high))
expect_equivalent(cmp2, cmp3)



# datagrid() catch scall() 
pre <- avg_predictions(mod, newdata = datagrid())
cmp <- avg_comparisons(mod, newdata = datagrid())
mfx <- avg_slopes(mod, newdata = datagrid())
expect_inherits(pre, "predictions")
expect_inherits(cmp, "comparisons")
expect_inherits(mfx, "slopes")