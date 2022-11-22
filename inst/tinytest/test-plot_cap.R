exit_file("tinyviztest")
source("helpers.R")
using("tinyviztest")
requiet("nnet")

# two conditions
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
p <- plot_cap(mod, condition = c("hp", "wt"))
expect_vdiff(p, "plot_cap")


# continuous vs. categorical x-axis
mod <- lm(mpg ~ hp * wt * factor(cyl), mtcars)
p <- plot_cap(mod, condition = c("cyl", "wt"))
expect_vdiff(p, "plot_cap vs categorical x-axis")
p <- plot_cap(mod, condition = c("wt", "cyl"))
expect_vdiff(p, "plot_cap vs continuous x-axis")


# conf.level in plots
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
p1 <- plot_cap(mod, condition = "hp", conf.level = .99)
p2 <- plot_cap(mod, condition = "hp", conf.level = .4)
expect_vdiff(p1, "plot_cap conf 99")
expect_vdiff(p2, "plot_cap conf 40")


# link vs response
mod <- glm(am ~ hp + wt, data = mtcars, family = binomial)
p1 <- plot_cap(mod, condition = "hp", type = "response")
p2 <- plot_cap(mod, condition = "hp", type = "link")
expect_vdiff(p1, "plot_cap response")
expect_vdiff(p2, "plot_cap link")


# bad condition raises error
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
expect_error(plot_cap(mod, condition = c("bad", "wt")))


# Issue #230: glm w/ weights includes confidence intervals
mod <- glm(am ~ mpg * cyl, data = mtcars, family = binomial(link = "logit"), weights = carb)
p <- plot_cap(mod, condition = c("mpg", "cyl"), draw = FALSE)
expect_true("conf.low" %in% colnames(p))
expect_true("conf.high" %in% colnames(p))


# vcov
#skip_if_not_installed("insight", minimum_version = "0.17.1")
mod <- lm(mpg ~ hp * wt, data = mtcars)
mfx0 <- plot_cap(mod, condition = "wt", vcov = FALSE, draw = FALSE)
mfx1 <- plot_cap(mod, condition = "wt", draw = FALSE)
mfx2 <- plot_cap(mod, condition = "wt", vcov = "HC3", draw = FALSE)
mfx3 <- plot_cap(mod, condition = "wt", vcov = ~cyl, draw = FALSE)
expect_false("conf.low" %in% colnames(mfx0))
expect_true(all(mfx1$std.error != mfx2$std.error))
expect_true(all(mfx1$std.error != mfx3$std.error))
expect_true(all(mfx2$std.error != mfx3$std.error))
expect_true(all(mfx1$conf.low != mfx2$conf.low))
expect_true(all(mfx1$conf.low != mfx3$conf.low))
expect_true(all(mfx2$conf.low != mfx3$conf.low))



# multinomial
mod <- multinom(factor(gear) ~ mpg * wt + am, data = mtcars, trace = FALSE)
p1 <- plot_cap(mod, condition = c("mpg", "group"), type = "probs")
p2 <- plot_cco(mod, effect = "mpg", condition = c("wt", "group"), type = "probs")
p3 <- plot_cme(mod, effect = "mpg", condition = c("wt", "group"), type = "probs")
expect_inherits(p1, "gg")
expect_inherits(p2, "gg")
expect_inherits(p3, "gg")



# Issue #498: New features
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
# condition list must be named or single characters
p <- plot_cap(mod, condition = list("hp", "wt" = c(1.5, 2.5, 3.5), "am" = 0:1))
expect_inherits(p, "gg")
p <- plot_cap(mod, condition = list("hp" = seq(110, 140), "wt" = c(1.5, 2.5, 3.5)))
expect_inherits(p, "gg")
p <- plot_cap(mod, condition = list("hp", "wt" = "threenum", "am" = "minmax"))
expect_inherits(p, "gg")
expect_error(
    plot_cap(mod, condition = list(100:110, "wt" = c(1.5, 2.5, 3.5))),
    pattern = "condition")