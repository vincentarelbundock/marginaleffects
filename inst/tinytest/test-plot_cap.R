source("helpers.R")
using("marginaleffects")
exit_if_not(!ON_OSX)

using("tinyviztest")
requiet("nnet")

# Issue #567: threenum and minmax are mixed up
dat <<- transform(mtcars, am_fct = factor(am))
mod <- lm(wt ~ am_fct * mpg, data = dat)

# minmax
p1 <- plot_cap(
    mod,
    draw = FALSE,
    condition = list("am_fct", mpg = "minmax")) 
p2 <- predictions(
    mod,
    newdata = datagrid(mpg = range, am_fct = 0:1))
p2$am_fct <- as.numeric(as.character(p2$am_fct))
p2 <- p2[order(-p2$am_fct, p2$mpg),]
expect_equivalent(p1$predicted, p2$predicted)

p1$condition1 <- as.character(p1$condition1)
p1$condition2 <- as.character(p1$condition2)

x <- p1[p1$condition1 == "1" & p1$condition2 == "Min", "predicted"]
y <- p2[p2$am_fct == 1 & p2$mpg == 10.4, "predicted"]
expect_equivalent(x, y)

# threenum
threenum <- c(
    mean(dat$mpg) - sd(dat$mpg),
    mean(dat$mpg),
    mean(dat$mpg) + sd(dat$mpg))

p1 <- plot_cap(
    mod,
    draw = FALSE,
    condition = list("am_fct", mpg = "threenum")) 
p2 <- predictions(
    mod,
    newdata = datagrid(mpg = threenum, am_fct = 0:1))
p2$am_fct <- as.numeric(as.character(p2$am_fct))
p2 <- p2[order(-p2$am_fct, p2$mpg),]
expect_equivalent(p1$predicted, p2$predicted)


# Issue #550
x <- abs(rnorm(100, sd = 5)) + 5
y <- exp(2 + 0.3 * x + rnorm(100, sd = 0.4))
dat <- data.frame(x = x, y = y)
dat[["log_x"]] <- log(x)
dat[["log_y"]] <- log(y)
model <- lm(log(y) ~ 1 + log(x), data = dat)
p <- plot_cap(model, condition = "x", draw = FALSE)
expect_false(any(is.na(p$predicted)))
expect_equal(nrow(p), 25)



######################################
######################################



# two conditions
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
p <- plot_cap(mod, condition = c("hp", "wt"))
expect_vdiff(p, "plot_cap")



# continuous vs. categorical x-axis
mod <- lm(mpg ~ hp * wt * factor(cyl), mtcars)
p <- plot_cap(mod, condition = c("cyl", "wt"))
expect_vdiff(p, "plot_cap_vs_categorical_x_axis")
p <- plot_cap(mod, condition = c("wt", "cyl"))
expect_vdiff(p, "plot_cap_vs_continuous_x_axis")


# conf.level in plots
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
p1 <- plot_cap(mod, condition = "hp", conf.level = .99)
p2 <- plot_cap(mod, condition = "hp", conf.level = .4)
expect_vdiff(p1, "plot_cap_conf_99")
expect_vdiff(p2, "plot_cap_conf_40")


# link vs response
mod <- glm(am ~ hp + wt, data = mtcars, family = binomial)
p1 <- plot_cap(mod, condition = "hp", type = "response")
p2 <- plot_cap(mod, condition = "hp", type = "link")
expect_vdiff(p1, "plot_cap_response")
expect_vdiff(p2, "plot_cap_link")


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
mod <- nnet::multinom(factor(gear) ~ mpg * wt + am, data = mtcars, trace = FALSE)
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


