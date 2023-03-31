source("helpers.R")
using("marginaleffects")
if (!requiet("tinysnapshot")) exit_file("tinysnapshot")
# if (ON_WINDOWS || ON_OSX) exit_file("linux only")
using("tinysnapshot")
requiet("nnet")

# Issue #567: threenum and minmax are mixed up
dat <- transform(mtcars, am_fct = factor(am))
mod <- lm(wt ~ am_fct * mpg, data = dat)


# minmax
p1 <- plot_predictions(
    mod,
    draw = FALSE,
    condition = list("am_fct", mpg = "minmax")) 
p2 <- predictions(
    mod,
    newdata = datagrid(mpg = range, am_fct = 0:1))
p2$am_fct <- as.numeric(as.character(p2$am_fct))
p2 <- p2[order(-p2$am_fct, p2$mpg),]
expect_equivalent(p1$estimate, p2$estimate)

p1$condition1 <- as.character(p1$am_fct)
p1$condition2 <- as.character(p1$mpg)

x <- p1[p1$condition1 == "1" & p1$condition2 == "Min", "estimate"]
y <- p2[p2$am_fct == 1 & p2$mpg == 10.4, "estimate"]
expect_equivalent(x, y)

# threenum
threenum <<- c(
    mean(dat$mpg) - sd(dat$mpg),
    mean(dat$mpg),
    mean(dat$mpg) + sd(dat$mpg))

p1 <- plot_predictions(
    mod,
    draw = FALSE,
    condition = list("am_fct", mpg = "threenum")) 
p2 <- predictions(
    mod,
    newdata = datagrid(mpg = threenum, am_fct = 0:1))
p2$am_fct <- as.numeric(as.character(p2$am_fct))
p2 <- p2[order(-p2$am_fct, p2$mpg),]
expect_equivalent(p1$estimate, p2$estimate)


# Issue #550
x <- abs(rnorm(100, sd = 5)) + 5
y <- exp(2 + 0.3 * x + rnorm(100, sd = 0.4))
dat <- data.frame(x = x, y = y)
dat[["log_x"]] <- log(x)
dat[["log_y"]] <- log(y)
model <- lm(log(y) ~ 1 + log(x), data = dat)
p <- plot_predictions(model, condition = "x", draw = FALSE)
expect_false(any(is.na(p$estimate)))
expect_equal(nrow(p), 50)


# points: alpha
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
p <- plot_predictions(mod, condition = list("hp", "wt" = "threenum"), points = .5)
expect_snapshot_plot(p, "plot_predictions-alpha")


# two conditions
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
p <- plot_predictions(mod, condition = c("hp", "wt"))
expect_snapshot_plot(p, "plot_predictions")

# gray scale
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
p <- plot_predictions(mod, condition = c("hp", "wt"), gray = TRUE)
expect_snapshot_plot(p, "plot_predictions-gray")

# continuous vs. categorical x-axis
mod <- lm(mpg ~ hp * wt * factor(cyl), mtcars)
p <- plot_predictions(mod, condition = c("cyl", "wt"))
expect_snapshot_plot(p, "plot_predictions_vs_categorical_x_axis")
p <- plot_predictions(mod, condition = c("wt", "cyl"))
expect_snapshot_plot(p, "plot_predictions_vs_continuous_x_axis")


# conf.level in plots
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
p1 <- plot_predictions(mod, condition = "hp", conf.level = .99)
p2 <- plot_predictions(mod, condition = "hp", conf.level = .4)
expect_snapshot_plot(p1, "plot_predictions_conf_99")
expect_snapshot_plot(p2, "plot_predictions_conf_40")


# link vs response
mod <- glm(am ~ hp + wt, data = mtcars, family = binomial)
p1 <- plot_predictions(mod, condition = "hp", type = "response")
p2 <- plot_predictions(mod, condition = "hp", type = "link")
expect_snapshot_plot(p1, "plot_predictions_response")
expect_snapshot_plot(p2, "plot_predictions_link")


# bad condition raises error
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
expect_error(plot_predictions(mod, condition = c("bad", "wt")))


# Issue #230: glm w/ weights includes confidence intervals
mod <- glm(am ~ mpg * cyl, data = mtcars, family = binomial(link = "logit"), weights = carb)
p <- plot_predictions(mod, condition = c("mpg", "cyl"), draw = FALSE)
expect_true("conf.low" %in% colnames(p))
expect_true("conf.high" %in% colnames(p))


# vcov
#skip_if_not_installed("insight", minimum_version = "0.17.1")
mod <- lm(mpg ~ hp * wt, data = mtcars)
mfx0 <- plot_predictions(mod, condition = "wt", vcov = FALSE, draw = FALSE)
mfx1 <- plot_predictions(mod, condition = "wt", draw = FALSE)
mfx2 <- plot_predictions(mod, condition = "wt", vcov = "HC3", draw = FALSE)
mfx3 <- plot_predictions(mod, condition = "wt", vcov = ~cyl, draw = FALSE)
expect_false("conf.low" %in% colnames(mfx0))
expect_true(all(mfx1$std.error != mfx2$std.error))
expect_true(all(mfx1$std.error != mfx3$std.error))
expect_true(all(mfx2$std.error != mfx3$std.error))
expect_true(all(mfx1$conf.low != mfx2$conf.low))
expect_true(all(mfx1$conf.low != mfx3$conf.low))
expect_true(all(mfx2$conf.low != mfx3$conf.low))



# multinomial
mod <- nnet::multinom(factor(gear) ~ mpg * wt + am, data = mtcars, trace = FALSE)
p1 <- plot_predictions(mod, condition = c("mpg", "group"), type = "probs")
p2 <- plot_comparisons(mod, variables = "mpg", condition = c("wt", "group"), type = "probs")
p3 <- plot_slopes(mod, variables = "mpg", condition = c("wt", "group"), type = "probs")
expect_inherits(p1, "gg")
expect_inherits(p2, "gg")
expect_inherits(p3, "gg")



# Issue #498: New features
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
# condition list must be named or single characters
p <- plot_predictions(mod, condition = list("hp", "wt" = c(1.5, 2.5, 3.5), "am" = 0:1))
expect_inherits(p, "gg")
p <- plot_predictions(mod, condition = list("hp" = seq(110, 140), "wt" = c(1.5, 2.5, 3.5)))
expect_inherits(p, "gg")
p <- plot_predictions(mod, condition = list("hp", "wt" = "threenum", "am" = "minmax"))
expect_inherits(p, "gg")
expect_error(
    plot_predictions(mod, condition = list(100:110, "wt" = c(1.5, 2.5, 3.5))),
    pattern = "condition")


# backward compatibility
dat <- transform(mtcars, am_fct = factor(am))
mod <- lm(wt ~ am_fct * mpg, data = dat)

p1 <- plot_cap(
    mod,
    condition = list("am_fct", mpg = "minmax")) 
expect_inherits(p1, "gg")



# Issue #609: `plot_*(draw=FALSE)` returns original column names instead of `condition1`
mod <- lm(mpg ~ hp * qsec * factor(gear), data = mtcars)
p <- plot_predictions(mod, condition = list("hp", "qsec", "gear"))
expect_inherits(p, "gg")
p <- plot_predictions(mod, condition = c("hp", "qsec", "gear"))
expect_inherits(p, "gg")
p <- plot_predictions(mod, condition = list("hp", "qsec" = "minmax"))
expect_inherits(p, "gg")

p <- plot_predictions(mod, condition = list("hp", "qsec" = "minmax", "gear"), draw = FALSE)
expect_true("qsec" %in% colnames(p))
p <- plot_comparisons(mod, variables = "hp", condition = list("qsec" = "minmax", "gear"), draw = FALSE)
expect_true("qsec" %in% colnames(p))
p <- plot_slopes(mod, variables = "hp", condition = list("qsec" = "minmax", "gear"), draw = FALSE)
expect_true("qsec" %in% colnames(p))



# Issue #725: `newdata` argument in plotting functions
mod <- lm(mpg ~ hp + am + factor(cyl), mtcars)
p1 <- plot_predictions(mod, by = "am", newdata = datagridcf(am = 0:1), draw = FALSE)
p2 <- avg_predictions(mod, by = "am", newdata = datagridcf(am = 0:1), draw = FALSE)
expect_equivalent(p1$estimate, p2$estimate)
expect_equivalent(p1$conf.low, p2$conf.low)
p3 <- plot_predictions(mod, by = "am", draw = FALSE)
p4 <- avg_predictions(mod, by = "am", draw = FALSE)
expect_equivalent(p3$estimate, p4$estimate)
expect_equivalent(p3$conf.low, p4$conf.low)
expect_true(all(p1$conf.low != p3$conf.low))
p5 <- plot_predictions(mod, condition = "am", draw = FALSE)
p6 <- predictions(mod, newdata = datagrid(am = 0:1))
expect_equivalent(p5$estimate, p6$estimate)
expect_equivalent(p5$conf.low, p6$conf.low)
expect_true(all(p1$conf.low != p5$conf.low))
expect_true(all(p3$conf.low != p5$conf.low))
expect_error(plot_predictions(mod, condition = "am", by = "am"))
expect_error(plot_predictions(mod, newdata = mtcars))





suppressWarnings(rm("threenum", .GlobalEnv))
rm(list = ls())