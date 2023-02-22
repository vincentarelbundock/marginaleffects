source("helpers.R")
using("marginaleffects")
requiet("prediction")
requiet("insight")


# lm: Average prediction vs. {prediction}
mod <- lm(am ~ mpg + drat + factor(cyl), data = mtcars)
pre <- predictions(mod)
tid <- tidy(pre)
expect_equal(nrow(tid), 1)
expect_equal(mean(pre$estimate), tid$estimate)
lee <- data.frame(summary(prediction::prediction(mod)))
expect_equivalent(tid$estimate, lee$Prediction)
expect_equivalent(tid$std.error, lee$SE)

# lm: Group-Average Prediction (no validity)
pre <- predictions(mod, by = "cyl")
tid <- tidy(pre)
expect_equal(nrow(tid), 3)

# glm response scale
# CI retrieved by `insight::get_predicted()` for units
# CI not supported yet for `tidy()`
mod <- glm(am ~ mpg + drat + factor(cyl), data = mtcars, family = binomial)
pre <- predictions(mod, type = "link")
tid <- tidy(pre)
lee <- data.frame(summary(prediction::prediction(mod, type = "link")))
ins <- data.frame(insight::get_predicted(mod, ci = .95, predict = "link"))
expect_equivalent(tid$estimate, lee$Prediction)
expect_equivalent(pre$estimate, ins$Predicted)
expect_equivalent(pre$conf.low, ins$CI_low)
expect_equivalent(pre$conf.high, ins$CI_high)
expect_true("std.error" %in% colnames(pre))
expect_true("conf.low" %in% colnames(pre))

# glm link scale: CI fully supported
mod <- glm(am ~ mpg + drat + factor(cyl), data = mtcars, family = binomial)
pre <- predictions(mod, type = "link")
tid <- tidy(pre)
lee <- data.frame(summary(prediction::prediction(mod, type = "link")))
ins <- data.frame(insight::get_predicted(mod, predict = "link", ci = .95))
expect_equivalent(tid$estimate, lee$Prediction)
expect_equivalent(tid$std.error, lee$SE)
expect_equivalent(tid$conf.low, lee$lower)
expect_equivalent(tid$conf.high, lee$upper)
expect_equivalent(tid$estimate, lee$Prediction)
expect_equivalent(pre$estimate, ins$Predicted)
expect_equivalent(pre$std.error, ins$SE)
expect_equivalent(pre$conf.low, ins$CI_low)
expect_equivalent(pre$conf.high, ins$CI_high)



rm(list = ls())