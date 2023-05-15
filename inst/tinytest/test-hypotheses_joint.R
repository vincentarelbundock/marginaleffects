source("helpers.R")
requiet("car")

model <- lm(mpg ~ as.factor(cyl) * hp, data = mtcars)
tn <- c("as.factor(cyl)6:hp", "as.factor(cyl)8:hp")

# f
a <- joint_test(model, tn, test = "f")
b <- car::linearHypothesis(model, tn, test = "F")
expect_equal(a$wald_statistic, b[["F"]][2])
expect_equal(a$p_value, b[["Pr(>F)"]][2])

# chi-squared
a = joint_test(model, tn, test = "chisq")
b = car::linearHypothesis(model, tn, test = "Chisq")
expect_equal(a$wald_statistic, b[["Chisq"]][2])
expect_equal(a$p_value, b[["Pr(>Chisq)"]][2])

# numeric indices
a <- joint_test(model, 5:6, test = "f")
b <- car::linearHypothesis(model, tn, test = "F")
expect_equal(a$wald_statistic, b[["F"]][2])
expect_equal(a$p_value, b[["Pr(>F)"]][2])

a <- joint_test(model, 2:3, test = "f")
b <- car::linearHypothesis(model, c("as.factor(cyl)6", "as.factor(cyl)8"), test = "F")
expect_equal(a$wald_statistic, b[["F"]][2])
expect_equal(a$p_value, b[["Pr(>F)"]][2])
