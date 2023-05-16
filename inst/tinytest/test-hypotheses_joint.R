source("helpers.R")
requiet("car")

model <- lm(mpg ~ as.factor(cyl) * hp, data = mtcars)
tn <- c("as.factor(cyl)6:hp", "as.factor(cyl)8:hp")

# f
a <- hypotheses(model, joint = tn, joint_test = "f")
b <- car::linearHypothesis(model, tn, test = "F")
expect_equal(a$statistic, b[["F"]][2])
expect_equal(a$p.value, b[["Pr(>F)"]][2])

# chi-squared
a <- hypotheses(model, joint = tn, joint_test = "chisq")
b <- car::linearHypothesis(model, tn, test = "Chisq")
expect_equal(a$statistic, b[["Chisq"]][2])
expect_equal(a$p.value, b[["Pr(>Chisq)"]][2])

# numeric indices
a <- hypotheses(model, joint = 5:6, joint_test = "f")
b <- car::linearHypothesis(model, tn, test = "F")
expect_equal(a$statistic, b[["F"]][2])
expect_equal(a$p.value, b[["Pr(>F)"]][2])

a <- hypotheses(model, joint = 2:3, joint_test = "f")
b <- car::linearHypothesis(model, c("as.factor(cyl)6", "as.factor(cyl)8"), test = "F")
expect_equal(a$statistic, b[["F"]][2])
expect_equal(a$p.value, b[["Pr(>F)"]][2])

# regex indices
a = hypotheses(model, joint = "cyl\\)\\d$")
expect_equal(a$statistic, 6.11733602323976)
a = hypotheses(model, joint = "cyl")
expect_equal(a$statistic, 5.70257517421252)

# marginaleffects objects
mod <- glm(am ~ vs + factor(carb), family = binomial, data = mtcars)
cmp <- avg_comparisons(mod)
a <- hypotheses(cmp, joint_test = "f", joint = TRUE)
b <- hypotheses(cmp, joint_test = "f", joint = 2:3)
expect_true(a$p.value < b$p.value)
expect_true(a$statistic > b$statistic)
