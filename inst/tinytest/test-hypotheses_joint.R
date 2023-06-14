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

# regex: marginaleffects object
mod <- glm(am ~ vs + factor(carb), family = binomial, data = mtcars)
cmp <- avg_comparisons(mod)
a <- hypotheses(cmp, joint = "carb")
expect_inherits(a, "hypotheses")

# marginaleffects objects
mod <- glm(am ~ vs + factor(carb), family = binomial, data = mtcars)
cmp <- avg_comparisons(mod)
a <- hypotheses(cmp, joint_test = "f", joint = TRUE)
b <- hypotheses(cmp, joint_test = "f", joint = 2:3)
expect_true(a$p.value < b$p.value)
expect_true(a$statistic > b$statistic)

# Null hypothesis vector
mod <- glm(am ~ vs + factor(carb), family = binomial, data = mtcars)
a <- hypotheses(mod, joint = 3:4, hypothesis = 1:2)
expect_inherits(a, "hypotheses")
expect_error(hypotheses(mod, joint = 3:4, hypothesis = 1:4))

# Single parameter
mod <- glm(am ~ vs, family = binomial, data = mtcars)
a = hypotheses(mod, joint = TRUE)
expect_inherits(a, "hypotheses")


# Issue #789: incorrect degrees of freedom
lmfit <- lm(mpg~as.factor(cyl)*hp, data=mtcars)
H <- matrix(0, nrow = length(coef(lmfit)), ncol = 2)
H[5, 1] <- H[6, 2] <- 1
hyp <- hypotheses(lmfit, hypothesis = H)
h1 <- hypotheses(hyp, joint = TRUE)
h2 <- hypotheses(lmfit, joint = 5:6, joint_test = "f")
expect_equivalent(h1$df1, h2$df1)
expect_equivalent(h1$df2, h2$df2)
