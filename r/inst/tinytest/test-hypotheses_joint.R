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


# Issue #981
model <- lm(mpg ~ as.factor(cyl), data = mtcars)
cmp <- avg_comparisons(model)
h1 <- hypotheses(cmp, joint = ".*")
h2 <- hypotheses(cmp, joint = "cyl")
h3 <- hypotheses(cmp, joint = TRUE)
expect_equivalent(h1$estimate, h2$estimate)
expect_equivalent(h1$estimate, h3$estimate)
expect_equivalent(h1$std.error, h2$std.error)
expect_equivalent(h1$std.error, h3$std.error)


# Issue #1214: vcov argument in joint hypotheses

# Joint test - all the same
model_lm <- lm(mpg ~ wt + vs + am, data = mtcars)
h1 <- hypotheses(model_lm, joint = c("wt", "vs", "am"))
h2 <- hypotheses(model_lm, joint = c("wt", "vs", "am"), vcov = "HC1")
h3 <- hypotheses(
    model_lm,
    joint = c("wt", "vs", "am"),
    vcov = sandwich::vcovHC(model_lm, type = "HC1")
)
expect_false(h1$statistic == h2$statistic)
expect_true(h2$statistic == h3$statistic)


# Issue #1340
mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
mfx <- slopes(mod, variables = "wt", newdata = datagrid(hp = fivenum))
h <- hypotheses(mfx, joint = 1:2)

# Test that joint hypotheses with character vector prints without error
model <- lm(mpg ~ as.factor(cyl) * hp, data = mtcars)
h <- hypotheses(model, joint = c("as.factor(cyl)6:hp", "as.factor(cyl)8:hp"))
expect_true(inherits(h, "hypotheses"))
expect_true(!is.null(attr(h, "marginaleffects")))
