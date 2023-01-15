source("helpers.R")
using("marginaleffects")

tmp <- mtcars
tmp$gear <- as.factor(tmp$gear)
tmp$cyl <- as.factor(tmp$cyl)
mod <- lm(mpg ~ gear + cyl + hp, data = tmp)

cmp1 <- comparisons(mod, variables = "hp", newdata = head(tmp, 1))
expect_equivalent(cmp1$term, "hp")
expect_equivalent(cmp1$contrast, "+1")

cmp2 <- comparisons(mod, variables = list("hp" = 1), newdata = head(tmp, 1))
expect_equivalent(cmp1, cmp2)

cmp1 <- comparisons(
    mod,
    variables = list(gear = "sequential", hp = 10, cyl = "pairwise"))
cmp1 <- tidy(cmp1)
cmp2 <- comparisons(
    mod,
    variables = list(gear = "sequential", hp = 1, cyl = "pairwise"))
cmp2 <- tidy(cmp2)
# known <- c("4 - 3", "5 - 4", "+10", "6 - 4", "8 - 4", "8 - 6")
# aggregate refactor gave us new labels
known <- c("mean(+10)", "mean(4) - mean(3)", "mean(5) - mean(4)", "mean(6) - mean(4)", 
"mean(8) - mean(4)", "mean(8) - mean(6)")
expect_true(all(known %in% cmp1$contrast))
expect_equivalent(cmp1$estimate[3], cmp2$estimate[3] * 10)

# informative errors
expect_error(suppressWarnings(comparisons(mod, variables = list(gear = "blah"))), pattern = "variables")
expect_error(suppressWarnings(comparisons(mod, variables = list(hp = "pairwise"))), pattern = "variables")

# regression test: factor in formula and numeric check
mod <- lm(mpg ~ factor(cyl), data = mtcars)
expect_inherits(comparisons(mod, variables = list(cyl = "pairwise")), "comparisons")
expect_error(comparisons(mod, variables = list(cyl = "iqr")), pattern = "element")


# Binary variables
mod <- glm(am ~ hp + vs, dat = mtcars, family = binomial)
cmp1 <- comparisons(mod, variables = "vs")
cmp2 <- comparisons(mod, variables = list(vs = 0:1))
cmp3 <- comparisons(mod, variables = list(vs = 1))
mfx <- slopes(mod, variables = "vs")
expect_equivalent(cmp1$estimate, cmp2$estimate)
expect_true(all(cmp1$estimate != mfx$estimate))
expect_true(all(cmp1$estimate != cmp3$estimate))


# Issue #582: sanitize_variables should reject reponse as 
mod <- lm(mpg ~ hp + qsec, data = mtcars)
expect_error(slopes(mod, variables = "mpg"), pattern = "outcome variable")