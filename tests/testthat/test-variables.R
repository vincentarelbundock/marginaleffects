testthat::skip_if_not_installed("ordinal")
requiet("ordinal")

tmp_variables <- mtcars
tmp_variables$gear <- as.factor(tmp_variables$gear)
tmp_variables$cyl <- as.factor(tmp_variables$cyl)
mod <- lm(mpg ~ gear + cyl + hp, data = tmp_variables)

cmp1 <- comparisons(mod, variables = "hp", newdata = head(tmp_variables, 1))
expect_equal(cmp1$term, "hp", ignore_attr = TRUE)
expect_equal(cmp1$contrast, "+1", ignore_attr = TRUE)

cmp2 <- comparisons(mod, variables = list("hp" = 1), newdata = head(tmp_variables, 1))
expect_equal(cmp1, cmp2, ignore_attr = TRUE)

cmp1 <- avg_comparisons(
    mod,
    variables = list(gear = "sequential", hp = 10, cyl = "pairwise")
) |>
    dplyr::arrange(term, contrast)
cmp2 <- avg_comparisons(
    mod,
    variables = list(gear = "sequential", hp = 1, cyl = "pairwise")
) |>
    dplyr::arrange(term, contrast)
# known <- c("4 - 3", "5 - 4", "+10", "6 - 4", "8 - 4", "8 - 6")
# aggregate refactor gave us new labels
known <- c("+10", "4 - 3", "5 - 4", "6 - 4", "8 - 4", "8 - 6")
expect_true(all(known %in% cmp1$contrast))
expect_equal(cmp1$estimate[6], cmp2$estimate[6] * 10, tolerance = 1e-6, ignore_attr = TRUE)

# informative errors
expect_error(suppressWarnings(comparisons(mod, variables = list(gear = "blah"))), regexp = "variables")
expect_error(suppressWarnings(comparisons(mod, variables = list(hp = "pairwise"))), regexp = "variables")

# regression test: factor in formula and numeric check
mod <- lm(mpg ~ factor(cyl), data = mtcars)
expect_s3_class(comparisons(mod, variables = list(cyl = "pairwise")), "comparisons")
expect_error(comparisons(mod, variables = list(cyl = "iqr")), regexp = "element")


# Binary variables
mod <- glm(am ~ hp + vs, dat = mtcars, family = binomial)
expect_error(comparisons(mod, variables = list(vs = 1), "length 2"))

# no need to include categorical focal variable when there is only one of them
mod <- lm(mpg ~ hp + factor(am) + wt, mtcars)
nd <- data.frame(hp = 120, am = 1)
expect_error(comparisons(mod, variables = "wt", newdata = nd), regexp = "newdata")
nd <- data.frame(hp = 120, am = 0, wt = 2.5)
cmp <- comparisons(mod, variables = "am", newdata = nd)
expect_s3_class(cmp, "comparisons")


# comparisons() variables = data.frame()
mod <- lm(mpg ~ hp, mtcars)
comparisons(mod, variables = list(hp = data.frame(mtcars$hp, mtcars$hp + 1:32)))


# Issue #757: rev
mod <- lm(mpg ~ factor(cyl), mtcars)
a <- avg_comparisons(mod, variables = list(cyl = "pairwise"))
b <- avg_comparisons(mod, variables = list(cyl = "revpairwise"))
expect_equal(a$estimate, -1 * b$estimate)
a <- avg_comparisons(mod, variables = list(cyl = "reference"))
b <- avg_comparisons(mod, variables = list(cyl = "revreference"))
expect_equal(a$estimate, -1 * b$estimate)
a <- avg_comparisons(mod, variables = list(cyl = "sequential"))
b <- avg_comparisons(mod, variables = list(cyl = "revsequential"))
expect_equal(a$estimate, -1 * b$estimate)


# Custom vector
mod <- lm(mpg ~ hp, mtcars)
cmp <- avg_comparisons(mod, variables = list(hp = \(x) data.frame(mtcars$hp, mtcars$cyl)), by = "cyl")
expect_equal(length(unique(cmp$estimate)), 3)
expect_equal(length(unique(round(cmp$statistic, 5))), 1)


# Issue 953: Custom functions or data frames for factors/logical columns
dat_variables <- wine |>
    transform(
        rating = ordered(ifelse(rating == 5, 1, rating)),
        temp = as.character(temp)
    )
mod <- clm(rating ~ response * temp, data = dat_variables)

DF = data.frame(
    lo = dat_variables$temp,
    hi = ifelse(dat_variables$temp == "cold", "warm", "cold")
)

cmp <- comparisons(mod, variables = list(temp = DF))
expect_s3_class(cmp, "comparisons")
p1 <- predictions(mod)
p2 <- predictions(mod, newdata = transform(dat_variables, temp = ifelse(temp == "cold", "warm", "cold")))
expect_equal(p2$estimate - p1$estimate, cmp$estimate)

cmp <- avg_comparisons(mod, variables = list(temp = DF))
expect_s3_class(cmp, "comparisons")
expect_equal(nrow(cmp), 4)

dat_variables$temp <- dat_variables$temp == "cold"
mod <- clm(rating ~ response * temp, data = dat_variables)

DF = data.frame(
    lo = dat_variables$temp,
    hi = ifelse(dat_variables$temp == FALSE, TRUE, FALSE)
)
cmp <- comparisons(mod, variables = list(temp = DF))
expect_s3_class(cmp, "comparisons")
p1 <- predictions(mod)
p2 <- predictions(mod, newdata = transform(dat_variables, temp = ifelse(temp == FALSE, TRUE, FALSE)))
expect_equal(p2$estimate - p1$estimate, cmp$estimate)

cmp <- avg_comparisons(mod, variables = list(temp = DF))
expect_s3_class(cmp, "comparisons")
expect_equal(nrow(cmp), 4)

DF = \(x)
data.frame(
    lo = x,
    hi = ifelse(x == FALSE, TRUE, FALSE)
)
cmp <- comparisons(mod, variables = list(temp = DF))
expect_s3_class(cmp, "comparisons")
expect_equal(nrow(cmp), 288)
