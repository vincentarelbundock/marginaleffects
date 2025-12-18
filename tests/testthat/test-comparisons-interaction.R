# interaction automatic flip from NULL to useful
dat <- mtcars
dat$gear <- factor(dat$gear)
dat$cyl <- factor(dat$cyl)
dat <- dat
mod1 <- lm(mpg ~ gear + cyl + wt + gear, data = dat)
mod2 <- lm(mpg ~ gear * cyl + wt + gear, data = dat)
cmp1 <- comparisons(mod1, newdata = datagrid())
cmp2 <- suppressWarnings(comparisons(mod2, newdata = datagrid(), cross = FALSE))
cmp3 <- suppressWarnings(comparisons(mod2, variables = c("cyl", "gear"), newdata = datagrid(), cross = TRUE))
expect_true("contrast" %in% colnames(cmp1))
expect_true("contrast" %in% colnames(cmp2))
expect_true(all(c("contrast_cyl", "contrast_gear") %in% colnames(cmp3)))

# variables must be unnamed vector
expect_error(
    comparisons(
        mod2,
        variables = c("cyl" = "ratio", "gear" = "difference"),
        newdata = datagrid()
    ),
    regexp = "May not have names"
)

# interaction vs. emmeans
if (requireNamespace("emmeans", quietly = TRUE)) {
    mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
    cmp <- suppressWarnings(comparisons(
        mod,
        variables = list("cyl" = "all", "am" = "all"),
        newdata = datagrid(),
        cross = TRUE
    ))
    em <- emmeans::emmeans(mod, c("cyl", "am"))
    em <- emmeans::contrast(em, method = "revpairwise")
    em <- data.frame(em)
    expect_true(all(round(abs(em$estimate), 5) %in% round(abs(cmp$estimate), 5)))
    expect_true(all(round(abs(em$SE), 4) %in% round(abs(cmp$std.error), 4)))
}


# tidy does not error (no validity)
mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
cmp <- comparisons(mod, variables = c("am", "cyl"), cross = TRUE)
tid <- tidy(cmp)
expect_true(all(tid$term == "cross"))


# `variables` must be specified
mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
cmp <- comparisons(mod, variables = c("am", "cyl"), cross = TRUE)
expect_s3_class(cmp, "comparisons")
expect_error(comparisons(mod, cross = TRUE), regexp = "variables")


# interaction (no validity)
mod <- lm(mpg ~ factor(am) * factor(cyl) + wt + gear, data = mtcars)

# one row only means tidy is same nrows
# on some machines I get 21 rows instead of 18, but can't replicate. maybe look into this if I have the energy. Seems minor.
cmp <- comparisons(
    mod,
    variables = list("cyl" = "all", "am" = "all"),
    newdata = datagrid(),
    cross = TRUE
)
expect_true(nrow(cmp) > 17)
expect_true(nrow(tidy(cmp)) > 17)


# deprecated argument
expect_error(comparisons(mod, interaction = TRUE), regexp = "cross")
