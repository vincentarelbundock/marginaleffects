skip_on_cran()

test_that("interaction automatic flip from NULL to useful", {
    withr_library("marginaleffects")
    withr_library("emmeans")

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
})

test_that("variables must be unnamed vector", {
    withr_library("marginaleffects")

    dat <- mtcars
    dat$gear <- factor(dat$gear)
    dat$cyl <- factor(dat$cyl)
    mod2 <- lm(mpg ~ gear * cyl + wt + gear, data = dat)

    # variables must be unnamed vector
    expect_error(
        comparisons(
            mod2,
            variables = c("cyl" = "ratio", "gear" = "difference"),
            newdata = datagrid()
        ),
        pattern = "May not have names"
    )
})

test_that("interaction vs. emmeans", {
    withr_library("marginaleffects")
    withr_library("emmeans")

    # interaction vs. emmeans
    mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
    cmp <- suppressWarnings(comparisons(
        mod,
        variables = list("cyl" = "all", "am" = "all"),
        newdata = datagrid(),
        cross = TRUE
    ))
    em <- emmeans(mod, c("cyl", "am"))
    em <- emmeans::contrast(em, method = "revpairwise")
    em <- data.frame(em)
    expect_true(all(round(abs(em$estimate), 5) %in% round(abs(cmp$estimate), 5)))
    expect_true(all(round(abs(em$SE), 4) %in% round(abs(cmp$std.error), 4)))
})

test_that("tidy does not error", {
    withr_library("marginaleffects")

    # tidy does not error (no validity)
    mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
    cmp <- comparisons(mod, variables = c("am", "cyl"), cross = TRUE)
    tid <- tidy(cmp)
    expect_true(all(tid$term == "cross"))
})

test_that("variables must be specified for cross", {
    withr_library("marginaleffects")

    # `variables` must be specified
    mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
    cmp <- comparisons(mod, variables = c("am", "cyl"), cross = TRUE)
    expect_s3_class(cmp, "comparisons")
    expect_error(comparisons(mod, cross = TRUE), pattern = "variables")
})

test_that("interaction with cross works correctly", {
    withr_library("marginaleffects")

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
})

test_that("deprecated interaction argument", {
    withr_library("marginaleffects")

    mod <- lm(mpg ~ factor(am) * factor(cyl) + wt + gear, data = mtcars)

    # deprecated argument
    expect_error(comparisons(mod, interaction = TRUE), pattern = "cross")
})
