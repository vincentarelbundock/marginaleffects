test_that("interaction automatic flip from NULL to useful", {
    dat <- mtcars
    dat$gear <- factor(dat$gear)
    dat$cyl <- factor(dat$cyl)
    mod1 <- lm(mpg ~ gear + cyl + wt + gear, data = dat)
    mod2 <- lm(mpg ~ gear * cyl + wt + gear, data = dat)
    cmp1 <- comparisons(mod1, newdata = datagrid())
    cmp2 <- suppressWarnings(comparisons(mod2, newdata = datagrid()))
    cmp3 <- suppressWarnings(comparisons(mod2, variables = c("cyl", "gear"), newdata = datagrid()))
    expect_true("contrast" %in% colnames(cmp1))
    expect_true("contrast" %in% colnames(cmp2))
    expect_true(all(c("contrast_cyl", "contrast_gear") %in% colnames(cmp3)))
})


test_that("interaction vs. emmeans", {
    requiet("emmeans")
    mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
    cmp <- suppressWarnings(comparisons(
        mod,
        variables = c("cyl", "am"),
        newdata = datagrid(),
        contrast_factor = "all",
        interaction = TRUE))
    em <- emmeans(mod, c("cyl", "am"))
    em <- contrast(em, method = "revpairwise")
    em <- data.frame(em)
    expect_true(all(round(abs(em$estimate), 5) %in% round(abs(cmp$comparison), 5)))
    expect_true(all(round(abs(em$SE), 5) %in% round(abs(cmp$std.error), 5)))
})


test_that("tidy does not error (no validity)", {
    mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
    cmp <- comparisons(mod, variables = c("am", "cyl"), interaction = TRUE)
    tid <- tidy(cmp)
    expect_true(all(tid$term == "interaction"))
})


test_that("`variables` must be specified", {
    mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
    expect_error(comparisons(mod, variables = c("am", "cyl"), interaction = TRUE), NA)
    expect_error(comparisons(mod, interaction = TRUE), regexp = "variables")
})


test_that("interaction (no validity)", {
    mod <- lm(mpg ~ factor(am) * factor(cyl) + wt + gear, data = mtcars)

    # one row only means tidy is same nrows
    cmp <- comparisons(
        mod,
        variables = c("cyl", "am"),
        newdata = datagrid(),
        contrast_factor = "all",
        interaction = TRUE)
    expect_equal(nrow(cmp), 18)
    expect_equal(nrow(tidy(cmp)), 18)

    cmp <- comparisons(
        mod,
        variables = c("cyl", "am"),
        contrast_factor = "sequential")
    expect_equal(nrow(cmp), 64)
    expect_equal(nrow(tidy(cmp)), 2)

    cmp <- comparisons(
        mod,
        variables = c("cyl", "am", "wt"))
    expect_equal(nrow(cmp), 192)
    expect_equal(nrow(tidy(cmp)), 6)

    cmp <- comparisons(
        mod,
        variables = c("cyl", "am", "wt"),
        contrast_factor = "pairwise")
    expect_equal(nrow(cmp), 768)
    expect_equal(nrow(tidy(cmp)), 24)
})


test_that("brms + order of first character doesn't matter", {
    mod <- download_model("brms_factor")
    cmp <- comparisons(mod, variables = c("cyl_fac", "mpg"), interaction = TRUE, contrast_factor = "all")
    expect_equal(nrow(cmp), 192)
    expect_equal(nrow(tidy(cmp)), 6)
})
