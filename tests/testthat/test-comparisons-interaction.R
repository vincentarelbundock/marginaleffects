test_that("interaction vs. emmeans", {
    requiet("emmeans")
    mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
    cmp <- suppressWarnings(comparisons(
        mod,
        variables = c("cyl", "am"),
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
    cmp <- comparisons(mod, interaction = TRUE)
    tid <- tidy(cmp)
    expect_true(all(tid$term == "interaction"))
})
  


test_that("interaction (no validity)", {
    mod <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)

    expect_warning(comparisons(mod, newdata = mtcars, interaction = TRUE))

    cmp <- comparisons(
        mod,
        variables = c("cyl", "am"),
        contrast_factor = "all",
        interaction = TRUE)
    expect_equal(nrow(cmp), 24)

    cmp <- comparisons(
        mod,
        variables = c("cyl", "am"),
        contrast_factor = "sequential",
        interaction = TRUE)
    expect_equal(nrow(cmp), 2)

    cmp <- comparisons(
        mod,
        variables = c("cyl", "am", "wt"),
        contrast_factor = "pairwise",
        interaction = TRUE)
    expect_equal(nrow(cmp), 3)

    cmp <- comparisons(
        mod,
        variables = c("cyl", "am", "wt"),
        contrast_factor = "pairwise",
        interaction = FALSE)
    cmp <- tidy(cmp)
    expect_equal(nrow(cmp), 5)
})


test_that("brms + order of first character doesn't matter", {
    mod <- download_model("brms_factor")
    cmp1 <- comparisons(mod, variables = c("cyl_fac", "mpg"), interaction = TRUE, contrast_factor = "all")
    cmp2 <- comparisons(mod, variables = c("mpg", "cyl_fac"), interaction = TRUE, contrast_factor = "all")
    expect_equal(nrow(cmp1), 6)
    expect_equal(nrow(cmp1), nrow(cmp2))
    expect_equal(cmp1$comparison, cmp2$comparison)
})
