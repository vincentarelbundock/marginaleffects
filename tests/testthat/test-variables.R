
test_that("named list", {

    tmp <- mtcars
    tmp$gear <- as.factor(tmp$gear)
    tmp$cyl <- as.factor(tmp$cyl)
    mod <- lm(mpg ~ gear + cyl + hp, data = tmp)

    cmp1 <- comparisons(mod, variables = "hp", newdata = head(tmp, 1))
    expect_equal(cmp1$term, "hp")
    expect_equal(cmp1$contrast, "(x + 1) - x")

    cmp2 <- comparisons(mod, variables = list("hp" = 1), newdata = head(tmp, 1))
    expect_equal(cmp1, cmp2)

    cmp <- comparisons(
        mod,
        variables = list(gear = "sequential", hp = 10, cyl = "pairwise"))
    cmp <- tidy(cmp)
    known <- c("4 - 3", "5 - 4", "(x + 10) - x", "6 - 4", "8 - 4", "8 - 6")
    expect_true(all(known %in% cmp$contrast))

    # informative errors
    expect_error(comparisons(mod, variables = list(gear = "blah")), regexp = "character variables")
    expect_error(comparisons(mod, variables = list(hp = "pairwise"), regexp = "numeric variables"))

})


test_that("regression test: factor in formula and numeric check", {
    mod <- lm(mpg ~ factor(cyl), data = mtcars)
    expect_error(comparisons(mod, variables = list(cyl = "pairwise")), NA)
    expect_error(comparisons(mod, variables = list(cyl = "iqr")), regexp = "factor")
})

