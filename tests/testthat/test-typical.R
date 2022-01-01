skip_if_not_installed("fixest")


test_that("typical(x = NA)", {
    # numeric
    nd <- typical(newdata = mtcars, mpg = NA, hp = 1:4)
    expect_equal(nrow(nd), 4)
    expect_true(all(is.na(nd$mpg)))

    # factor
    tmp <- mtcars
    tmp$gear <- factor(tmp$gear)
    nd <- typical(newdata = tmp, gear = NA, hp = 1:4)
    expect_equal(nrow(nd), 4)
    expect_true(all(is.na(nd$gear)))
})

test_that("unique values", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    mod_int <- lm(mpg ~ am * factor(cyl), tmp)
    mfx <- marginaleffects(mod_int,
                           newdata = typical(cyl = tmp$cyl),
                           variables = "am")
    expect_equal(nrow(mfx), 3)
})

test_that("typical FUN.*", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    tmp$cyl <- as.factor(tmp$cyl)
    tmp$gear <- as.character(tmp$gear)
    typ <- typical(newdata = tmp,
                   FUN.character = max,
                   FUN.factor = \(x) sort(x)[1],
                   FUN.numeric = stats::median)
    expect_equal(typ$drat, stats::median(mtcars$drat))
    expect_equal(typ$cyl, factor("4", levels = c("4", "6", "8")))
    expect_equal(typ$gear, "5")
})

test_that("all manual", {
    mod <- lm(hp ~ mpg, mtcars)
    nd <- typical(model = mod, mpg = 110)
    expect_s3_class(nd, "data.frame")
    expect_equal(dim(nd), c(1, 1))
})

test_that("errors and warnings", {
    mod <- lm(hp ~ mpg, mtcars)
    expect_error(typical(), regexp = "should not both")
    expect_error(typical(model = mod, newdata = mtcars), regexp = "must be")

    mod <- lm(hp ~ factor(cyl), mtcars)
    expect_error(typical(model = mod, cyl = "4"), NA)
    expect_error(typical(model = mod, cyl = "2"), regexp = "must be one of the factor levels")

    mod <- fixest::feols(mpg ~ hp | cyl, data = mtcars)
    expect_warning(typical(model = mod), regexp = "cluster")
})
