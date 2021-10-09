test_that("old bug: counterfactual with a single regressor", {
    mod <- lm(mpg ~ hp + drat + wt, mtcars)
    x <- counterfactual(model = mod, hp = c(100, 110))
    expect_equal(nrow(x), 64)
    mod <- lm(mpg ~ hp, mtcars)
    x <- counterfactual(model = mod, hp = c(100, 110))
    expect_equal(nrow(x), 64)
})

test_that("marginal effects does not overwrite counterfactual rowid", {
    mod <- glm(am ~ mpg + factor(cyl), data = mtcars, family = binomial)
    mfx <- marginaleffects(mod, newdata = counterfactual(cyl = c(4, 6, 8)))
    expect_true(all(mfx$rowid %in% 1:32))
})

test_that("alternative syntaxes", {
    mod <- lm(mpg ~ hp + drat + wt, mtcars)
    nd1 <- typical(wt = 3, hp = c(100, 110), model = mod)
    nd2 <- typical(wt = 3, hp = c(100, 110), newdata = mtcars)
    x <- marginaleffects(mod, newdata = typical(wt = 3, hp = c(100, 110)))
    y <- marginaleffects(mod, newdata = nd1)
    z <- marginaleffects(mod, newdata = nd2)
    z <- z[, colnames(x)]
    expect_true(all(x == y))
    expect_true(all(x == z))
})


test_that("size", {
    mod <- lm(mpg ~ hp + drat + wt, mtcars)
    expect_equal(nrow(counterfactual(wt = 2:3, newdata = mtcars)), nrow(mtcars) * 2)
    expect_equal(nrow(typical(wt = 2:3, newdata = mtcars)), 2)
    expect_equal(nrow(counterfactual(wt = 2:3, model = mod)), nrow(mtcars) * 2)
    expect_equal(nrow(typical(wt = 2:3, model = mod)), 2)
})


test_that("warning on bad `at` entry", {
    expect_warning(counterfactual(newdata = mtcars, at = list("blah" = 0:1)))
    expect_warning(typical(newdata = mtcars, at = list("blah" = 0:1)))
})


test_that("counterfactual(): factor, logical, automatic variable", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    tmp$gear <- as.factor(tmp$gear)
    mod <- lm(mpg ~ hp * wt + am + gear, data = tmp)
    res <- counterfactual(model = mod,
                          hp = c(100, 110),
                          gear = c(3, 4),
                          am = TRUE)
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(128, 5))
})

test_that("typical(): factor, logical, numeric", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    tmp$gear <- as.factor(tmp$gear)
    res <- typical(newdata = tmp)
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(1, 11))
    expect_equal(sum(sapply(res, is.logical)), 1)
    expect_equal(sum(sapply(res, is.factor)), 1)
    expect_equal(sum(sapply(res, is.numeric)), 9)
})

test_that("typical number of rows", {
    mod <- lm(mpg ~ hp * wt, data = mtcars)
    nd <- typical(model = mod, hp = c(100, 110))
    expect_equal(dim(marginaleffects(mod, newdata = nd)), c(4, 8))
})
