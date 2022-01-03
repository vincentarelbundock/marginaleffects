test_that("type dictionary does not include duplicates", {
    x <- marginaleffects:::type_dictionary
    y <- type_dictionary_build()
    expect_equal(x, y) # for codecov
    dup <- x[!x$base %in% c("link", "prediction"), ]
    dup <- stats::na.omit(dup)
    expect_false(any(dup$base == dup$insight))
    dup <- stats::na.omit(x[, c("class", "base")])
    dup <- paste(dup$class, dup$base)
    expect_true(anyDuplicated(dup) == 0)
    dup <- stats::na.omit(x[, c("class", "insight")])
    dup <- paste(dup$class, dup$insight)
    expect_true(anyDuplicated(dup) == 0)
})


test_that("multiple prediction types", {
    skip_if_not_installed("pscl")
    requiet("pscl")
    data("bioChemists", package = "pscl")
    model <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    mfx <- marginaleffects(model, type = c("response", "zero"))
    expect_true(all(c("response", "zero") %in% mfx$type))
})


test_that("predicted values are added to the data.frame", {
    mod <- glm(am ~ mpg * wt, data = mtcars, family = binomial)
    mfx <- marginaleffects(mod, type = "response")
    expect_true("response" %in% mfx$type)
    mfx <- marginaleffects(mod, type = c("response", "link"))
    expect_true(all(c("response", "link") %in% mfx$type))
})


test_that("multiple prediction types", {
    skip_if_not_installed("pscl")
    requiet("pscl")
    data("bioChemists", package = "pscl")
    model <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    mfx <- marginaleffects(model, type = c("response", "zero"))
    expect_true(all(c("response", "zero") %in% mfx$type))
})


test_that("error: multivariate", {
    skip_if_not_installed("pscl")
    requiet("pscl")
    data("bioChemists", package = "pscl")
    model <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    mfx <- marginaleffects(model, type = "prob")
    expect_true(all(as.character(0:19) %in% mfx$group))
})
