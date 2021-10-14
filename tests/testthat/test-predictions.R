skip_if_not_installed("pscl")
requiet("pscl")

tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ hp + wt + factor(cyl) + am, data = tmp)

# TODO: remove this when insight is updated
test_that("insight > 0.14.1 allows us to support `type`", {
    expect_warning(predictions(mod, type = "response"), NA)
})


test_that("bugfix: counterfactual predictions keep rowid", {
  mod <- lm(mpg ~ hp + am, mtcars)
  pred <- predictions(mod, newdata = counterfactual(am = 0:1))
  expect_predictions(pred, n_row = 64)
  expect_true("rowid" %in% colnames(pred))
})


################
#  conf.level  #
################

test_that("conf.level argument changes width of interval", {
    for (L in c(.4, .7, .9, .95, .99, .999)) {
        nd <- typical(model = mod)
        unknown <- predictions(mod, newdata = nd, conf.level = L)
        known <- predict(mod, newdata = nd, se.fit = TRUE, interval = "confidence", level = L)$fit
        expect_equal(unknown$conf.low, known[, "lwr"])
        expect_equal(unknown$conf.high, known[, "upr"])
    }
})

######################################
#  values against predict benchmark  #
######################################

test_that("predictions() = predict()", {
    nd <- typical(model = mod, cyl = c(4, 6, 8))
    mm <- predictions(mod, newdata = nd)
    expect_equal(mm$predicted, unname(predict(mod, newdata = nd)))
})

##############################
#  size: variables argument  #
##############################

test_that("`variables` arg: factor", {
    mm <- predictions(mod, variables = "cyl")
    expect_equal(nrow(mm), 3)
})

test_that("`variables` arg: logical", {
    mm <- predictions(mod, variables = "am")
    expect_equal(nrow(mm), 2)
})

test_that("`variables` arg: numeric", {
    mm <- predictions(mod, variables = "wt")
    expect_equal(nrow(mm), 5)
})

test_that("`variables` arg: factor + logical", {
    mm <- predictions(mod, variables = c("am", "cyl"))
    # logical 2; cyl factor 3
    expect_equal(nrow(mm), 2 * 3)
})


test_that("`variables` arg: logical + numeric", {
    mm <- predictions(mod, variables = c("am", "wt"))
    # logical 2; numeric 5 numbers
    expect_equal(nrow(mm), 2 * 5)
})

test_that("`variables` arg: factor + numeric", {
    mm <- predictions(mod, variables = c("cyl", "wt"))
    # logical 2; numeric 5 numbers
    expect_equal(nrow(mm), 3 * 5)
})


#############################
#  size: new data argument  #
#############################

test_that("`newdata`: mtcars has 32 rows", {
    mm <- predictions(mod, newdata = tmp)
    expect_equal(nrow(mm), 32)
})
 
test_that("`typical`: all factors", {
    mm <- predictions(mod, newdata = typical(cyl = c(4, 6, 8)))
    expect_equal(nrow(mm), 3)
})

test_that("`typical`: two missing factors", {
    mm <- predictions(mod, newdata = typical(cyl = 4))
    expect_equal(nrow(mm), 1)
})

test_that("`typical`: one missing factor", {
    mm <- predictions(mod, newdata = typical(cyl = c(4, 6)))
    expect_equal(nrow(mm), 2)
})

test_that("`typical`: all logical", {
    mm <- predictions(mod, newdata = typical(am = c(TRUE, FALSE)))
    expect_equal(nrow(mm), 2)
    expect_equal(length(unique(mm$predicted)), nrow(mm))
})

test_that("`typical`: missing logical", {
    mm <- predictions(mod, newdata = typical(am = TRUE))
    expect_equal(nrow(mm), 1)
})

#########################################################################
#  some models do not return data.frame under `insight::get_predicted`  #
#########################################################################

test_that("hurdle predictions", {
    data("bioChemists", package = "pscl")
    mod <- pscl::hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
    pred <- predictions(mod)
    expect_s3_class(pred, "data.frame")
    expect_true("predicted" %in% colnames(pred))
})
