tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ hp + wt + factor(cyl) + am, data = tmp)

######################################
#  values against predict benchmark  #
######################################

test_that("marginalmeans() = predict()", {
    nd <- typical(model = mod, cyl = c(4, 6, 8))
    mm <- marginalmeans(mod, newdata = nd)
    expect_equal(mm$predicted, unname(predict(mod, newdata = nd)))
})

##############################
#  size: variables argument  #
##############################

test_that("`variables` arg: factor", {

              pkgload::load_all()
    mm <- marginalmeans(mod, variables = "cyl")

    expect_equal(nrow(mm), 3)
})

test_that("`variables` arg: logical", {
    mm <- marginalmeans(mod, variables = "am")
    expect_equal(nrow(mm), 2)
})

test_that("`variables` arg: numeric", {
    mm <- marginalmeans(mod, variables = "wt")
    expect_equal(nrow(mm), 5)
})

test_that("`variables` arg: factor + logical", {
    mm <- marginalmeans(mod, variables = c("am", "cyl"))
    # logical 2; cyl factor 3
    expect_equal(nrow(mm), 2 * 3)
})


test_that("`variables` arg: logical + numeric", {
    mm <- marginalmeans(mod, variables = c("am", "wt"))
    # logical 2; numeric 5 numbers
    expect_equal(nrow(mm), 2 * 5)
})

test_that("`variables` arg: factor + numeric", {
    mm <- marginalmeans(mod, variables = c("cyl", "wt"))
    # logical 2; numeric 5 numbers
    expect_equal(nrow(mm), 3 * 5)
})


#############################
#  size: new data argument  #
#############################

test_that("`newdata`: mtcars has 32 rows", {
    mm <- marginalmeans(mod, newdata = tmp)
    expect_equal(nrow(mm), 32)
})
 
test_that("`typical`: all factors", {
    mm <- marginalmeans(mod, newdata = typical(cyl = c(4, 6, 8)))
    expect_equal(nrow(mm), 3)
})

test_that("`typical`: two missing factors", {
    mm <- marginalmeans(mod, newdata = typical(cyl = 4))
    expect_equal(nrow(mm), 1)
})

test_that("`typical`: one missing factor", {
    mm <- marginalmeans(mod, newdata = typical(cyl = c(4, 6)))
    expect_equal(nrow(mm), 2)
})

test_that("`typical`: all logical", {
    mm <- marginalmeans(mod, newdata = typical(am = c(TRUE, FALSE)))
    expect_equal(nrow(mm), 2)
    expect_equal(length(unique(mm$predicted)), nrow(mm))
})

test_that("`typical`: missing logical", {
    mm <- marginalmeans(mod, newdata = typical(am = TRUE))
    expect_equal(nrow(mm), 1)
})
