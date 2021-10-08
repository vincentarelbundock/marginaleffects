skip_if_not_installed("rms")

test_that("marginaleffects: rms: no validity", {
    model <- rms::lrm(am ~ mpg, mtcars)
    void <- capture.output({
        expect_marginaleffects(model, type = "lp", n_unique = 1)
    })
})

test_that("predictions: rms: no validity", {
    skip("unsupported data argument in get_predicted.lrm")
    model <- rms::lrm(am ~ mpg, mtcars)
    pred1 <- predictions(model, type = "lp")
    pred2 <- predictions(model, type = "lp", newdata = head(mtcars))
    expect_predictions(pred1, n_row = 1, se = FALSE)
    expect_predictions(pred2, n_row = 6, se = FALSE)
})

