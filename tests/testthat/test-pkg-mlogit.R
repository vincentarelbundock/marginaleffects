# why `newdata` is not supported

# here the `newdata` does not include the individual or choice variabls at all,
# but we still get a prediction. Impossible to know what order the rows are in,
# if `newdata` is balanced, or what group ids to give. `newdata` could be
# completely malformed and we would still produce results. I could make strong
# assumptions about group id being a multiple of number of rows with some
# modulo hacks, but that's bad practice. Example:
# nd <- TravelMode[, 3:ncol(TravelMode)]
# predict(mod, newdata = head(nd, 12))

requiet("mlogit")
requiet("AER")
data("TravelMode", package = "AER")

test_that("no validity", {
    mod <- mlogit(choice ~ wait + gcost | income + size, TravelMode)
    cmp <- comparisons(mod)
    pre <- predictions(mod)
    tid <- tidy(cmp)
    expect_s3_class(cmp, "comparisons")
    expect_s3_class(pre, "predictions")
    expect_marginaleffects(mod)
    expect_error(comparisons(mod, newdata = TravelMode), regexp = "newdata.*supported")
    expect_error(predictions(mod, newdata = TravelMode), regexp = "newdata.*supported")
    expect_error(marginaleffects(mod, newdata = TravelMode), regexp = "newdata.*supported")
    expect_true("group" %in% colnames(tid))
})
