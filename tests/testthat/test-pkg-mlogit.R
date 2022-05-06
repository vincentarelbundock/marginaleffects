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
    expect_true("group" %in% colnames(tid))
})

test_that("error on bad newdata", {
    mod <- mlogit(choice ~ wait + gcost | income + size, TravelMode)
    nd <- head(TravelMode, 5)
    expect_error(comparisons(mod, newdata = nd), regexp = "number of choices")
})

# mlogit doesn't install on Github actions, so we can't have it in DESCRIPTION,
# but if we use the Fishing data, this raises an error in check()

# test_that("vs. nnet::multinom", {
#     requiet("nnet")
#     requiet("data.table")
#     data("Fishing", package = "mlogit")
#     Fish <- dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")
#     m1 <- mlogit(mode ~ 0 | income, data = Fish)
#     m2 <- nnet::multinom(mode ~ income, data = Fishing, trace = FALSE)
#     mfx1 <- marginaleffects(m1)
#     mfx2 <- suppressWarnings(marginaleffects(m2, type = "probs"))
#     setDT(mfx1)
#     setDT(mfx2)
#     setkey(mfx1, rowid, group)
#     setkey(mfx2, rowid, group)
#     expect_equal(mfx1$dydx, mfx2$dydx, tolerance = 1e-5)
#     expect_equal(mfx1$dydx, mfx2$dydx, tolerance = 1e-5)
# })


