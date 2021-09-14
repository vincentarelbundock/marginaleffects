skip_if_not_installed("estimatr")

library("estimatr")

test_that("estimator::iv_robust no validity chec", {
    data(Kmenta, package = "ivreg")
    model <- estimatr::iv_robust(Q ~ P * D | D + F + A, data = Kmenta)
    # there are lots of zeros for instruments, so a low n_unique is fine.
    expect_mfx(model, n_unique = 9)
})
