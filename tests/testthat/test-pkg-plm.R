skip_if_not_installed("plm")
requiet("plm")

test_that("Baltagi (2013) replications from the plm doc", {
    # replicates some results from Baltagi (2013), table 3.1
    data("Grunfeld", package = "plm")
    pool <- plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
    mfx <- marginaleffects(pool)
    expect_false(any(mfx$estimate == 0))
    expect_false(any(mfx$std.error == 0))
    
    # within model are not supported by `predict.plm`
    wi <- plm(inv ~ value + capital,
              data = Grunfeld, model = "within", effect = "twoways")
    expect_error(marginaleffects(wi), regexp = "within")
    
    swar <- plm(inv ~ value + capital,
                data = Grunfeld, model = "random", effect = "twoways")
    mfx <- marginaleffects(swar)
    expect_false(any(mfx$estimate == 0))
    expect_false(any(mfx$std.error == 0))
    
    amemiya <- plm(inv ~ value + capital,
                   data = Grunfeld, model = "random", random.method = "amemiya",
                   effect = "twoways")
    mfx <- marginaleffects(amemiya)
    expect_false(any(mfx$estimate == 0))
    expect_false(any(mfx$std.error == 0))
    
    walhus <- plm(inv ~ value + capital,
                  data = Grunfeld, model = "random", random.method = "walhus",
                  effect = "twoways")
    mfx <- marginaleffects(walhus)
    expect_false(any(mfx$estimate == 0))
    expect_false(any(mfx$std.error == 0))
})
