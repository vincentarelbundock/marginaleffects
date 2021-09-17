skip_on_ci() # different graphics engine produce different snapshots

test_that("two conditions", {
    mod <- lm(mpg ~ hp * wt * am, data = mtcars)
    vdiffr::expect_doppelganger("cme plot with 2 conditions",
                                plot_cmm(mod, condition = c("hp", "wt")))
})
