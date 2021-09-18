skip_on_ci() # different graphics engine produce different snapshots

test_that("two conditions", {
    mod <- lm(mpg ~ hp * wt * am, data = mtcars)
    p <- plot_cmm(mod, condition = c("hp", "wt"))
    vdiffr::expect_doppelganger("cmm plot with 2 conditions", p)
})


test_that("conf.level in plots", {
    mod <- lm(mpg ~ hp * wt * am, data = mtcars)
    p1 <- plot_cmm(mod, condition = "hp", conf.level = .99)
    p2 <- plot_cmm(mod, condition = "hp", conf.level = .4)
    vdiffr::expect_doppelganger("plot_cmm large ci", p1)
    vdiffr::expect_doppelganger("plot_cmm small ci", p2)
})
