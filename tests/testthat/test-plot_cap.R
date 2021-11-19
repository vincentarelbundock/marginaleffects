skip_on_ci() # different graphics engine produce different snapshots
skip_on_cran() # different graphics engine produce different snapshots

test_that("two conditions", {
    mod <- lm(mpg ~ hp * wt * am, data = mtcars)
    p <- plot_cap(mod, condition = c("hp", "wt"))
    vdiffr::expect_doppelganger("cmm plot with 2 conditions", p)
})

test_that("continuous vs. categorical x-axis", {
    mod <- lm(mpg ~ hp * wt * factor(cyl), mtcars)
    p <- plot_cap(mod, condition = c("cyl", "wt"))
    vdiffr::expect_doppelganger("cmm categorical x-axis", p)
    p <- plot_cap(mod, condition = c("wt", "cyl"))
    vdiffr::expect_doppelganger("cmm continuous x-axis", p)
})

test_that("conf.level in plots", {
    mod <- lm(mpg ~ hp * wt * am, data = mtcars)
    p1 <- plot_cap(mod, condition = "hp", conf.level = .99)
    p2 <- plot_cap(mod, condition = "hp", conf.level = .4)
    vdiffr::expect_doppelganger("plot_cap large ci", p1)
    vdiffr::expect_doppelganger("plot_cap small ci", p2)
})

test_that("link vs response", {
    mod <- glm(am ~ hp + wt, data = mtcars, family = binomial)
    p1 <- plot_cap(mod, condition = "hp", type = "response")
    p2 <- plot_cap(mod, condition = "hp", type = "link")
    vdiffr::expect_doppelganger("plot_cap logit response", p1)
    vdiffr::expect_doppelganger("plot_cap logit link", p2)
})

test_that("bad condition raises error", {
    mod <- lm(mpg ~ hp * wt * am, data = mtcars)
    expect_error(plot_cap(mod, condition = c("bad", "wt")))
})
