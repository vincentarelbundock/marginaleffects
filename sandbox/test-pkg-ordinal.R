# PROBLEM: see methods_ordinal.R for a potential bug that breaks things for us

skip_if_not_installed("ordinal")
requiet("margins")

stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
mod <- ordinal::clm(factor(y) ~ x1 + x2, data = dat)
mod_polr <- MASS::polr(factor(y) ~ x1 + x2, data = dat)

# Problem: all columns include the same predictions. We should have separate ones to make dy/dx for each level. This looks like a bug, so apparently we cannot support this model.
a <- predict(mod, newdata = insight::get_data(mod), type = "prob")$fit
b <- predict(mod, newdata = insight::get_data(mod), type = "cum.prob")[[2]]

test_that("ordinal vs. MASS", {
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    mod <- ordinal::clm(factor(y) ~ x1 + x2, data = dat)
    mod_polr <- MASS::polr(factor(y) ~ x1 + x2, data = dat)
    mfx <- tidy(marginaleffects(mod, type = "prob"))
    mfx_polr <- tidy(marginaleffects(mod_polr, type = "prob"))
})

test_that("ordinal: vs `margins`", {
    data("wine", package = "ordinal")
    tmp <- wine
    tmp$warm <- as.numeric(tmp$temp == "warm")
    mod <- ordinal::clm(rating ~ warm + contact, data = tmp)
    mod2 <- MASS::polr(rating ~ warm + contact, data = tmp)
    mfx <- marginaleffects(mod, type = "prob")
    mar <- suppressWarnings(margins::margins(mod))
    expect_mfx(mod, type = "prob")
    expect_true(test_against_margins(mfx, mar, tol = .01))
})
