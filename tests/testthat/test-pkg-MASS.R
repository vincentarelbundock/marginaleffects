skip_if_not_installed("MASS")
skip_if_not_installed("margins")

library("margins")

test_that("MASS::rlm no validity check", {
    model <- MASS::rlm(mpg ~ hp * wt, mtcars)
    mfx <- marginaleffects(model)
    expect_s3_class(mfx, "data.frame")
    expect_false(any(mfx$estimate == 0))
    expect_false(any(mfx$std.error == 0))
})


test_that("polr vs. margins (dydx only)", {
    skip("`margins` produces weird results with MASS::polr")
    tmp <- data.frame(mtcars)
    tmp$carb <- as.factor(tmp$carb)
    mod <- MASS::polr(carb ~ hp + am + mpg, data = tmp) 
    res <- marginaleffects(mod, vcov = FALSE, predict_type = "probs")
    mar <- margins(mod)
})


test_that("polr vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
    dat <- read.csv(test_path("stata/data/MASS_polr_01.csv"))
    mod <- MASS::polr(factor(y) ~ x1 + x2, data = dat)
    void <- capture.output(suppressWarnings(suppressMessages(
        ame <- marginaleffects(mod, vcov = FALSE, predict_type = "probs") %>%
               dplyr::group_by(group, term) %>%
               dplyr::summarize(dydx = mean(dydx)) %>% #, std.error = mean(std.error)) %>%
               dplyr::mutate(group = as.numeric(group)) %>%
               dplyr::inner_join(stata, by = c("group", "term"))
    )))
    expect_equal(ame$dydx, ame$dydxstata, tolerance = 0.001)
    # expect_equal(ame$std.error, ame$std.errorstata, tolerance = 0.001)
})


test_that("glm.nb vs. margins", {
    # margins does not support unit-level standard errors
    model <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    mfx <- marginaleffects(model)
    mar <- margins(model)
    expect_true(test_against_margins(mfx, mar))
})
