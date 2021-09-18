skip_if_not_installed("MASS")
skip_if_not_installed("margins")

requiet("margins")
requiet("MASS")

test_that("MASS::rlm no validity check", {
    model <- MASS::rlm(mpg ~ hp + drat, mtcars)
    mfx <- marginaleffects(model)
    expect_mfx(model, n_unique = 1)
})


test_that("polr vs. margins (dydx only)", {
    skip("`margins` produces weird results with MASS::polr")
    tmp <- data.frame(mtcars)
    tmp$carb <- as.factor(tmp$carb)
    mod <- MASS::polr(carb ~ hp + am + mpg, data = tmp) 
    res <- marginaleffects(mod, vcov = FALSE, type = "probs")
    mar <- margins(mod)
})


test_that("glm.nb vs. margins", {
    # margins does not support unit-level standard errors
    model <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    mfx <- marginaleffects(model)
    mar <- margins(model)
    expect_true(test_against_margins(mfx, mar))
})


test_that("glm.nb vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$mass_glm_nb
    model <- suppressWarnings(
        MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    mfx <- merge(tidy(marginaleffects(model)), stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
})

# test_that("polr vs. Stata", {
#     stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
#     dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
#     mod <- MASS::polr(factor(y) ~ x1 + x2, data = dat)
#     void <- capture.output(suppressWarnings(suppressMessages(
#         ame <- marginaleffects(mod, vcov = FALSE, type = "probs") %>%
#                dplyr::group_by(group, term) %>%
#                dplyr::summarize(dydx = mean(dydx)) %>% #, std.error = mean(std.error)) %>%
#                dplyr::mutate(group = as.numeric(group)) %>%
#                dplyr::inner_join(stata, by = c("group", "term"))
#     )))
#     expect_equal(ame$dydx, ame$dydxstata, tolerance = 0.001)
#     # expect_equal(ame$std.error, ame$std.errorstata, tolerance = 0.001)
# })
