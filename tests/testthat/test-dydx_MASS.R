skip_if_not_installed("MASS")

library("margins")
library("haven")
library("dplyr", warn.conflicts = FALSE)

test_that("polr vs. margins (dydx only)", {
    skip("`margins` produces weird results with MASS::polr")
    tmp <- data.frame(mtcars)
    tmp$carb <- as.factor(tmp$carb)
    mod <- MASS::polr(carb ~ hp + am + mpg, data = tmp) 
    res <- marginaleffects(mod, variance = NULL, prediction_type = "probs")
    mar <- margins(mod)
})

test_that("polr vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
    dat <- read_dta(test_path("stata/data/MASS_polr_01.dta"))
    mod <- MASS::polr(factor(y) ~ x1 + x2, data = dat)
    ame <- marginaleffects(mod, variance = NULL, prediction_type = "probs") %>%
           group_by(group, term) %>%
           summarize(dydx = mean(dydx)) %>% #, std.error = mean(std.error)) %>%
           mutate(group = as.numeric(group)) %>%
           inner_join(stata, by = c("group", "term"))
    expect_equal(ame$dydx, ame$dydxstata, tolerance = 0.001)
    # expect_equal(ame$std.error, ame$std.errorstata, tolerance = 0.001)
})
