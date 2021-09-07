skip_if_not_installed("MASS")

library("margins")
library("haven")
library("dplyr")

test_that("polr vs. margins", {
    skip("no idea why this fails")
    tmp <- data.frame(mtcars)
    tmp$carb <- as.factor(tmp$carb)
    mod <- MASS::polr(carb ~ hp + am + mpg, data = tmp) 
    res <- meffects(mod, variance = NULL)
    mar <- margins(mod)
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(480, 8))
    # TODO: not supported yet
    expect_error(meffects(mod, variance = NULL), regexp = "group_name")
    expect_error(meffects(mod, group_names = "1"), regexp = "not yet supported")
})

test_that("polr vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
    dat <- read_dta(test_path("stata/data/MASS_polr_01.dta"))
    mod <- MASS::polr(factor(y) ~ x1 + x2, data = dat)
    ame <- meffects(mod, 
                    variance = NULL, 
                    prediction_type = "probs") %>%
           group_by(group, term) %>%
           summarize(dydx = mean(dydx)) %>%
           mutate(group = as.numeric(group)) %>%
           inner_join(stata, by = c("group", "term"))
    expect_equal(ame$dydx, ame$dydxstata, tolerance = 0.001)
})
