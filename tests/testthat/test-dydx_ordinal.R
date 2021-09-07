skip_if_not_installed("ordinal")

library("margins")

test_that("ordinal: vs `margins`", {
    data("wine", package = "ordinal")
    tmp <- wine
    tmp$warm <- as.numeric(tmp$temp == "warm")
    mod <- ordinal::clm(rating ~ warm * contact, data = tmp)
    res <- meffects(mod, 
               variables = "warm", 
               variance = NULL,
               prediction_type = "prob")
    mar <- suppressWarnings(data.frame(margins(mod)))
    expect_true(cor(res$dydx, mar$dydx_warm) > 0.999)
    expect_error(meffects(mod, 
                     variables = "warm",
                     prediction_type = "prob"), regexp = "variance")
    expect_warning(meffects(mod, 
                     variance = NULL,
                     variables = "warm"),
                 regexp = "prediction")
    expect_warning(meffects(mod, 
                       variance = NULL,
                       prediction_type = "probs"), regexp = "numeric")
})
