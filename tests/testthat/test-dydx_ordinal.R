skip_if_not_installed("ordinal")

library("margins")

test_that("ordinal: vs `margins`", {
    data("wine", package = "ordinal")
    tmp <- wine
    tmp$warm <- as.numeric(tmp$temp == "warm")
    mod <- ordinal::clm(rating ~ warm * contact, data = tmp)
    res <- marginaleffects(mod, 
                    variables = "warm", 
                    variance = FALSE,
                    prediction_type = "prob")
    mar <- suppressWarnings(margins(mod))
    expect_true(test_against_margins(res, mar, tol = .01))
    warning("low tolerance")
    expect_warning(marginaleffects(mod, 
                                   variables = "warm",
                                   prediction_type = "prob"), 
                   regexp = "variance")
    expect_warning(marginaleffects(mod, 
                                   variance = FALSE,
                                   variables = "warm"),
                   regexp = "prediction")
})
