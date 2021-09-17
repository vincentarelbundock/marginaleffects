# skip_if_not_installed("ordinal")

# requiet("margins")

# test_that("ordinal: vs `margins`", {
#     data("wine", package = "ordinal")
#     tmp <- wine
#     tmp$warm <- as.numeric(tmp$temp == "warm")
#     mod <- ordinal::clm(rating ~ warm * contact, data = tmp)
#     res <- marginaleffects(mod, 
#                            variables = "warm", 
#                            vcov = FALSE,
#                            type = "prob")
#     mar <- suppressWarnings(margins(mod))
#     expect_true(test_against_margins(res, mar, tol = .01))
#     warning("low tolerance")
#     expect_warning(marginaleffects(mod, 
#                                    variables = "warm",
#                                    type = "prob"), 
#                    regexp = "Variance.*supported")
#     expect_warning(marginaleffects(mod, 
#                                    vcov = FALSE,
#                                    variables = "warm"),
#                    regexp = "type")
# })
