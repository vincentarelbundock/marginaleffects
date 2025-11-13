testthat::skip_if_not_installed("margins")
testthat::skip_if_not_installed("dplyr")
testthat::skip_if_not_installed("ivreg")

requiet("margins")
requiet("dplyr")
requiet("ivreg")

# Basic expectation tests
data("Kmenta", package = "ivreg")
mod_simple <- ivreg::ivreg(Q ~ P + D | D + F, data = Kmenta)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# marginaleffects: vs. margins
data(Kmenta, package = "ivreg")
mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
res <- slopes(mod)
mar <- data.frame(margins(mod, unit_ses = TRUE))
expect_margins2(res, mar, tolerance = .1, verbose = TRUE)


# plot_predictions: bugs stay dead
# broke when no conf.low available
data(Kmenta, package = "ivreg")
mod <- ivreg::ivreg(Q ~ P + D + I(D^2) | D + I(D^2) + F + A, data = Kmenta)
expect_s3_class(plot_predictions(mod, condition = "D"), "ggplot")


# marginaleffects: vs. Stata
dat_ivreg <- read.csv(testing_path("stata/databases/ivreg_ivreg_01.csv"))
stata <- readRDS(testing_path("stata/stata.rds"))[["ivreg_ivreg_01"]]
mod <- ivreg::ivreg(Q ~ P + D | D + F + A, data = dat_ivreg)
ame <- slopes(mod) |>
    dplyr::group_by(term) |>
    dplyr::summarize(estimate = mean(estimate), std.error = mean(std.error)) |>
    dplyr::inner_join(stata, by = "term")
expect_equal(ame$estimate, ame$dydxstata, tolerance = 0.0001, ignore_attr = TRUE)


# predictions: no validity
data(Kmenta, package = "ivreg")
mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
pred1 <- predictions(mod)
pred2 <- predictions(mod, newdata = head(Kmenta))
expect_predictions2(mod, n_row = nrow(Kmenta))
expect_predictions2(mod, newdata = head(Kmenta), n_row = 6)




if (!AUTODIFF) testthat::skip("autodiff")
mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
autodiff(TRUE)
expect_message(p1 <- predictions(mod))
autodiff(FALSE)
p2 <- predictions(mod)
expect_equal(p1$estimate, p2$estimate, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(p1$std.error, p2$std.error, tolerance = 4e-3, ignore_attr = TRUE)




# dat = get_dataset("airbnb")
# mod = ivreg::ivreg(price ~ bathrooms + bedrooms | Dryer + unit_type, data = dat)
# automatic = function() {
#     autodiff(TRUE)
#     suppressMessages(p1 <- predictions(mod))
# }
# finite = function() {
#     autodiff(FALSE)
#     p2 <- predictions(mod)
# }
# microbenchmark::microbenchmark(automatic(), finite(), times = 10)
