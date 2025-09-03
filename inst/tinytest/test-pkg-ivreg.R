source("helpers.R")
using("marginaleffects")

requiet("margins")
requiet("dplyr")
requiet("ivreg")

# Basic expectation tests
data("Kmenta", package = "ivreg")
mod_simple <- ivreg::ivreg(Q ~ P + D | D + F, data = Kmenta)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

# marginaleffects: vs. margins
data(Kmenta, package = "ivreg")
mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
res <- slopes(mod)
mar <- data.frame(margins(mod, unit_ses = TRUE))
expect_true(expect_margins(res, mar, tolerance = .1, verbose = TRUE))


# plot_predictions: bugs stay dead
# broke when no conf.low available
data(Kmenta, package = "ivreg")
mod <- ivreg::ivreg(Q ~ P + D + I(D^2) | D + I(D^2) + F + A, data = Kmenta)
expect_inherits(plot_predictions(mod, condition = "D"), "ggplot")


# marginaleffects: vs. Stata
dat <- read.csv(testing_path("stata/databases/ivreg_ivreg_01.csv"))
stata <- readRDS(testing_path("stata/stata.rds"))[["ivreg_ivreg_01"]]
mod <- ivreg::ivreg(Q ~ P + D | D + F + A, data = dat)
ame <- slopes(mod) |>
    dplyr::group_by(term) |>
    dplyr::summarize(estimate = mean(estimate), std.error = mean(std.error)) |>
    dplyr::inner_join(stata, by = "term")
expect_equivalent(ame$estimate, ame$dydxstata, tolerance = 0.0001)


# predictions: no validity
data(Kmenta, package = "ivreg")
mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
pred1 <- predictions(mod)
pred2 <- predictions(mod, newdata = head(Kmenta))
expect_predictions(mod, n_row = nrow(Kmenta))
expect_predictions(mod, newdata = head(Kmenta), n_row = 6)






if (!AUTODIFF) exit_file("autodiff")
mod <- ivreg::ivreg(Q ~ P * D | D + F + A, data = Kmenta)
autodiff(TRUE)
expect_message(p1 <- predictions(mod))
autodiff(FALSE)
p2 <- predictions(mod)
expect_equivalent(p1$estimate, p2$estimate)
expect_equivalent(p1$std.error, p2$std.error, tolerance = 4e-3)
