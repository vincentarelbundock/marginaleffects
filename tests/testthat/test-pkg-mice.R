skip_if_not_installed("mice")

suppressPackageStartupMessages({
  library(testthat)
  library(mice)
})


test_that("basic classes", {
  skip_if_not_installed("cobalt")
  withr::local_seed(48103)
  withr_rm(c("imp", "est"))
  data("lalonde_mis", package = "cobalt")
  lalonde_mis <- get_dataset("lalonde")
  imp <<- mice(lalonde_mis, print = FALSE) |> suppressWarnings()
  est <<- with(imp, lm(re78 ~ treat * (age + educ + re74)))
  pre <- avg_predictions(est)
  cmp <- avg_comparisons(est)
  slo <- avg_slopes(est)
  expect_s3_class(pre, "predictions")
  expect_s3_class(cmp, "comparisons")
  expect_s3_class(slo, "slopes")
})


test_that("nrow(missing) == nrow(complete)", {
  withr::local_seed(1024)
  dat <- iris
  dat$Sepal.Length[sample(seq_len(nrow(iris)), 40)] <- NA
  dat$Sepal.Width[sample(seq_len(nrow(iris)), 40)] <- NA
  dat$Species[sample(seq_len(nrow(iris)), 40)] <- NA
  dat_mice <- mice::mice(dat, m = 20, printFlag = FALSE, .Random.seed = 1024)
  mir <- with(dat_mice, lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species))
  mod <- lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species, data = dat)
  mfx1 <- suppressWarnings(avg_slopes(mir, by = "Species"))
  mfx2 <- avg_slopes(mod, by = "Species")
  expect_s3_class(mfx1, "slopes")
  expect_equal(nrow(mfx1), nrow(mfx2))
})


test_that("Issue #711: avg_slopes == mice::pool()", {
  data <- structure(list(id = 1:37, trt = c(rep("soc", 21), rep("arm", 16)), endp = structure(c(1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L), levels = c("TRUE", "FALSE"), class = "factor")), row.names = c(NA, -37L), class = "data.frame")
  data$endp <- factor(data$endp, levels = c("TRUE", "FALSE"))
  data_miss <- data
  data_miss[c(1, 5, 7, 30), "endp"] <- NA
  imp <- suppressWarnings(
    mice::mice(
      data_miss,
      m = 20, method = "pmm", maxit = 50, seed = 1000, print = FALSE)
  )
  dat_list <- mice::complete(imp, "all")
  fit_logistic <- function(dat) {
    mod <- glm(endp ~ trt, family = binomial(link = "logit"), data = dat)
    avg_slopes(mod, newdata = dat)
  }
  mod_imputation <- suppressWarnings(lapply(dat_list, fit_logistic))
  manu <- suppressWarnings(summary(pool(mod_imputation), conf.int = TRUE))
  fit <- with(imp, glm(endp ~ trt, family = binomial(link = "logit")))
  auto <- suppressWarnings(avg_slopes(fit))
  expect_equal(auto$estimate, manu$estimate)
  expect_equal(auto$std.error, manu$std.error, tolerance = 1e-6)
})


test_that("Issue #1269: transforms applied after pooling", {
  skip_if_not_installed("cobalt")
  data("lalonde_mis", package = "cobalt")
  imp <- mice::mice(lalonde_mis, print = FALSE, seed = 48103)
  fits <- with(imp, glm(treat ~ age + married, family = binomial))
  cmp1 <- avg_comparisons(fits, variables = "married", comparison = "lnratioavg", transform = "exp") |>
    suppressWarnings()
  expect_equal(cmp1$estimate, 0.3380001, tolerance = 1e-6)
  expect_equal(cmp1$conf.low, 0.2386019, tolerance = 1e-3)
  cmp2 <- avg_comparisons(fits, variables = "married", comparison = "lnratioavg") |>
    suppressWarnings()
  expect_equal(cmp2$estimate, -1.084709, tolerance = 1e-6)
  expect_equal(cmp2$conf.low, -1.432959, tolerance = 1e-3)
})


test_that("Issue #1420: hypotheses() has finite estimates and SEs with mice", {
  withr::local_seed(1024)
  dat <- iris
  dat$Sepal.Length[sample(seq_len(nrow(iris)), 40)] <- NA
  dat$Sepal.Width[sample(seq_len(nrow(iris)), 40)] <- NA
  dat$Species[sample(seq_len(nrow(iris)), 40)] <- NA
  dat_mice <- mice::mice(dat, m = 20, printFlag = FALSE, .Random.seed = 1024)
  mod_mice <- with(dat_mice, lm(Petal.Width ~ Sepal.Length))
  h <- hypotheses(mod_mice, hypothesis = "Sepal.Length = 0", equivalence = c(-1, 1))
  expect_false(anyNA(h$std.error))
  expect_false(anyNA(h$estimate))
})


test_that("Issue #1117: predictions and comparisons class on subsetted newdata", {
  skip_if_not_installed("cobalt")
  withr_rm(c("imp", "est"))
  data("lalonde_mis", package = "cobalt")
  imp <<- mice::mice(lalonde_mis, print = FALSE)
  est <<- with(imp, lm(re78 ~ treat * (age + educ + re74)))
  pre <- avg_predictions(est, variables = "treat", newdata = subset(treat == 1))
  expect_s3_class(pre, "predictions")
  cmp <- avg_comparisons(est, variables = "treat", newdata = subset(treat == 1))
  expect_s3_class(cmp, "comparisons")
})


test_that("newdata correspondence: subset() vs dplyr::filter()", {
  skip_if_not_installed("cobalt")
  skip_if_not_installed("dplyr")
  library(dplyr)
  withr_rm(c("imp", "est"))
  withr_detach("dplyr")

  data("lalonde_mis", package = "cobalt")
  imp <<- mice::mice(lalonde_mis, print = FALSE)
  est <<- with(imp, lm(re78 ~ treat * (age + educ + re74)))

  p1 <- avg_predictions(est, variables = "treat")
  p2 <- avg_predictions(est, variables = "treat", newdata = subset(treat == 0))
  p3 <- avg_predictions(est, variables = "treat", newdata = filter(treat == 0))
  p4 <- avg_predictions(est, variables = "treat", newdata = subset(treat == 1))
  p5 <- avg_predictions(est, variables = "treat", newdata = filter(treat == 1))

  expect_equal(p2$estimate, p3$estimate)
  expect_equal(p4$estimate, p5$estimate)
  expect_true(all(p1$estimate != p2$estimate))
  expect_true(all(p1$estimate != p4$estimate))
})


test_that("TODO: subset mice with argument (not supported/decided)", {
  skip("TODO: this should not be supported")
  # If/when supported, convert the four calls below into concrete expectations:
  # p1 <- avg_predictions(est, variables = "treat", newdata = subset(lalonde_mis, treat == 0))
  # p2 <- avg_predictions(est, variables = "treat", newdata = subset(lalonde_mis, treat == 1))
  # p3 <- avg_predictions(est, variables = "treat", newdata = dplyr::filter(lalonde_mis, treat == 0))
  # p4 <- avg_predictions(est, variables = "treat", newdata = dplyr::filter(lalonde_mis, treat == 1))
})
