# HPD tests against emmeans, which uses HDI, but our default is ETI
# HDI is implemented specifically for these tests
# https://github.com/vincentarelbundock/marginaleffects/issues/240
# skip_if(isTRUE(Sys.getenv("R_TEST_SKIP_BRMS") == "1"))
options("marginaleffects_credible_interval" = "hdi")
requiet("brms")
requiet("emmeans")
requiet("broom")
requiet("brmsmargins")
tol <- 0.0001
tol_se <- 0.001


# download models
brms_numeric <- download_model("brms_numeric")
brms_numeric2 <- download_model("brms_numeric2")
brms_character <- download_model("brms_character")
brms_factor <- download_model("brms_factor")
brms_factor_formula <- download_model("brms_factor_formula")
brms_interaction <- download_model("brms_interaction")
brms_logical <- download_model("brms_logical")
brms_epi <- download_model("brms_epi")
brms_cumulative_random <- download_model("brms_cumulative_random")
brms_monotonic <- download_model("brms_monotonic")
brms_monotonic_factor <- download_model("brms_monotonic_factor")
brms_vdem <- download_model("brms_vdem")


test_that("average marginal effects brmsmargins", {
  options("marginaleffects_credible_interval" = "eti")
  h <- 5e-5
  bm <- brmsmargins(
      brms_numeric,
      add = data.frame(hp = c(0, 0 + h)),
      contrasts = cbind("AME MPG" = c(-1 / h, 1 / h)),
      CI = 0.95, CIType = "ETI") 
  bm <- data.frame(bm$ContrastSummary)

  mfx <- marginaleffects(brms_numeric)
  mfx <- tidy(mfx, FUN = mean)

  expect_equal(mfx$estimate, bm$M, tolerance = tol)
  expect_equal(mfx$conf.low, bm$LL, tolerance = tol)
  expect_equal(mfx$conf.high, bm$UL, tolerance = tol)
  
  options("marginaleffects_credible_interval" = "hdi")
})


test_that("marginaleffects vs. emmeans", {
    mfx <- marginaleffects(brms_numeric2, newdata = datagrid(mpg = 20, hp = 100),
                           variables = "mpg", type = "link")
    em <- emtrends(brms_numeric2, ~mpg, "mpg", at = list(mpg = 20, hp = 100))
    em <- tidy(em)
    expect_equal(mfx$dydx, em$mpg.trend)
    expect_equal(mfx$conf.low, em$lower.HPD)
    expect_equal(mfx$conf.high, em$upper.HPD)
    # tolerance is less good for back-transformed response
    mfx <- marginaleffects(brms_numeric2, newdata = datagrid(mpg = 20, hp = 100),
                           variables = "mpg", type = "response")
    em <- emtrends(brms_numeric2, ~mpg, "mpg", at = list(mpg = 20, hp = 100), regrid = "response")
    em <- tidy(em)
    expect_equal(mfx$dydx, em$mpg.trend, tolerance = .1)
    expect_equal(mfx$conf.low, em$lower.HPD, tolerance = .01)
    expect_equal(mfx$conf.high, em$upper.HPD, tolerance = .1)
})

test_that("brms: cumulative: marginaleffects: no validity", {
    expect_marginaleffects(brms_cumulative_random, se = FALSE)
})

test_that("brms: logical regressor", {
    mfx <- marginaleffects(brms_logical)
    expect_s3_class(mfx, "marginaleffects")
    expect_equal(nrow(mfx), nrow(attr(mfx, "posterior_draws")))
})


test_that("predictions: hypothetical group", {
    mod <- insight::download_model("brms_mixed_3")
    nd <- suppressWarnings(datagrid(model = mod, grp = 4, subgrp = 12))
    nd$Subject <- 1000
    set.seed(1024)
    p1 <- predictions(mod, newdata = nd, allow_new_levels = TRUE)
    set.seed(1024)
    p2 <- predictions(mod, newdata = nd, allow_new_levels = TRUE, sample_new_levels = "gaussian")
    set.seed(1024)
    p3 <- predictions(mod, newdata = nd, allow_new_levels = TRUE, sample_new_levels = "uncertainty")
    expect_false(any(p1$predicted == p2$predicted))
    expect_equal(p1, p3, ignore_attr = TRUE)
})


test_that("predictions w/ random effects", {
    mod <- insight::download_model("brms_mixed_3")

    # link
    w <- apply(posterior_linpred(mod), 2, stats::median)
    x <- get_predict(mod, type = "link")
    y <- predictions(mod, type = "link")
    expect_equal(w, x$predicted, ignore_attr = TRUE)
    expect_equal(w, y$predicted, ignore_attr = TRUE)

    # response
    w <- apply(posterior_epred(mod), 2, stats::median)
    x <- get_predict(mod, type = "response")
    y <- predictions(mod, type = "response")
    expect_equal(w, x$predicted, ignore_attr = TRUE)
    expect_equal(w, y$predicted, ignore_attr = TRUE)

    # no random effects
    w1 <- apply(posterior_epred(mod), 2, stats::median)
    w2 <- apply(posterior_epred(mod, re_formula = NA), 2, stats::median)
    x <- get_predict(mod, re_formula = NA, type = "response")
    y <- predictions(mod, re_formula = NA, type = "response")
    expect_true(all(w1 != w2))
    expect_equal(w2, x$predicted, ignore_attr = TRUE)
    expect_equal(w2, y$predicted, ignore_attr = TRUE)
})

test_that("brms: cumulative: predictions: no validity", {
    set.seed(1024)
    p1 <- predictions(brms_cumulative_random)
    p2 <- predictions(brms_cumulative_random, re_formula = NA)
    expect_true(mean(p1$conf.low < p2$conf.low) > .95) # tolerance
    expect_true(mean(p1$conf.high > p2$conf.high) > .99) # tolerance
    expect_warning(predictions(brms_cumulative_random, include_random = FALSE)) # only for lme4
})

test_that("marginaleffects: ordinal no validity", {
    mod <- insight::download_model("brms_ordinal_1")
    expect_marginaleffects(mod, se = FALSE)
})

test_that("predict new unit: no validity", {
    dat1 <- dat2 <- datagrid(model = brms_epi)
    dat2$patient <- 9999
    set.seed(1024)
    mfx1 <- marginaleffects(brms_epi, newdata = dat1)
    set.seed(1024)
    mfx2 <- marginaleffects(brms_epi, newdata = dat2, allow_new_levels = TRUE)
    expect_false(any(mfx1$dydx == mfx2$dydx))
})


test_that("tidy()", {
    dat <- mtcars
    dat$logic <- as.logical(dat$vs)
    dat$cyl_fac <- as.factor(dat$cyl)
    dat$cyl_cha <- as.character(dat$cyl)
    mfx <- marginaleffects(brms_factor, newdata = dat)
    ti <- tidy(mfx)
    expect_s3_class(ti, "data.frame")
    expect_equal(dim(ti), c(3, 6))
    expect_true(all(c("term", "estimate", "conf.low") %in% colnames(ti)))
})


test_that("predictions: no validity", {
    # simple
    pred <- predictions(brms_numeric2, newdata = datagrid(hp = c(100, 120)))
    expect_predictions(pred, se = FALSE) 
    expect_equal(dim(attr(pred, "posterior_draws")), c(2, 2000))
    # interaction
    pred <- predictions(brms_interaction, newdata = datagrid(mpg = c(20, 25)))
    expect_predictions(pred, se = FALSE)
    # factor in data frame
    pred <- predictions(brms_factor, newdata = datagrid())
    expect_predictions(pred, se = FALSE)
})


test_that("predictions: prediction vs. expectation vs. include_random", {
    # prediction vs. response
    p1 <- suppressWarnings(predictions(brms_epi, type = "prediction"))
    p2 <- suppressWarnings(predictions(brms_epi, type = "response"))
    expect_true(all(p1$conf.low < p2$conf.low))
    expect_true(all(p1$conf.high > p2$conf.high))
    # re_formula
    p1 <- predictions(brms_epi, newdata = datagrid(patient = 1))
    p2 <- predictions(brms_epi, newdata = datagrid(patient = 1), re_formula = NA)
    expect_false(p1$predicted == p2$predicted)
    expect_false(p1$conf.low == p2$conf.low)
    expect_false(p1$conf.high == p2$conf.high)
})


test_that("predictions vs. emmeans", {
    requiet("emmeans")
    em <- emmeans::emmeans(brms_numeric, ~hp, "hp", at = list(hp = c(100, 120)))
    em <- data.frame(em)
    pred <- predictions(brms_numeric, newdata = datagrid(hp = c(100, 120)), type = "link")
    expect_equal(pred$predicted, em$emmean)
    expect_equal(pred$conf.low, em$lower.HPD)
    expect_equal(pred$conf.high, em$upper.HPD)
})


test_that("marginalmeans vs. emmeans", {
    requiet("emmeans")
    requiet("broom")
    expect_error(marginalmeans(brms_factor, variables = "cyl_fac", type = "link"), regexp = "github.*issues")
    # emmeans::emmeans(brms_factor, specs = ~cyl_fac)
})


test_that("marginaleffects: no validity", {
    expect_marginaleffects(brms_numeric2, se = FALSE)
    expect_marginaleffects(brms_interaction, se = FALSE)
    expect_marginaleffects(brms_factor, se = FALSE)
    # credible intervals and posterior draws
    tmp <- marginaleffects(brms_factor)
    expect_true("conf.low" %in% colnames(tmp))
    expect_true(all(tmp$dydx > tmp$conf.low))
    expect_true(all(tmp$dydx < tmp$conf.high))
    expect_false(is.null(attr(tmp, "posterior_draws")))
    expect_equal(nrow(attr(tmp, "posterior_draws")), nrow(tmp))
})


test_that("marginaleffects vs. emmeans", {
    requiet("emmeans")

    # known frequentist example to compare syntax
    brms_numeric_freq <- glm(am ~ hp, data = mtcars, family = binomial)
    marginaleffects(brms_numeric_freq, newdata = datagrid(hp = 147), type = "link")
    emmeans::emtrends(brms_numeric_freq, specs = ~hp, var = "hp", at = list(hp = 147))

    # one variable: link scale
    mfx1 <- marginaleffects(brms_numeric, variables = "hp", newdata = datagrid(hp = 110), type = "link")
    mfx2 <- as.data.frame(emmeans::emtrends(brms_numeric, ~hp, var = "hp", at = list(hp = 110)))
    expect_equal(mfx1$dydx, mfx2$hp.trend)
    expect_equal(mfx1$conf.low, mfx2$lower.HPD)
    expect_equal(mfx1$conf.high, mfx2$upper.HPD)

    # one variable: response scale
    mfx1 <- marginaleffects(brms_numeric, variables = "hp", newdata = datagrid(hp = 110))
    mfx2 <- as.data.frame(emtrends(brms_numeric, ~hp, var = "hp", at = list(hp = 110), regrid = "response"))
    expect_equal(mfx1$dydx, mfx2$hp.trend, tolerance = .001)
    expect_equal(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001)
    expect_equal(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001)

    # numeric + factor: numeric variable
    dat <- datagrid(model = brms_factor, mpg = 25, cyl_fac = 4)
    mfx1 <- marginaleffects(brms_factor, variables = "mpg", newdata = dat, type = "link")
    mfx2 <- as.data.frame(emmeans::emtrends(brms_factor, ~mpg, var = "mpg", at = list(mpg = 25, cyl_fac = 4)))
    expect_equal(mfx1$dydx, mfx2$mpg.trend, tolerance = .001)
    expect_equal(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001)
    expect_equal(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001)

    # numeric + factor: factor
    dat <- datagrid(model = brms_factor, mpg = 25, cyl_fac = 4)
    mfx1 <- marginaleffects(brms_factor, variables = "cyl_fac", newdata = dat, type = "link")
    mfx2 <- emmeans::emmeans(brms_factor, ~ cyl_fac, var = "cyl_fac", at = list(mpg = 25))
    mfx2 <- emmeans::contrast(mfx2, method = "revpairwise")
    mfx2 <- data.frame(mfx2)[1:2,]
    expect_equal(mfx1$dydx, mfx2$estimate, tolerance = .001)
    expect_equal(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001)
    expect_equal(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001)
})


test_that("plot_cap: no validity", {
  p <- plot_cap(brms_interaction, condition = c("mpg", "vs"))
  vdiffr::expect_doppelganger("brms logit plot_cap", p)
})


test_that("plot_cap: no validity", {
  p <- plot_cap(brms_interaction, condition = c("mpg", "vs"))
  vdiffr::expect_doppelganger("brms logit plot_cap", p)
})


test_that("factor in formula", {
    expect_error(marginaleffects(brms_factor_formula),
                 regexp = "factor")
    expect_error(predictions(brms_factor_formula),
                 regexp = "factor")
})


test_that("bugs stay dead: factor indexing for posterior draws", {
    tmp <- predictions(brms_factor, newdata = datagrid(cyl_fac = 4, mpg = c(10, 20))) 
    expect_error(posteriordraws(tmp), NA)
})


test_that("mo() recognized as factor: Issue #220", {
    # marginaleffects
    mfx1 <- marginaleffects(brms_monotonic)
    mfx2 <- marginaleffects(brms_monotonic, variable = "carb")
    expect_error(marginaleffects(brms_monotonic_factor), regexp = "cannot be used")
    expect_s3_class(mfx1, "marginaleffects")
    expect_s3_class(mfx2, "marginaleffects")

    # comparisons
    expect_error(comparisons(brms_monotonic_factor), regexp = "cannot be used")
    contr1 <- tidy(comparisons(brms_monotonic))
    expect_true(all(paste(c(2, 3, 4, 6, 8), "-", 1) %in% contr1$contrast))
    contr2 <- tidy(comparisons(brms_monotonic, contrast_factor = "pairwise", variables = "carb"))
    expect_equal(nrow(contr2), 15)
})


test_that("multivariate outcome", {
    mod <- download_model("brms_mv_1")

    beta <- get_coef(mod)
    expect_equal(length(beta), 12)

    mfx <- marginaleffects(mod)
    expect_s3_class(mfx, "marginaleffects")

    pred <- predictions(mod)
    expect_s3_class(pred, "predictions")

    comp <- comparisons(mod)
    expect_s3_class(comp, "comparisons")

    draws <- posteriordraws(mfx)
    expect_s3_class(draws, "data.frame")
    expect_true(all(c("drawid", "draw", "rowid") %in% colnames(draws)))
})


test_that("categorical outcome", {
    mod <- download_model("brms_categorical_1")

    mfx <- marginaleffects(mod)
    expect_s3_class(mfx, "marginaleffects")

    pred <- predictions(mod)
    expect_s3_class(pred, "predictions")

    comp <- comparisons(mod)
    expect_s3_class(comp, "comparisons")

    draws <- posteriordraws(mfx)
    expect_s3_class(draws, "data.frame")
    expect_true(all(c("drawid", "draw", "rowid") %in% colnames(draws)))
})


test_that("vignette vdem example", {
    brms_vdem <- download_model("brms_vdem")
    p_response <- predictions(
        brms_vdem,
        type = "response",
        newdata = datagrid(party_autonomy = c(TRUE, FALSE),
                           civil_liberties = .5,
                           region = "Middle East and North Africa"))
    expect_predictions(p_response, se = FALSE)
    p_prediction <- predictions(
        brms_vdem,
        type = "prediction",
        newdata = datagrid(party_autonomy = c(TRUE, FALSE),
                           civil_liberties = .5,
                           region = "Middle East and North Africa"))
    expect_predictions(p_prediction, se = FALSE)
})


test_that("bugs stay dead: character regressors used to produce duplicates", {
    expect_marginaleffects(brms_character, se = FALSE)
    mfx <- marginaleffects(brms_character)
    ti <- tidy(mfx)
    expect_true(length(unique(ti$estimate)) == nrow(ti))
})


test_that("warning: vcov not supported", {
    expect_warning(marginaleffects(brms_numeric, vcov = "HC3"),
                   regexp = "vcov.*not supported")
})
