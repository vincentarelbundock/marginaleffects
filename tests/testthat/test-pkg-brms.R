requiet("brms")
requiet("cmdstanr")
requiet("emmeans")
requiet("broom")
skip_if_not_installed("data.table") # cmdstanr


void <- capture.output({
    dat <- mtcars
    dat$logic <- as.logical(dat$vs)
    dat$cyl_fac <- as.factor(dat$cyl)
    dat$cyl_cha <- as.character(dat$cyl)
    mod_one <- brm(am ~ hp, data = dat, family = bernoulli(),
                   backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_two <- brm(am ~ mpg + hp, data = dat, family = bernoulli(),
                   backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_character <- brm(am ~ mpg + cyl_cha, data = dat, family = bernoulli(),
                         backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_factor <- brm(am ~ mpg + cyl_fac, data = dat, family = bernoulli(),
                      backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_factor_formula <- brm(am ~ mpg + factor(cyl), data = dat, family = bernoulli(),
                              backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_int <- brm(am ~ mpg * vs, data = dat, family = bernoulli(),
                   backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_log <- brm(am ~ logic, data = dat, family = bernoulli(),
                   backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    prior1 <- prior(normal(0, 10), class = b) + prior(cauchy(0, 2), class = sd)
    mod_epi <- brm(count ~ zAge + zBase * Trt + (1 | patient),
                   data = epilepsy, family = poisson(), prior = prior1,
                   seed = 1024, iter = 1000, silent = 2, backend = "cmdstanr")
    mod_ran <- brm(rating ~ treat + period + (1 | subject), family = cumulative(), data = inhaler,
                   silent = 2, backend = "cmdstanr")
    mod_mo1 <- brm(mpg ~ hp + mo(carb), data = mtcars, silent = 2, backend = "cmdstanr")
    mod_mo2 <- brm(mpg ~ hp + factor(cyl) + mo(carb), data = mtcars, silent = 2, backend = "cmdstanr")
})

test_that("marginaleffects vs. emmeans", {
    mfx <- marginaleffects(mod_two, newdata = datagrid(mpg = 20, hp = 100),
                           variables = "mpg", type = "link")
    em <- emtrends(mod_two, ~mpg, "mpg", at = list(mpg = 20, hp = 100))
    em <- tidy(em)
    expect_equal(mfx$dydx, em$mpg.trend)
    expect_equal(mfx$conf.low, em$lower.HPD)
    expect_equal(mfx$conf.high, em$upper.HPD)
    # tolerance is less good for back-transformed response
    mfx <- marginaleffects(mod_two, newdata = datagrid(mpg = 20, hp = 100),
                           variables = "mpg", type = "response")
    em <- emtrends(mod_two, ~mpg, "mpg", at = list(mpg = 20, hp = 100), trans = TRUE)
    em <- tidy(em)
    expect_equal(mfx$dydx, em$mpg.trend, tolerance = .1)
    expect_equal(mfx$conf.low, em$lower.HPD, tolerance = .01)
    expect_equal(mfx$conf.high, em$upper.HPD, tolerance = .1)
})

test_that("brms: cumulative: marginaleffects: no validity", {
    expect_marginaleffects(mod_ran, se = FALSE)
})

test_that("brms: logical regressor", {
    mfx <- marginaleffects(mod_log)
    expect_s3_class(mfx, "marginaleffects")
    expect_equal(nrow(mfx), nrow(attr(mfx, "posterior_draws")))
})


test_that("predictions: hypothetical group", {
    mod <- insight::download_model("brms_mixed_3")
    nd <- datagrid(model = mod, grp = 4, subgrp = 12)
    nd$Subject <- 1000
    set.seed(1024)
    p1 <- predictions(mod, newdata = nd, allow_new_levels = TRUE)
    set.seed(1024)
    p2 <- predictions(mod, newdata = nd, allow_new_levels = TRUE, sample_new_levels = "gaussian")
    set.seed(1024)
    p3 <- predictions(mod, newdata = nd, allow_new_levels = TRUE, sample_new_levels = "uncertainty")
    expect_false(any(p1$predicted == p2$predicted))
    expect_equal(p1, p3)
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
    p1 <- predictions(mod_ran)
    p2 <- predictions(mod_ran, re_formula = NA)
    expect_true(mean(p1$conf.low < p2$conf.low) > .99) # tolerance
    expect_true(mean(p1$conf.high > p2$conf.high) > .99) # tolerance
    expect_error(predictions(mod_ran, include_random = FALSE)) # only for lme4
})

test_that("marginaleffects: ordinal no validity", {
    mod <- insight::download_model("brms_ordinal_1")
    expect_marginaleffects(mod, se = FALSE)
})


test_that("predict new unit: no validity", {
    dat1 <- dat2 <- datagrid(model = mod_epi)
    dat2$patient <- NA
    set.seed(1024)
    mfx1 <- marginaleffects(mod_epi, newdata = dat1)
    set.seed(1024)
    mfx2 <- marginaleffects(mod_epi, newdata = dat2, re_formula = NULL)
    expect_false(any(mfx1$dydx == mfx2$dydx))
})


test_that("tidy()", {
    mfx <- marginaleffects(mod_factor)
    ti <- tidy(mfx)
    expect_s3_class(ti, "data.frame")
    expect_equal(dim(ti), c(3, 6))
    expect_true(all(c("term", "estimate", "conf.low") %in% colnames(ti)))
})


test_that("predictions: no validity", {
    # simple
    pred <- predictions(mod_two, newdata = datagrid(hp = c(100, 120)))
    expect_predictions(pred, se = FALSE) 
    expect_equal(dim(attr(pred, "posterior_draws")), c(2, 2000))
    # interaction
    pred <- predictions(mod_int, newdata = datagrid(mpg = c(20, 25)))
    expect_predictions(pred, se = FALSE)
    # factor in data frame
    pred <- predictions(mod_factor, newdata = datagrid())
    expect_predictions(pred, se = FALSE)
})


test_that("predictions: prediction vs. expectation vs. include_random", {
    # prediction vs. response
    p1 <- suppressWarnings(predictions(mod_epi, type = "prediction"))
    p2 <- suppressWarnings(predictions(mod_epi, type = "response"))
    expect_true(all(p1$conf.low < p2$conf.low))
    expect_true(all(p1$conf.high > p2$conf.high))
    # re_formula
    p1 <- predictions(mod_epi, newdata = datagrid(patient = 1))
    p2 <- predictions(mod_epi, newdata = datagrid(patient = 1), re_formula = NA)
    expect_false(p1$predicted == p2$predicted)
    expect_false(p1$conf.low == p2$conf.low)
    expect_false(p1$conf.high == p2$conf.high)
})


test_that("marginaleffects vs. emmeans: multiple types are correctly aligned", {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("emmeans")
    library(ggplot2)
    mfx <- marginaleffects(mod_int, variables = "mpg", type = c("response", "link"),
                           newdata = datagrid(vs = 0:1, mpg = 20))
    em_r <- emmeans::emtrends(mod_int, ~vs, var = "mpg", at = list(vs = c(0, 1), mpg = 20), epred = TRUE)
    em_l <- emmeans::emtrends(mod_int, ~vs, var = "mpg", at = list(vs = c(0, 1), mpg = 20))
    em_r <- data.frame(em_r)
    em_l <- data.frame(em_l)
    expect_equal(mfx[mfx$type == "link", "dydx"], em_l$mpg.trend)
    expect_equal(mfx[mfx$type == "link", "conf.low"], em_l$lower.HPD)
    expect_equal(mfx[mfx$type == "link", "conf.high"], em_l$upper.HPD)
    expect_equal(mfx[mfx$type == "response", "dydx"], em_r$mpg.trend, tolerance = .01)
    expect_equal(mfx[mfx$type == "response", "conf.low"], em_r$lower.HPD, tolerance = .01)
    expect_equal(mfx[mfx$type == "response", "conf.high"], em_r$upper.HPD, tolerance = .01)

    # easier to see correct alignment of draws in a density plot
    tmp <- posteriordraws(mfx)
    p <- ggplot(tmp, aes(x = draw, fill = factor(vs))) +
         geom_density(alpha = .4) +
         facet_grid(~type, scales = "free")
    vdiffr::expect_doppelganger("posterior_draws alignment with multi-type mfx", p)
})


test_that("predictions vs. emmeans", {
    skip_if_not_installed("emmeans")
    em <- emmeans::emmeans(mod_one, ~hp, "hp", at = list(hp = c(100, 120)))
    em <- data.frame(em)
    pred <- predictions(mod_one, newdata = datagrid(hp = c(100, 120)), type = "link")
    expect_equal(pred$predicted, em$emmean)
    expect_equal(pred$conf.low, em$lower.HPD)
    expect_equal(pred$conf.high, em$upper.HPD)
})


test_that("marginalmeans vs. emmeans", {
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")
    expect_error(marginalmeans(mod_factor, variables = "cyl_fac", type = "link"), regexp = "github.*issues")
    # emmeans::emmeans(mod_factor, specs = ~cyl_fac)
})


test_that("marginaleffects: no validity", {
    expect_marginaleffects(mod_two, se = FALSE)
    expect_marginaleffects(mod_int, se = FALSE)
    expect_marginaleffects(mod_factor, se = FALSE)
    # credible intervals and posterior draws
    tmp <- marginaleffects(mod_factor)
    expect_true("conf.low" %in% colnames(tmp))
    expect_true(all(tmp$dydx > tmp$conf.low))
    expect_true(all(tmp$dydx < tmp$conf.high))
    expect_false(is.null(attr(tmp, "posterior_draws")))
    expect_equal(nrow(attr(tmp, "posterior_draws")), nrow(tmp))
})


test_that("marginaleffects vs. emmeans", {
    skip_if_not_installed("emmeans")
    # NOTE: `emmeans` reports the median draw for `dydx`.

    ## known frequentist example to compare syntax
    # mod_one_freq <- glm(am ~ hp, data = mtcars, family = binomial)
    # marginaleffects(mod_one_freq, newdata = datagrid(hp = 147), type = "link")
    # emmeans::emtrends(mod_one_freq, specs = ~hp, var = "hp", at = list(hp = 147))

    # one variable: link scale
    mfx1 <- marginaleffects(mod_one, variables = "hp", newdata = datagrid(hp = 110), type = "link")
    mfx2 <- as.data.frame(emmeans::emtrends(mod_one, ~hp, var = "hp", at = list(hp = 110)))
    expect_equal(mfx1$dydx, mfx2$hp.trend)
    expect_equal(mfx1$conf.low, mfx2$lower.HPD)
    expect_equal(mfx1$conf.high, mfx2$upper.HPD)

    ## one variable: response scale
    mfx1 <- marginaleffects(mod_one, variables = "hp", newdata = datagrid(hp = 110))
    mfx2 <- as.data.frame(emmeans::emtrends(mod_one, ~hp, var = "hp", at = list(hp = 110), transform = "response"))
    expect_equal(mfx1$dydx, mfx2$hp.trend, tolerance = .001)
    expect_equal(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001)
    expect_equal(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001)

    # numeric + factor: numeric variable
    dat <- datagrid(model = mod_factor, mpg = 25, cyl_fac = 4)
    mfx1 <- marginaleffects(mod_factor, variables = "mpg", newdata = dat, type = "link")
    mfx2 <- as.data.frame(emmeans::emtrends(mod_factor, ~mpg, var = "mpg", at = list(mpg = 25, cyl_fac = 4)))
    expect_equal(mfx1$dydx, mfx2$mpg.trend, tolerance = .001)
    expect_equal(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001)
    expect_equal(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001)

    # numeric + factor: factor
    dat <- datagrid(model = mod_factor, mpg = 25, cyl_fac = 4)
    mfx1 <- marginaleffects(mod_factor, variables = "cyl_fac", newdata = dat, type = "link")
    mfx2 <- emmeans::emmeans(mod_factor, ~ cyl_fac, var = "cyl_fac", at = list(mpg = 25))
    mfx2 <- emmeans::contrast(mfx2, method = "revpairwise")
    mfx2 <- data.frame(mfx2)[1:2,]
    expect_equal(mfx1$dydx, mfx2$estimate, tolerance = .001)
    expect_equal(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001)
    expect_equal(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001)
})


test_that("plot_cap: no validity", {
  p <- plot_cap(mod_int, condition = c("mpg", "vs"))
  vdiffr::expect_doppelganger("brms logit plot_cap", p)
})


test_that("plot_cap: no validity", {
  p <- plot_cap(mod_int, condition = c("mpg", "vs"))
  vdiffr::expect_doppelganger("brms logit plot_cap", p)
})


test_that("factor in formula", {
    skip("https://github.com/easystats/insight/issues/469")
    # marginaleffects
    expect_marginaleffects(mod_factor_formula, se = FALSE)
    # predictions
    pred <- predictions(mod_factor_formula, newdata = datagrid())
    expect_predictions(pred, se = FALSE)
})


test_that("bugs stay dead: factor indexing for posterior draws", {
    tmp <- predictions(mod_factor, newdata = datagrid(cyl_fac = 4, mpg = c(10, 20))) 
    expect_error(posteriordraws(tmp), NA)
})

test_that("bugs stay dead: character regressors used to produce duplicates", {
    expect_marginaleffects(mod_character, se = FALSE)
    mfx <- marginaleffects(mod_character)
    ti <- tidy(mfx)
    expect_true(length(unique(ti$estimate)) == nrow(ti))
})

test_that("mo() recognized as factor: Issue #220", {
    # marginaleffects
    mfx1 <- marginaleffects(mod_mo1)
    mfx2 <- marginaleffects(mod_mo1, variable = "carb")
    expect_error(marginaleffects(mod_mo2), regexp = "cannot be used")
    expect_s3_class(mfx1, "marginaleffects")
    expect_s3_class(mfx2, "marginaleffects")

    # comparisons
    expect_error(comparisons(mod_mo2), regexp = "cannot be used")
    contr1 <- tidy(comparisons(mod_mo1))
    expect_true(all(paste(c(2, 3, 4, 6, 8), "-", 1) %in% contr1$contrast))
    contr2 <- tidy(comparisons(mod_mo1, contrast_factor = "pairwise", variables = "carb"))
    expect_equal(nrow(contr2), 15)
})



test_that("multivariate outcome", {
    mod <- insight::download_model("brms_mv_1")

    beta <- get_coef(mod)
    expect_equal(length(beta), 12)

    mfx <- marginaleffects(mod)
    expect_s3_class(mfx, "marginaleffects")

    pred <- predictions(mod)
    expect_s3_class(pred, "predictions")

    comp <- comparisons(mod)
    expect_s3_class(comp, "comparisons")
})


test_that("categorical outcome", {
    mod <- insight::download_model("brms_categorical_1_wt")

    mfx <- marginaleffects(mod)
    expect_s3_class(mfx, "marginaleffects")

    pred <- predictions(mod)
    expect_s3_class(pred, "predictions")

    comp <- comparisons(mod)
    expect_s3_class(comp, "comparisons")
})

