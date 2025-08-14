# skip("many broken tests, keeping tiny for now")
skip_on_cran()

EXPENSIVE <- TRUE

test_that("simulation-based inference works correctly", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    set.seed(1024)
    R <- 25
    mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

    # simulation-based inference
    x <- mod |>
        avg_predictions() |>
        inferences(method = "simulation", R = R)
    expect_s3_class(x, "predictions")

    x <- mod |>
        slopes() |>
        inferences(method = "simulation", R = R) |>
        head()
    expect_s3_class(x, "slopes")

    x <- mod |>
        predictions(vcov = "HC3") |>
        inferences(method = "simulation", R = R) |>
        head()
    expect_s3_class(x, "predictions")

    x <- mod |>
        comparisons() |>
        inferences(method = "simulation", R = R)
    expect_true(is.matrix(attr(x, "mfx")@draws))
})

test_that("boot-based inference works correctly", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    set.seed(1234)
    R <- 25
    mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

    # {boot}
    x <- mod |>
        avg_predictions() |>
        inferences(method = "boot", R = R)
    expect_s3_class(x, "predictions")
    expect_equal(nrow(x), 1, ignore_attr = TRUE)

    # head works
    set.seed(1234)
    x <- mod |>
        slopes() |>
        inferences(method = "boot", R = R)
    expect_s3_class(head(x), "slopes")
    expect_equal(nrow(x), 300, ignore_attr = TRUE)
    expect_equal(nrow(head(x)), 6, ignore_attr = TRUE)

    # avg_ works
    set.seed(1234)
    x <- mod |>
        avg_slopes() |>
        inferences(method = "boot", R = R)
    expect_s3_class(x, "slopes") # should be slopes
    expect_equal(nrow(x), 2, ignore_attr = TRUE)

    x <- mod |>
        predictions(vcov = "HC3") |>
        inferences(method = "boot", R = R) |>
        head()
    expect_s3_class(x, "predictions")

    x <- mod |>
        comparisons() |>
        inferences(method = "boot", R = R)
    expect_s3_class(attr(x, "mfx")@inferences, "boot")

    nd <<- datagrid(Sepal.Length = range, model = mod)
    x <- mod |>
        comparisons(variables = "Sepal.Width", newdata = nd) |>
        inferences(method = "boot", R = R)
    expect_equal(nrow(x), 2, ignore_attr = TRUE)

    x <- mod |>
        avg_comparisons() |>
        inferences(method = "simulation", R = R)
    expect_equal(nrow(x), 2, ignore_attr = TRUE)

    x <- x |> get_draws()
    expect_equal(nrow(x), 2 * R, ignore_attr = TRUE)
})

test_that("rsample-based inference works correctly", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    set.seed(1234)
    R <- 25
    mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

    # {rsample}
    x <- mod |>
        avg_predictions() |>
        inferences(method = "rsample", R = R) |>
        suppressWarnings()
    expect_equal(x$conf.low, 3.665, tolerance = 1e-3)
    expect_s3_class(x, "predictions")

    x <- mod |>
        slopes() |>
        inferences(method = "rsample", R = R) |>
        suppressWarnings()
    expect_s3_class(x, "slopes")

    x <- mod |>
        predictions(vcov = "HC3") |>
        inferences(method = "rsample", R = R) |>
        suppressWarnings()
    expect_s3_class(x, "predictions")

    x <- mod |>
        comparisons() |>
        inferences(method = "rsample", R = R) |>
        suppressWarnings()
    expect_s3_class(attr(x, "mfx")@inferences, "bootstraps")

    nd <<- datagrid(Sepal.Length = range, model = mod)
    x <- mod |>
        comparisons(variables = "Sepal.Width", newdata = nd) |>
        inferences(method = "rsample", R = R) |>
        suppressWarnings()
    expect_equal(nrow(x), 2, ignore_attr = TRUE)

    x <- mod |>
        avg_comparisons() |>
        inferences(method = "rsample", R = R) |>
        get_draws() |>
        suppressWarnings()
    expect_equal(nrow(x), 2 * R, ignore_attr = TRUE)
})

test_that("fwb-based inference works correctly", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    set.seed(1234)
    R <- 25
    mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

    # fwb no validity check
    x <- mod |>
        comparisons() |>
        inferences(method = "fwb", R = R) |>
        suppressWarnings()
    expect_equal(nrow(x), 300, ignore_attr = TRUE)
    expect_equal(x$std.error[1:3], c(0.0642131648304821, 0.0444891291752277, 0.0442572266844693))

    x <- mod |>
        avg_comparisons() |>
        inferences(method = "fwb", R = R) |>
        suppressWarnings()
    expect_equal(nrow(x), 2, ignore_attr = TRUE)
})

test_that("fwb error when user supplied its own weights", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    dat <- transform(mtcars, w = runif(32))
    mod <- lm(mpg ~ hp, data = dat)
    expect_error(inferences(comparisons(mod, wts = "w"), method = "fwb"), pattern = "wts")
})

test_that("cross-comparisons with boot work correctly", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    # Issue #856
    tmp <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
    cmp <- avg_comparisons(tmp, variables = list(Sepal.Length = 1, Species = "reference"), cross = TRUE) |>
        inferences(method = "boot", R = 5) |>
        suppressWarnings()
    expect_s3_class(cmp, "comparisons")
    expect_equal(nrow(cmp), 2, ignore_attr = TRUE)
})

test_that("predictions with by argument work correctly", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    # Issue #853
    m <- glm(am ~ mpg + hp + cyl, data = mtcars, family = binomial)
    p <- avg_predictions(m, by = "cyl") |>
        inferences(method = "boot", R = 5) |>
        suppressWarnings()
    expect_s3_class(p, "predictions")

    p <- predictions(m, by = "cyl") |>
        inferences(method = "boot", R = 5) |>
        suppressWarnings()
    expect_s3_class(p, "predictions")
})

test_that("simulation-based inference preserves original estimates", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    # Issue #851: simulation-based inference use the original estimates, not the mean/median of simulations
    mod <- glm(vs ~ hp + mpg + am, data = mtcars, family = binomial)
    cmp1 <- avg_comparisons(mod)
    cmp2 <- cmp1 |> inferences(method = "simulation", R = 500)
    expect_equal(cmp1$estimate, cmp2$estimate, ignore_attr = TRUE)
})

test_that("avg_predictions with by argument work correctly", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    # mfxplainer bug
    mod <- lm(mpg ~ hp + cyl, data = mtcars)
    p <- avg_predictions(mod, by = "cyl") |> inferences(method = "simulation", R = 25)
    expect_s3_class(p, "predictions")
})

test_that("inferences with hypotheses work correctly", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    mod <- lm(mpg ~ hp + cyl, data = mtcars)
    p <- hypotheses(mod, hypothesis = "hp/cyl=1") |>
        inferences(method = "boot", R = 25) |>
        suppressWarnings()
    expect_s3_class(p, "hypotheses")

    p <- hypotheses(mod, hypothesis = "hp/cyl=1") |> inferences(method = "simulation", R = 25)
    expect_s3_class(p, "hypotheses")
})

test_that("clarify integration works correctly", {
    withr_library("marginaleffects")
    withr_library("clarify")
    withr_library("MatchIt")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    data("lalonde", package = "MatchIt")
    set.seed(1025)
    fit <- glm(
        I(re78 == 0) ~ treat * (age + educ + race + married + nodegree + re74 + re75),
        data = lalonde,
        family = binomial
    )
    sim_coefs <- clarify::sim(fit)
    ATT_fun <- function(fit) {
        d <- subset(lalonde, treat == 1)
        d$treat <- 1
        p1 <- mean(predict(fit, newdata = d, type = "response"))
        d$treat <- 0
        p0 <- mean(predict(fit, newdata = d, type = "response"))
        c(`E[Y(0)]` = p0, `E[Y(1)]` = p1, `RR` = p1 / p0)
    }
    sim_est <- sim_apply(sim_coefs, ATT_fun, verbose = FALSE)
    s1 <- summary(sim_est)
    s3 <- avg_predictions(fit, variables = "treat", type = "response", newdata = subset(lalonde, treat == 1)) |>
        inferences(method = "simulation", R = 1000)
    expect_equal(s1[1:2, 2], s3$conf.low, tolerance = .03, ignore_attr = TRUE)
    expect_equal(s1[1:2, 3], s3$conf.high, tolerance = .03, ignore_attr = TRUE)
})

test_that("inferences preserves correct scale", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    # issue #1124: inferences is on the correct scale
    set.seed(1024)
    dat <- read.csv(testthat::test_path("modelarchive/data/impartiality.csv"))
    m <- glm(
        impartial ~ equal * democracy + continent,
        data = dat,
        family = binomial
    )
    p <- predictions(m, by = "democracy", type = "response") |>
        inferences(method = "simulation", R = 100)
    expect_true(all(p$estimate > 0 & p$estimate < 1))
    expect_true(all(p$conf.low > 0 & p$conf.low < 1))
    expect_true(all(p$conf.high > 0 & p$conf.high < 1))
    expect_true(all(p$conf.low < p$estimate & p$conf.high > p$estimate))

    p2 <- predictions(m, by = "democracy", type = "response")
    expect_equal(p2$estimate, p$estimate, ignore_attr = TRUE)
})

test_that("mixed models work with inferences", {
    withr_library("marginaleffects")
    withr_library("lme4")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    # Issue #1054
    mod <- glmer(
        cbind(incidence, size - incidence) ~ period + (1 | herd),
        data = cbpp,
        family = binomial
    )
    cmp <- avg_comparisons(mod, variables = "period", vcov = FALSE) |>
        inferences(method = "simulation", R = 15)
    expect_s3_class(cmp, "comparisons")
})

test_that("conformal inference works correctly", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    # Issue #1407: conformal inference with `residual_sq` scores.
    set.seed(48103)
    dat = get_dataset("military")
    idx = sample(c("train", "calibration", "test"), nrow(dat), replace = TRUE)
    dat = split(dat, idx)
    train = dat$train
    calib = dat$calibration
    test = dat$test
    mod = lm(rank ~ grade + branch + gender + race, data = train)
    p = predictions(mod, conf_level = 0.9) |>
        inferences(
            method = "conformal_split",
            conformal_calibration = calib,
            conformal_score = "residual_abs",
            conformal_test = test
        )
    coverage = mean(p$rank > p$pred.low & p$rank < p$pred.high)
    expect_equal(round(coverage, 2), .9, ignore_attr = TRUE)

    p = predictions(mod, conf_level = 0.9) |>
        inferences(
            method = "conformal_split",
            conformal_calibration = calib,
            conformal_score = "residual_sq",
            conformal_test = test
        )
    coverage = mean(p$rank > p$pred.low & p$rank < p$pred.high)
    expect_equal(round(coverage, 2), .9, ignore_attr = TRUE)
})

test_that("rsample handles non-unique terms correctly", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    # Bug: rsample collapses non-unique term
    mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
    k <- avg_comparisons(mod) |>
        inferences(method = "rsample", R = 25) |>
        suppressWarnings()
    expect_s3_class(k, "comparisons")
})

test_that("estimator function works correctly", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    # `estimator` function
    lalonde <- get_dataset("lalonde")
    estimator <- function(data) {
        fit1 <- glm(treat ~ age + educ + race, family = binomial, data = data)
        ps <- predict(fit1, type = "response")
        m <- lm(re78 ~ treat * (re75 + age + educ + race), data = data, weight = ps)
        avg_comparisons(m, variables = "treat", wts = ps, vcov = FALSE)
    }
    cmp <- inferences(lalonde, method = "rsample", estimator = estimator, R = 25) |>
        suppressWarnings()
    expect_s3_class(cmp, "comparisons")

    expect_error(inferences(lalonde, method = "rsample"), "when supplying a function to the `estimator` argument.")
    expect_error(
        inferences(estimator(lalonde), estimator = estimator, method = "rsample"),
        "The `x` argument must be a raw data frame when using the `estimator` argument."
    )
    expect_no_error(inferences(lalonde, method = "rsample", estimator = estimator, R = 3)) |>
        suppressWarnings()
})

test_that("survival models work with inferences", {
    withr_library("marginaleffects")
    withr_library("survival")
    withr_library("splines")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    # survival vignette
    model <- coxph(
        Surv(dtime, death) ~ hormon * factor(grade) + ns(age, df = 2),
        data = rotterdam
    )
    nd <<- datagrid(
        hormon = unique,
        grade = unique,
        dtime = seq(36, 7043, length.out = 25),
        grid_type = "counterfactual",
        model = model
    )

    p <- predictions(model,
        vcov = FALSE,
        type = "survival",
        by = c("dtime", "hormon", "grade"),
        newdata = nd)
    p <- inferences(p, method = "rsample", R = 25) |> suppressWarnings()
    expect_true(all(p$estimate >= p$conf.low))
    expect_true(all(p$estimate <= p$conf.high))
})

test_that("interval validity tests for all methods", {
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    # Minimal interval validity tests for all methods
    set.seed(1024)
    mod_test <- lm(mpg ~ hp + cyl, data = mtcars)
    pred_default <- avg_predictions(mod_test)
    cmp_default <- avg_comparisons(mod_test)
    pred_sim <- avg_predictions(mod_test) |> inferences(method = "simulation", R = 50)
    cmp_sim <- avg_comparisons(mod_test) |> inferences(method = "simulation", R = 50)
    pred_boot <- avg_predictions(mod_test) |>
        inferences(method = "boot", R = 50) |>
        suppressWarnings()
    cmp_boot <- avg_comparisons(mod_test) |>
        inferences(method = "boot", R = 50) |>
        suppressWarnings()
    pred_fwb <- avg_predictions(mod_test) |>
        inferences(method = "fwb", R = 50) |>
        suppressWarnings()
    cmp_fwb <- avg_comparisons(mod_test) |>
        inferences(method = "fwb", R = 50) |>
        suppressWarnings()
    pred_rsample <- avg_predictions(mod_test) |>
        inferences(method = "rsample", R = 50) |>
        suppressWarnings()
    cmp_rsample <- avg_comparisons(mod_test) |>
        inferences(method = "rsample", R = 50) |>
        suppressWarnings()

    # 1. Test interval validity: conf.low < estimate < conf.high
    expect_true(all(pred_sim$conf.low < pred_sim$estimate & pred_sim$estimate < pred_sim$conf.high))
    expect_true(all(cmp_sim$conf.low < cmp_sim$estimate & cmp_sim$estimate < cmp_sim$conf.high))
    expect_true(all(pred_boot$conf.low < pred_boot$estimate & pred_boot$estimate < pred_boot$conf.high))
    expect_true(all(cmp_boot$conf.low < cmp_boot$estimate & cmp_boot$estimate < cmp_boot$conf.high))
    expect_true(all(pred_fwb$conf.low < pred_fwb$estimate & pred_fwb$estimate < pred_fwb$conf.high))
    expect_true(all(cmp_fwb$conf.low < cmp_fwb$estimate & cmp_fwb$estimate < cmp_fwb$conf.high))
    expect_true(all(pred_rsample$conf.low < pred_rsample$estimate & pred_rsample$estimate < pred_rsample$conf.high))
    expect_true(all(cmp_rsample$conf.low < cmp_rsample$estimate & cmp_rsample$estimate < cmp_rsample$conf.high))

    # 2. Test that intervals differ from default delta method
    expect_false(all(abs(pred_sim$conf.low - pred_default$conf.low) < 1e-4))
    expect_false(all(abs(pred_sim$conf.high - pred_default$conf.high) < 1e-4))
    expect_false(all(abs(cmp_sim$conf.low - cmp_default$conf.low) < 1e-4))
    expect_false(all(abs(cmp_sim$conf.high - cmp_default$conf.high) < 1e-4))
    expect_false(all(abs(pred_boot$conf.low - pred_default$conf.low) < 1e-4))
    expect_false(all(abs(pred_boot$conf.high - pred_default$conf.high) < 1e-4))
    expect_false(all(abs(cmp_boot$conf.low - cmp_default$conf.low) < 1e-4))
    expect_false(all(abs(cmp_boot$conf.high - cmp_default$conf.high) < 1e-4))
    expect_false(all(abs(pred_rsample$conf.low - pred_default$conf.low) < 1e-4))
    expect_false(all(abs(pred_rsample$conf.high - pred_default$conf.high) < 1e-4))
    expect_false(all(abs(cmp_rsample$conf.low - cmp_default$conf.low) < 1e-4))
    expect_false(all(abs(cmp_rsample$conf.high - cmp_default$conf.high) < 1e-4))
    expect_false(all(abs(pred_fwb$conf.low - pred_default$conf.low) < 1e-4))
    expect_false(all(abs(pred_fwb$conf.high - pred_default$conf.high) < 1e-4))
    expect_false(all(abs(cmp_fwb$conf.low - cmp_default$conf.low) < 1e-4))
    expect_false(all(abs(cmp_fwb$conf.high - cmp_default$conf.high) < 1e-4))
})

test_that("simulation-based inference respects vcov argument", {
    skip("Issue #1548: still fails")
    withr_library("marginaleffects")

    # Skip if expensive tests are disabled
    skip_if_not(EXPENSIVE, "expensive")

    # Issue #1548
    # simulation-based inference respects `vcov` argument
    mod <- lm(mpg ~ hp + cyl, data = mtcars)
    set.seed(48103)
    h1 <- hypotheses(mod, hypothesis = "hp/cyl=1", vcov = "HC3") |>
        inferences(method = "simulation", R = 25)
    set.seed(48103)
    h2 <- hypotheses(mod, hypothesis = "hp/cyl=1", vcov = "HC3") |>
        inferences(method = "simulation", R = 25)
    set.seed(48103)
    h3 <- hypotheses(mod, hypothesis = "hp/cyl=1") |>
        inferences(method = "simulation", R = 25)
    expect_equal(h1$conf.low, h2$conf.low, ignore_attr = TRUE)
    expect_equal(h1$conf.high, h2$conf.high, ignore_attr = TRUE)
    expect_false(isTRUE(all.equal(h1$conf.low, h3$conf.low)))
    expect_false(isTRUE(all.equal(h1$conf.high, h3$conf.high)))
})
