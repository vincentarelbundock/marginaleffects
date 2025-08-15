skip("works interactively")
skip_on_cran()
withr_library("marginaleffects")
withr_library("tidyverse")
withr_library("survey")
withr_library("rstan")

test_that("mtcars logit with weights works correctly", {
    # mtcars logit
    tmp <- get_dataset("mtcars", "datasets")
    tmp$weights <- tmp$w <- 1:32
    dat <- tmp
    mod <- suppressWarnings(svyglm(
        am ~ mpg + cyl,
        design = svydesign(ids = ~1, weights = ~weights, data = dat),
        family = binomial
    ))

    expect_warning(p1 <- avg_predictions(mod, newdata = dat), "good practice to specify weights")
    p2 <- avg_predictions(mod, wts = "weights", newdata = dat)
    p3 <- avg_predictions(mod, wts = "w", newdata = dat)
    p4 <- avg_predictions(mod, wts = dat$weights)
    expect_false(p1$estimate == p2$estimate)
    expect_false(p1$std.error == p2$std.error)
    expect_equal(p2, p3, ignore_attr = TRUE)
    expect_equal(p2, p4, ignore_attr = TRUE)
})

test_that("by supports weights", {
    withr_library("marginaleffects")
    withr_library("tidyverse")
    withr_library("survey")

    tmp <- get_dataset("mtcars", "datasets")
    tmp$weights <- tmp$w <- 1:32
    dat <- tmp
    mod <- suppressWarnings(svyglm(
        am ~ mpg + cyl,
        design = svydesign(ids = ~1, weights = ~weights, data = dat),
        family = binomial
    ))

    # by supports weights
    p1 <- avg_predictions(mod, wts = "weights", newdata = dat)
    expect_s3_class(p1, "data.frame")
    m1 <- avg_slopes(mod, wts = "weights", newdata = dat, by = "cyl")
    expect_s3_class(m1, "data.frame")
    c1 <- avg_comparisons(mod, wts = "weights", newdata = dat, by = "cyl")
    expect_s3_class(c1, "data.frame")
})

test_that("wts + comparison=avg works correctly", {
    withr_library("marginaleffects")

    # wts + comparison="avg"
    set.seed(100)
    k <- get_dataset("lalonde", "MatchIt")
    k$w <- rchisq(614, 2)
    fit <- lm(re78 ~ treat * (age + educ + race + married + re74), data = k, weights = w)
    cmp1 <- comparisons(fit, variables = "treat", wts = "w")
    cmp2 <- comparisons(fit, variables = "treat", wts = "w", comparison = "differenceavg")
    expect_equal(cmp2$estimate, weighted.mean(cmp1$estimate, k$w), ignore_attr = TRUE)
})

test_that("wts = TRUE correctly extracts weights", {
    withr_library("marginaleffects")

    set.seed(100)
    k <- get_dataset("lalonde", "MatchIt")
    k$w <- rchisq(614, 2)
    fit <- lm(re78 ~ treat * (age + educ + race + married + re74), data = k, weights = w)

    # wts = TRUE correctly extracts weights
    a1 <- avg_comparisons(fit, variables = "treat", wts = "w")
    a2 <- avg_comparisons(fit, variables = "treat", wts = TRUE)
    expect_equal(a1, a2, ignore_attr = TRUE)

    a1 <- avg_comparisons(fit, variables = "treat", by = "married", wts = k$w)
    a2 <- avg_comparisons(fit, variables = "treat", by = "married", wts = TRUE)
    expect_equal(a1, a2, ignore_attr = TRUE)

    a1 <- avg_predictions(fit, wts = "w")
    a2 <- avg_predictions(fit, wts = TRUE)
    expect_equal(a1, a2, ignore_attr = TRUE)

    a1 <- avg_predictions(fit, by = "married", wts = k$w)
    a2 <- avg_predictions(fit, by = "married", wts = TRUE)
    expect_equal(a1, a2, ignore_attr = TRUE)
})

test_that("sanity check for invalid weights", {
    withr_library("marginaleffects")
    withr_library("survey")

    tmp <- get_dataset("mtcars", "datasets")
    tmp$weights <- tmp$w <- 1:32
    dat <- tmp
    mod <- suppressWarnings(svyglm(
        am ~ mpg + cyl,
        design = svydesign(ids = ~1, weights = ~weights, data = dat),
        family = binomial
    ))

    # sanity check
    expect_error(comparisons(mod, wts = "junk"), pattern = "explicitly")
    expect_error(slopes(mod, wts = "junk"), pattern = "explicitly")
})

test_that("vs. Stata comparison", {
    withr_library("marginaleffects")
    withr_library("survey")

    tmp <- get_dataset("mtcars", "datasets")
    tmp$weights <- tmp$w <- 1:32
    dat <- tmp

    # vs. Stata (not clear what SE they use, so we give tolerance)
    mod <- suppressWarnings(svyglm(
        am ~ mpg,
        design = svydesign(ids = ~1, weights = ~weights, data = dat),
        family = binomial
    ))
    tmp <- mod$prior.weights
    stata <- c(.0441066, .0061046)
    mfx <- slopes(mod, wts = tmp, by = "term")
    expect_equal(mfx$estimate[1], stata[1], tolerance = .01, ignore_attr = TRUE)
    expect_equal(mfx$std.error, stata[2], tolerance = 0.002, ignore_attr = TRUE)
})

test_that("Issue #737: lnratioavg with weights", {
    withr_library("marginaleffects")

    # Issue #737
    md <- tibble::tribble(
    ~g, ~device, ~y, ~N, ~p,
    "Control", "desktop", 12403, 103341L, 0.120020127538925,
    "Control", "mobile", 1015, 16192L, 0.0626852766798419,
    "Control", "tablet", 38, 401L, 0.0947630922693267,
    "X", "desktop", 12474, 103063L, 0.121032766366203,
    "X", "mobile", 1030, 16493L, 0.0624507366761656,
    "X", "tablet", 47, 438L, 0.107305936073059,
    "Z", "desktop", 12968, 102867L, 0.126065696481865,
    "Z", "mobile", 973, 16145L, 0.0602663363270362,
    "Z", "tablet", 34, 438L, 0.0776255707762557,
    "W", "desktop", 12407, 103381L, 0.120012381385361,
    "W", "mobile", 1007, 16589L, 0.060702875399361,
    "W", "tablet", 30, 435L, 0.0689655172413793)
    tmp <<- as.data.frame(md)
    tmp <- as.data.frame(md)
    fit <- glm(cbind(y, N - y) ~ g * device, data = tmp, family = binomial())
    cmp1 <- avg_comparisons(
        fit,
        variables = list(g = c("Control", "Z")),
        wts = "N",
        newdata = tmp,
        comparison = "lnratioavg",
        transform = exp
    )
    cmp2 <- predictions(fit, variables = list(g = c("Control", "Z"))) |>
        dplyr::group_by(g) |>
        dplyr::summarise(estimate = weighted.mean(estimate, N)) |>
        as.data.frame()
    expect_equal(
        cmp1$estimate,
        cmp2$estimate[cmp2$g == "Z"] / cmp2$estimate[cmp2$g == "Control"],
        ignore_attr = TRUE
    )
})

test_that("wts shortcuts are internal-only", {
    withr_library("marginaleffects")

    md <- tibble::tribble(
    ~g, ~device, ~y, ~N, ~p,
    "Control", "desktop", 12403, 103341L, 0.120020127538925,
    "Control", "mobile", 1015, 16192L, 0.0626852766798419,
    "Control", "tablet", 38, 401L, 0.0947630922693267,
    "X", "desktop", 12474, 103063L, 0.121032766366203,
    "X", "mobile", 1030, 16493L, 0.0624507366761656,
    "X", "tablet", 47, 438L, 0.107305936073059,
    "Z", "desktop", 12968, 102867L, 0.126065696481865,
    "Z", "mobile", 973, 16145L, 0.0602663363270362,
    "Z", "tablet", 34, 438L, 0.0776255707762557,
    "W", "desktop", 12407, 103381L, 0.120012381385361,
    "W", "mobile", 1007, 16589L, 0.060702875399361,
    "W", "tablet", 30, 435L, 0.0689655172413793)
    tmp <- as.data.frame(md)
    fit <- glm(cbind(y, N - y) ~ g * device, data = tmp, family = binomial())

    # wts shortcuts are internal-only
    expect_error(
        avg_comparisons(fit, variables = "g", wts = "N", comparison = "lnratioavgwts", transform = exp),
        pattern = "check_choice"
    )
})

test_that("lnratioavg = lnratio with by", {
    withr_library("marginaleffects")

    md <- tibble::tribble(
    ~g, ~device, ~y, ~N, ~p,
    "Control", "desktop", 12403, 103341L, 0.120020127538925,
    "Control", "mobile", 1015, 16192L, 0.0626852766798419,
    "Control", "tablet", 38, 401L, 0.0947630922693267,
    "X", "desktop", 12474, 103063L, 0.121032766366203,
    "X", "mobile", 1030, 16493L, 0.0624507366761656,
    "X", "tablet", 47, 438L, 0.107305936073059,
    "Z", "desktop", 12968, 102867L, 0.126065696481865,
    "Z", "mobile", 973, 16145L, 0.0602663363270362,
    "Z", "tablet", 34, 438L, 0.0776255707762557,
    "W", "desktop", 12407, 103381L, 0.120012381385361,
    "W", "mobile", 1007, 16589L, 0.060702875399361,
    "W", "tablet", 30, 435L, 0.0689655172413793)
    tmp <- as.data.frame(md)
    fit <- glm(cbind(y, N - y) ~ g * device, data = tmp, family = binomial())

    # lnratioavg = lnratio with `by`
    cmp1 <- avg_comparisons(fit, variables = "g", by = "device", wts = "N", comparison = "lnratioavg", transform = exp)
    cmp2 <- avg_comparisons(fit, variables = "g", by = "device", wts = "N", comparison = "lnratio", transform = exp)
    expect_equal(cmp1, cmp2, ignore_attr = TRUE)

    # lnratioavg + wts produces same results in this particular case, because there are only the g*device predictors
    cmp1 <- avg_comparisons(fit, variables = "g", by = "device", wts = "N", comparison = "lnratioavg", transform = exp)
    cmp2 <- avg_comparisons(fit, variables = "g", by = "device", wts = "N", comparison = "lnratioavg", transform = exp)
    expect_equal(cmp1, cmp2, ignore_attr = TRUE)
})

test_that("Issue #865: differenceavg comparison", {
    withr_library("marginaleffects")

    # Issue #865
    # fmt: skip
    d = data.frame( outcome = c( 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0 ), foo = c( 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ), bar = c( 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1 ) )

    mod = glm(
        outcome ~ foo + bar,
        family = "binomial",
        data = d
    )
    cmp1 <- avg_comparisons(mod, variables = list(foo = 0:1), type = "response", comparison = "difference")
    cmp2 <- comparisons(mod, variables = list(foo = 0:1), type = "response", comparison = "differenceavg")
    expect_equal(cmp1$estimate, cmp2$estimate, ignore_attr = TRUE)
})

test_that("Issue #870: predictions with weights", {
    withr_library("marginaleffects")
    withr_library("HistData")

    # Issue #870
    Guerry <- get_dataset("Guerry", "HistData")
    Guerry <- Guerry[which(Guerry$Region != ""), ]
    mod <- lm(Literacy ~ Pop1831 * Desertion, data = Guerry)
    p1 <- predictions(mod, by = "Region", wts = "Donations")
    p2 <- predictions(mod, by = "Region")
    expect_s3_class(p1, "predictions")
    expect_false(any(p1$estimate == p2$estimate))
})

test_that("brms with weights works correctly", {
    withr_library("marginaleffects")
    withr_library("rstan")

    # brms
    set.seed(1024)
    mod <- marginaleffects:::modelarchive_model("brms_numeric2")
    w <- runif(32)
    cmp1 <- comparisons(mod, comparison = "differenceavg")
    cmp2 <- comparisons(mod, wts = w, comparison = "differenceavg")
    expect_true(all(cmp1$estimate != cmp2$estimate))
})
