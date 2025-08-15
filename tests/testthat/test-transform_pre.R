test_that("transform_pre: manual average contrast", {
    skip_on_cran()
    skip_if_not_installed("modelsummary")

    withr_library("modelsummary")

    tol <- .0001

    # manual average contrast
    mod <- glm(am ~ vs + mpg, data = mtcars, family = binomial)
    cmp1 <- avg_comparisons(
        mod,
        variables = list(vs = 0:1),
        comparison = function(hi, lo) mean(hi - lo)
    )
    cmp2 <- avg_comparisons(
        mod,
        variables = list(vs = 0:1)
    )
    expect_equal(cmp1$estimate, cmp2$estimate, ignore_attr = TRUE)
    expect_equal(cmp1$std.error, cmp2$std.error, tolerance = tol, ignore_attr = TRUE)
})

test_that("transform_pre: error handling", {
    skip_on_cran()
    skip_if_not_installed("survey")

    withr_library("survey")

    # error when function breaks or returns a bad vector
    data(nhanes, package = "survey")
    dat <- setNames(nhanes, tolower(names(nhanes)))
    dat$female <- dat$riagendr == 2
    dat$race <- sprintf("race%s", dat$race)
    mod <- glm(hi_chol ~ female, data = dat, family = binomial)
    expect_error(comparisons(mod, comparison = function(x) rep(1, 1234)), pattern = "numeric vector")
    expect_error(comparisons(mod, comparison = function(hi, lo) head(hi - lo)), pattern = "numeric vector")
})

test_that("transform_pre: univariate vs Stata", {
    skip_on_cran()

    tol <- .0001

    # univariate vs. Stata
    # known stata results
    arr_s <- c(arr.est = 0.94026450, arr.std_err = 0.09584693, arr.ci_l = 0.76998425, arr.ci_h = 1.14820184)
    ard_s <- c(ard.est = -0.00996557, ard.std_err = 0.01647135, ard.ci_l = -0.04224882, ard.ci_h = 0.02231767)

    acs12 <- get_dataset("acs12", "openintro")
    acs12$disability <- as.numeric(acs12$disability == "yes")
    mod <- glm(disability ~ gender, data = acs12, family = binomial)

    ard_r <- avg_comparisons(mod, comparison = function(hi, lo) lo - hi)
    arr_r <- avg_comparisons(mod, comparison = function(hi, lo) mean(lo) / mean(hi))

    cols <- c("estimate", "std.error", "conf.low", "conf.high")
    ard_r <- unlist(ard_r[, cols])
    arr_r <- unlist(arr_r[, cols])

    expect_equal(arr_r[1:2], arr_s[1:2], tolerance = tol, ignore_attr = TRUE)
    expect_equal(ard_r[1:2], ard_s[1:2], tolerance = tol, ignore_attr = TRUE)
})

test_that("transform_pre: multivariate vs Stata", {
    skip_on_cran()

    tol <- .0001

    # multivariate vs. Stata
    # known stata values
    arr_s <- c(arr.est = 0.80285689, arr.std_err = 0.07496766, arr.ci_l = 0.66858441, arr.ci_h = 0.96409545)
    ard_s <- c(ard.est = -0.03544519, ard.std_err = 0.01499735, ard.ci_l = -0.06483945, ard.ci_h = -0.00605093)

    acs12 <- get_dataset("acs12", "openintro")
    acs12$disability <- as.numeric(acs12$disability == "yes")
    mod <- glm(disability ~ gender + race + married + age, data = acs12, family = binomial)

    ard_r <- avg_comparisons(mod, variables = "gender", comparison = function(hi, lo) lo - hi)
    arr_r <- avg_comparisons(mod, variables = "gender", comparison = function(hi, lo) mean(lo) / mean(hi))

    cols <- c("estimate", "std.error", "conf.low", "conf.high")
    ard_r <- unlist(ard_r[, cols])
    arr_r <- unlist(arr_r[, cols])

    expect_equal(arr_r[1:2], arr_s[1:2], tolerance = tol, ignore_attr = TRUE)
    expect_equal(arr_r[1:2], arr_s[1:2], tolerance = tol, ignore_attr = TRUE)
})

test_that("transform_pre: health insurance vs Stata", {
    skip_on_cran()

    tol <- .0001

    # health insurance vs. Stata
    # known stata results
    arr_s <- c(arr.est = 1.04786879, arr.std_err = 0.00976999, arr.ci_l = 1.02889386, arr.ci_h = 1.06719366)
    ard_s <- c(ard.est = 0.04277614, ard.std_err = 0.00837836, ard.ci_l = 0.02635485, ard.ci_h = 0.05919742)

    dat <- get_dataset("HealthInsurance", "AER")
    dat$health <- as.factor(dat$health)
    mod <- glm(health ~ insurance + gender + ethnicity + married + age, data = dat, family = binomial)

    ard_r <- avg_comparisons(
        mod,
        variables = "insurance",
        comparison = function(hi, lo) hi - lo
    )

    arr_r <- avg_comparisons(
        mod,
        variables = "insurance",
        comparison = function(hi, lo) mean(hi) / mean(lo)
    )

    cols <- c("estimate", "std.error", "conf.low", "conf.high")
    ard_r <- unlist(ard_r[, cols])
    arr_r <- unlist(arr_r[, cols])

    expect_equal(ard_r[1:2], ard_s[1:2], tolerance = tol, ignore_attr = TRUE)
    expect_equal(arr_r[1:2], arr_s[1:2], tolerance = tol, ignore_attr = TRUE)

    # Using manual back-transformation
    cols <- c("estimate", "conf.low", "conf.high")
    arr_r <- avg_comparisons(
        mod,
        variables = "insurance",
        comparison = function(hi, lo) log(mean(hi) / mean(lo)),
        transform = exp
    )
    arr_r <- unlist(arr_r[, cols])
    expect_equal(arr_r, arr_s[c(1, 3, 4)], tolerance = tol, ignore_attr = TRUE)
})

test_that("transform_pre: bug fixes", {
    skip_on_cran()

    # bugfix: multiple terms w/ n=1 transform
    # the function must be applied to each group if it takes a mean or something similar
    dat <- get_dataset("TitanicSurvival", "carData")
    dat$survived <- as.factor(dat$survived)
    mod <- glm(survived ~ passengerClass + sex, data = dat, family = binomial)
    cmp <- avg_comparisons(mod, comparison = function(hi, lo) mean(hi - lo))
    # bug created duplicate estimates
    expect_equal(length(unique(cmp$estimate)), nrow(cmp), ignore_attr = TRUE)
})

test_that("transform_pre: comparison slope vs slopes", {
    skip_on_cran()

    # TODO: fix eps to make sure slopes() and comparisons() give same result
    # comparison slope vs slopes()
    mod <- glm(vs ~ mpg + hp, data = mtcars, family = binomial)
    mfx1 <- slopes(mod)
    mfx2 <- comparisons(mod, comparison = "dydx")
    mfx3 <- slopes(mod, eps = 1e-5)
    mfx4 <- comparisons(mod, comparison = "dydx", eps = 1e-5)
    expect_equal(mfx1$estimate, mfx2$estimate, ignore_attr = TRUE)
    expect_equal(mfx1$std.error, mfx2$std.error, ignore_attr = TRUE)
    expect_equal(mfx3$estimate, mfx4$estimate, ignore_attr = TRUE)
    expect_equal(mfx3$std.error, mfx4$std.error, ignore_attr = TRUE)
})
