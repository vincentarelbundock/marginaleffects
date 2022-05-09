requiet("modelsummary")
tol <- .001

test_that("error when function breaks or returns a bad vector", {
    requiet("survey")
    data(nhanes, package = "survey")
    dat <- setNames(nhanes, tolower(names(nhanes)))
    dat$female <- dat$riagendr == 2
    dat$race <- sprintf("race%s", dat$race)
    mod <- glm(hi_chol ~ female, data = dat, family = binomial)
    expect_error(comparisons(mod, transformation = mean),
                 regexp = "numeric vector")
    expect_error(comparisons(mod, transformation = function(hi, lo) head(hi - lo)),
                 regexp = "numeric vector")
})


test_that("univariate vs. Stata", {
    requiet("parameters")
    acs12 <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/openintro/acs12.csv")
    acs12$disability <- as.numeric(acs12$disability == "yes")
    mod <- glm(disability ~ gender, data = acs12, family = binomial)

    cols <- c("estimate", "std.error", "conf.low", "conf.high")
    ard_r <- comparisons(mod, transformation = function(hi, lo) lo - hi)
    arr_r <- comparisons(mod, transformation = function(hi, lo) lo / hi)

    # # Stata probably uses t instead of z
    # arr_r <- tidy(arr_r)
    # critical_t <- qt(0.025, df = 1999)
    # arr_r$estimate - abs(critical_t) * arr_r$std.error
    # arr_r$estimate + abs(critical_t) * arr_r$std.error

    ard_r <- unlist(tidy(ard_r)[, cols])
    arr_r <- unlist(tidy(arr_r)[, cols])

    # known stata output
    arr_s <- c(0.9403, 0.0958, 0.7700, 1.1482)
    ard_s <- c(-0.0100, 0.0165, -0.0422, 0.0223)

    expect_equal(arr_r, arr_s, tolerance = tol, ignore_attr = TRUE)
    expect_equal(ard_r, ard_s, tolerance = tol, ignore_attr = TRUE)
})


test_that("Incomplete: multivariate vs. Stata", {
    acs12 <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/openintro/acs12.csv")
    acs12$disability <- as.numeric(acs12$disability == "yes")
    mod <- glm(disability ~ gender + race + married + age, data = acs12, family = binomial)

    cols <- c("estimate", "std.error", "conf.low", "conf.high")
    ard_r <- comparisons(mod, variables = "gender", transformation = function(hi, lo) lo - hi)
    arr_r <- comparisons(mod, variables = "gender", transformation = function(hi, lo) lo / hi)

    ard_r <- comparisons(
        mod,
        variables = "gender",
        newdata = datagrid(gender = acs12$gender, grid.type = "counterfactual"),
        transformation = function(hi, lo) lo - hi)
    # summary(ard_r)

    nd = datagrid(model = mod, gender = acs12$gender, grid.type = "counterfactual")
    p <- predictions(mod, newdata = nd)
    p <- aggregate(predicted ~ gender, data = p, FUN = mean)
    p$predicted[1] / p$predicted[2]
    p$predicted[1] - p$predicted[2]
})


# x <- rnorm(10)
# y <- rnorm(10)
# mean(x) / mean(y)
# mean(x / y)
