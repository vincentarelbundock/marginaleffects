skip_if_not_installed("lme4")

requiet("margins")
requiet("haven")


test_that("glmer vs. stata", {
    dat <- haven::read_dta(test_path("stata/databases/lme4_02.dta"))
    mod <- lme4::glmer(y ~ x1 * x2 + (1 | clus), data = dat, family = binomial)
    stata <- readRDS(test_path("stata/stata.rds"))$lme4_glmer
    mfx <- merge(tidy(marginaleffects(mod)), stata)
    expect_mfx(mod)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .01)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .01)
})


test_that("lmer vs. stata", {
    dat <- haven::read_dta(test_path("stata/databases/lme4_01.dta"))
    mod <- lme4::lmer(y ~ x1 * x2 + (1 | clus), data = dat)
    stata <- readRDS(test_path("stata/stata.rds"))$lme4_lmer
    mfx <- merge(tidy(marginaleffects(mod)), stata)
    expect_mfx(mod)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
})


test_that("vs. margins (dydx only)", {
    dat <- haven::read_dta(test_path("stata/databases/lme4_02.dta"))
    mod <- lme4::glmer(y ~ x1 * x2 + (1 | clus), data = dat, family = binomial)
    res <- marginaleffects(mod, vcov = FALSE)
    mar <- margins::margins(mod)
    expect_true(test_against_margins(res, mar, tolerance = 1e-3))

    dat <- haven::read_dta(test_path("stata/databases/lme4_01.dta"))
    mod <- lme4::lmer(y ~ x1 * x2 + (1 | clus), data = dat)
    res <- marginaleffects(mod, vcov = FALSE)
    mar <- margins::margins(mod)
    expect_true(test_against_margins(res, mar))
})


test_that("sanity check on dpoMatrix", {
    dat <- haven::read_dta(test_path("stata/databases/lme4_02.dta"))
    mod <- lme4::glmer(y ~ x1 * x2 + (1 | clus), data = dat, family = binomial)
    k <- marginaleffects(mod, vcov = stats::vcov(mod))
    expect_s3_class(k, "data.frame")
})


test_that("bug stay dead: tidy without std.error", {
    dat <- haven::read_dta(test_path("stata/databases/lme4_02.dta"))
    mod <- lme4::glmer(y ~ x1 * x2 + (1 | clus), data = dat, family = binomial)
    res <- marginaleffects(mod, vcov = FALSE)
    tid <- tidy(res)
    expect_s3_class(tid, "data.frame")
    expect_equal(nrow(tid), 2)
    expect_error(summary(res), NA)
})


# test_that("'group' cannot be a column name because of conflict with tidy output", {
#     set.seed(1024)
#     N <- 1000
#     tmp <- data.frame(x1 = rnorm(N),
#                       x2 = rnorm(N),
#                       y = sample(0:1, N, replace = TRUE),
#                       group = sample(letters[1:10], N, replace = TRUE))
#     mod <- lme4::glmer(y ~ x1 + x2 + (1 | group), data = tmp, family = binomial)
#     expect_error(marginaleffects(mod), regexp = "more descriptive")
# })
