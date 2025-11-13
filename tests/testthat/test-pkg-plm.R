testthat::skip_if_not_installed("margins")
testthat::skip_if_not_installed("broom")
testthat::skip_if_not_installed("plm")
requiet("margins")
requiet("broom")
requiet("plm")

tol <- .001
tol_se <- .01 # BDR emergency email about tiny numerical differences

# Basic expectation tests
data("Produc", package = "plm")
mod_simple <- plm::plm(log(gsp) ~ log(pcap) + log(pc), data = Produc, index = c("state", "year"))
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

dat_plm <- get_dataset("Grunfeld", "plm")
dat_plm$rownames <- NULL
dat_plm <<- pdata.frame(dat_plm)
pool <- plm(inv ~ value * capital, data = dat_plm, model = "pooling")
swamy <- plm(
    inv ~ value * capital,
    data = dat_plm,
    model = "random",
    variables = "individual"
)
amemiya <- plm(
    inv ~ value * capital,
    data = dat_plm,
    model = "random",
    random.method = "amemiya",
    variables = "twoways"
)
walhus <- plm(
    inv ~ value * capital,
    data = dat_plm,
    model = "random",
    random.method = "walhus",
    variables = "twoways"
)

### marginaleffects

# pooling vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))$plm_pooling
mfx <- merge(avg_slopes(pool), stata)
expect_slopes2(pool, n_unique = 1)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = tol, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = tol_se, ignore_attr = TRUE)


# Swamy-Arora vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))$plm_sa
mfx <- merge(avg_slopes(swamy), stata)
expect_slopes2(swamy)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = tol, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = tol_se, ignore_attr = TRUE)

# margins
mfx <- avg_slopes(swamy)
mar <- tidy(margins(swamy))
mfx <- mfx[order(mfx$term), ]
expect_equal(mfx$estimate, mar$estimate, tolerance = tol, ignore_attr = TRUE)
expect_equal(mfx$std.error, mar$std.error, tolerance = tol_se, ignore_attr = TRUE)


# no validity checks
expect_slopes2(amemiya)
# margins
avg_slopes(amemiya, type = "link")
avg_slopes(amemiya, type = "response")
mfx <- avg_slopes(amemiya)
mar <- tidy(margins(amemiya))
mfx <- mfx[order(mfx$term), ]
expect_equal(mfx$estimate, mar$estimate, tolerance = tol, ignore_attr = TRUE)
expect_equal(mfx$std.error, mar$std.error, tolerance = tol_se, ignore_attr = TRUE)

expect_slopes2(walhus)

# margins
mfx <- avg_slopes(walhus)
mar <- tidy(margins(walhus))
mfx <- mfx[order(mfx$term), ]
expect_equal(mfx$estimate, mar$estimate, tolerance = tol, ignore_attr = TRUE)
expect_equal(mfx$std.error, mar$std.error, tolerance = tol_se, ignore_attr = TRUE)


# # commented out because the dev version of {plm} now has a fully-working predict method
# # within error
# # within model are not supported by `predict.plm`
# stata <- readRDS(testing_path("stata/stata.rds"))$plm_within
# mod <- plm(inv ~ value * capital, data = dat_plm, model = "within", variables = "twoways")
# expect_error(slopes(mod), regexp = "Unable")

### predictions

# predictions: pooling no validity
pred1 <- predictions(pool)
pred2 <- predictions(pool, newdata = head(dat_plm))
expect_predictions2(pool, n_row = nrow(dat_plm))
expect_predictions2(pool, newdata = head(dat_plm), n_row = 6)
