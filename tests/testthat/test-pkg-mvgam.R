testthat::skip_if(!EXPENSIVE, "EXPENSIVE")
testthat::skip_if(ON_WINDOWS, "on windows")
testthat::skip_if(getRversion() < "4.1.0", "R 4.1.0")
testthat::skip_if_not_installed("mvgam")
requiet("mvgam")


# load a pre-compiled model
mod1 <- mvgam:::mvgam_example1

# slopes() and tidy()
mfx <- slopes(mod1)
ti <- tidy(mfx)
expect_s3_class(ti, "data.frame")
expect_true(nrow(ti) == 66)
expect_true(ncol(ti) >= 10)
expect_true(all(c("term", "estimate", "conf.low") %in% colnames(ti)))

# get_predict() with original data
preds <- get_predict(mod1,
    newdata = components(mfx, "modeldata"))
expect_equal(NROW(preds), NROW(mod1$obs_data))

w <- apply(posterior_linpred(mod1, process_error = FALSE), 2, stats::median)
x <- get_predict(mod1,
    newdata = components(mfx, "modeldata"),
    type = "link", process_error = FALSE)
expect_equal(w, x$estimate, ignore_attr = TRUE)

# get_predict() with newdata
newdat <- mod1$obs_data
newdat$season <- rep(1, nrow(newdat))
w <- apply(
    posterior_linpred(mod1, newdata = newdat, process_error = FALSE),
    2,
    stats::median
)
x <- get_predict(mod1, type = "link", newdata = newdat, process_error = FALSE)
expect_equal(w, x$estimate, ignore_attr = TRUE)
expect_equal(NROW(x), NROW(newdat))

# expectations vs response predictions()
p1 <- suppressWarnings(predictions(mod1, type = "expected"))
p2 <- suppressWarnings(predictions(mod1, type = "response"))
expected_uncertainty <- p1$conf.high - p1$conf.low
response_uncertainty <- p2$conf.high - p2$conf.low
expect_true(all(expected_uncertainty < response_uncertainty))

# avg_predictions()
ems <- avg_predictions(mod1)
expect_equal(NROW(ems), 1)
expect_true(all(c("estimate", "conf.low", "conf.high") %in% colnames(ems)))

ems <- avg_predictions(mod1, variables = list(season = c(1, 6, 12)))
expect_equal(NROW(ems), 3)
expect_true(all(c("season", "estimate", "conf.low", "conf.high") %in% colnames(ems)))

# latent_N should be an allowed type, but shouldn't work for this model
expect_error(predictions(mod1, type = "latent_N"), '"latent_N" type only available for N-mixture models', fixed = TRUE)
