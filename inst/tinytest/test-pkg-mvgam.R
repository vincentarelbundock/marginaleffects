source("helpers.R")
using("marginaleffects")

if (!EXPENSIVE) exit_file("EXPENSIVE")
if (ON_WINDOWS) exit_file("on windows")
if (!minver("base", "4.1.0")) exit_file("R 4.1.0")
if (!requiet("mvgam")) exit_file("mvgam not installed")


# load a pre-compiled model
mod1 <- mvgam:::mvgam_example1

# slopes() and tidy()
mfx <- avg_slopes(mod1)
expect_true(nrow(mfx) == 3)
expect_true(ncol(mfx) >= 5)
expect_true(all(c("term", "estimate", "conf.low") %in% colnames(mfx)))

# get_predict() with original data
preds <- get_predict(mod1, newdata = mod1$obs_data)
expect_equal(NROW(preds), NROW(mod1$obs_data))

w <- apply(posterior_linpred(mod1, process_error = FALSE), 2, stats::median)
x <- get_predict(mod1, type = "link", process_error = FALSE, newdata = mod1$obs_data)
expect_equivalent(w, x$estimate)

# get_predict() with newdata
newdat <- mod1$obs_data
newdat$season <- rep(1, nrow(newdat))
w <- apply(
    posterior_linpred(mod1, newdata = newdat, process_error = FALSE),
    2,
    stats::median
)
x <- get_predict(mod1, type = "link", newdata = newdat, process_error = FALSE)
expect_equivalent(w, x$estimate)
expect_equal(NROW(x), NROW(newdat))


# avg_predictions()
ems <- avg_predictions(mod1)
expect_equal(NROW(ems), 1)
expect_true(all(c("estimate", "conf.low", "conf.high") %in% colnames(ems)))

ems <- avg_predictions(mod1, variables = list(season = c(1, 6, 12)))
expect_equal(NROW(ems), 3)
expect_true(all(c("season", "estimate", "conf.low", "conf.high") %in% colnames(ems)))

# latent_N should be an allowed type, but shouldn't work for this model
expect_error(predictions(mod1, type = "latent_N"), '"latent_N" type only available for N-mixture models', fixed = TRUE)


exit_file("works interactively")
# expectations vs response predictions()
p1 <- suppressWarnings(predictions(mod1, type = "expected"))
p2 <- suppressWarnings(predictions(mod1, type = "response"))
expected_uncertainty <- p1$conf.high - p1$conf.low
response_uncertainty <- p2$conf.high - p2$conf.low
expect_true(all(expected_uncertainty < response_uncertainty))
