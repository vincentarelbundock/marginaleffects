source("helpers.R")
using("marginaleffects")

if (ON_WINDOWS) exit_file("on windows")
if (!minver("base", "4.1.0")) exit_file("R 4.1.0")
if (!requiet("mvgam")) exit_file("mvgam not installed")

# mvgam prediction arguments raise no warning
mod_class <- structure(list(), class = "mvgam")
expect_silent(marginaleffects:::sanity_dots(mod_class, process_error = TRUE, ndraws = 10))


# load a pre-compiled model (modelarchive/fit_models.Rmd)
mod1 <- readRDS(testing_path("modelarchive/data/mvgam_ar.rds"))
dat <- insight::get_data(mod1)

# slopes() and tidy()
mfx <- slopes(mod1)
ti <- tidy(mfx)
expect_inherits(ti, "data.frame")
expect_equal(nrow(ti), 2 * nrow(dat))
expect_true(ncol(ti) >= 10)
expect_true(all(c("term", "estimate", "conf.low") %in% colnames(ti)))

# get_predict() with original data
w <- apply(posterior_linpred(mod1, process_error = FALSE), 2, stats::median)
x <- get_predict(mod1, newdata = dat, type = "link", process_error = FALSE)
expect_equal(NROW(x), NROW(dat))
expect_equivalent(w, x$estimate)

# get_predict() with newdata
newdat <- dat
newdat$x <- 0
w <- apply(
    posterior_linpred(mod1, newdata = newdat, process_error = FALSE),
    2,
    stats::median
)
x <- get_predict(mod1, type = "link", newdata = newdat, process_error = FALSE)
expect_equivalent(w, x$estimate)
expect_equal(NROW(x), NROW(newdat))

# expectations vs response predictions()
p1 <- predictions(mod1, type = "expected")
p2 <- predictions(mod1, type = "response")
expected_uncertainty <- p1$conf.high - p1$conf.low
response_uncertainty <- p2$conf.high - p2$conf.low
expect_true(all(expected_uncertainty < response_uncertainty))

# avg_predictions()
ems <- avg_predictions(mod1, type = "expected")
expect_equal(NROW(ems), 1)
expect_true(all(c("estimate", "conf.low", "conf.high") %in% colnames(ems)))

ems <- avg_predictions(mod1, type = "expected", variables = list(x = c(-1, 0, 1)))
expect_equal(NROW(ems), 3)
expect_true(all(c("x", "estimate", "conf.low", "conf.high") %in% colnames(ems)))

# process_error = TRUE reaches the predictions
s1 <- avg_slopes(mod1, variables = "x", type = "expected")
s2 <- avg_slopes(mod1, variables = "x", type = "expected", process_error = TRUE)
expect_true(s2$conf.high - s2$conf.low > s1$conf.high - s1$conf.low)

# latent_N is an allowed type, but only for N-mixture and occupancy models
expect_error(predictions(mod1, type = "latent_N"), "has no closure units", fixed = TRUE)
