source("helpers.R")

if (!AUTODIFF) exit_file("autodiff")

# Tolerance levels for numerical comparison between autodiff and finite differences
# Small differences are expected due to different numerical methods
tol_b <- 1e-5  # tolerance for estimates (betas/predictions)
tol_se <- 1e-4 # tolerance for standard errors

penguins <- get_dataset("penguins", "palmerpenguins") |> na.omit()

dat <- transform(penguins, big = bill_length_mm > median(bill_length_mm))
mod <- glm(big ~ bill_depth_mm + body_mass_g + flipper_length_mm + island,
  family = binomial, data = dat)

# predictions()
autodiff(FALSE)
pre1 <- predictions(mod, type = "response")
autodiff(TRUE)
expect_message(pre2 <- predictions(mod, type = "response"))
expect_equal(pre1$estimate, pre2$estimate, tol = tol_b)
expect_equal(pre1$std.error, pre2$std.error, tol = tol_se)

# avg_predictions()
autodiff(FALSE)
pre1 <- avg_predictions(mod, type = "response")
autodiff(TRUE)
expect_message(pre2 <- avg_predictions(mod, type = "response"))
expect_equal(pre1$estimate, pre2$estimate, tol = tol_b)
expect_equal(pre1$std.error, pre2$std.error, tol = tol_se)

# comparisons()
autodiff(FALSE)
pre1 <- predictions(mod)
autodiff(TRUE)
expect_message(pre2 <- predictions(mod))
expect_equal(pre1$estimate, pre2$estimate, tol = tol_b)
expect_equal(pre1$std.error, pre2$std.error, tol = tol_se)

# avg_comparisons()
autodiff(FALSE)
cmp1 <- avg_comparisons(mod)
autodiff(TRUE)
expect_message(cmp2 <- avg_comparisons(mod))
expect_equal(cmp1$estimate, cmp2$estimate, tol = tol_b)
expect_equal(cmp1$std.error, cmp2$std.error, tol = tol_se)

# avg_comparisons(by=)
mod <- lm(Sepal.Length ~ Sepal.Width * Petal.Length * Petal.Width * Species, iris)
autodiff(FALSE)
cmp1 <- avg_comparisons(mod, by = "Species")
autodiff(TRUE)
expect_message(cmp2 <- avg_comparisons(mod, by = "Species"))
expect_equal(cmp1$estimate, cmp2$estimate, tol = tol_b)
expect_equal(cmp1$std.error, cmp2$std.error, tol = tol_se)

# invlink
mod <- glm(vs ~ mpg + wt, data = mtcars, family = binomial(link = "logit"))
autodiff(TRUE)
expect_message(p1 <- predictions(mod, type = "invlink(link)"))
autodiff(FALSE)
p2 <- predictions(mod, type = "invlink(link)")
expect_equal(p1$estimate, p2$estimate, tol = tol_b)
expect_equal(p1$std.error, p2$std.error, tol = tol_se)


# GLM comparisons with type="response" for all family/link combinations
# Create test datasets for different response types
set.seed(123)
n <- 200
x1 <- rnorm(n, 0, 0.5) # Smaller variance for stability
x2 <- runif(n, -1, 1) # Centered around 0
binary_y <- rbinom(n, 1, plogis(0.5 * x1 + 0.3 * x2))
count_y <- rpois(n, exp(1 + 0.2 * x1 + 0.15 * x2)) # Smaller coefficients
continuous_y <- rnorm(n, 2 + 0.5 * x1 + 0.3 * x2, 1)
gamma_y <- rgamma(n, shape = 2, rate = 1 / exp(1 + 0.2 * x1 + 0.1 * x2)) # More stable parameterization
test_data <- data.frame(
  x1 = x1,
  x2 = x2,
  binary_y = binary_y,
  count_y = pmax(count_y, 1), # Ensure positive counts
  continuous_y = continuous_y,
  gamma_y = pmax(gamma_y, 0.1) # Ensure positive gamma values
)


# Define family/link combinations to test (cauchit excluded - not supported by Python marginaleffects)
family_link_combos <- list(
  list(family = binomial(link = "logit"), response = "binary_y"),
  list(family = binomial(link = "probit"), response = "binary_y"),
  list(family = binomial(link = "cloglog"), response = "binary_y"),
  list(family = gaussian(link = "identity"), response = "continuous_y"),
  # list(family = gaussian(link = "log"), response = "continuous_y"),
  list(family = Gamma(link = "inverse"), response = "gamma_y"),
  list(family = Gamma(link = "log"), response = "gamma_y"),
  list(family = Gamma(link = "identity"), response = "gamma_y"),
  list(family = poisson(link = "log"), response = "count_y"),
  list(family = poisson(link = "identity"), response = "count_y"),
  list(family = poisson(link = "sqrt"), response = "count_y"),
  list(family = quasibinomial(link = "logit"), response = "binary_y"),
  list(family = quasipoisson(link = "log"), response = "count_y")
)

#  Test comparisons() with type="response" for each family/link combination
for (combo in family_link_combos) {
  # Fit model
  formula_str <- paste(combo$response, "~ x1 + x2")
  mod <- glm(as.formula(formula_str), data = test_data, family = combo$family)

  # Test comparisons() with autodiff FALSE vs TRUE
  autodiff(FALSE)
  cmp1 <- comparisons(mod, type = "response")
  autodiff(TRUE)
  expect_message(cmp2 <- comparisons(mod, type = "response"))

  # Compare estimates and standard errors
  expect_equal(cmp1$estimate, cmp2$estimate,
    tolerance = tol_b,
    info = paste("estimates differ for", combo$family$family, combo$family$link))
  expect_equal(cmp1$std.error, cmp2$std.error,
    tolerance = tol_se,
    info = paste("std.errors differ for", combo$family$family, combo$family$link))

  # Test avg_comparisons() with type="response"
  autodiff(FALSE)
  acmp1 <- avg_comparisons(mod, type = "response")
  autodiff(TRUE)
  expect_message(acmp2 <- avg_comparisons(mod, type = "response"))

  # Compare estimates and standard errors
  expect_equal(acmp1$estimate, acmp2$estimate,
    tolerance = tol_b,
    info = paste("avg estimates differ for", combo$family$family, combo$family$link))
  expect_equal(acmp1$std.error, acmp2$std.error,
    tolerance = tol_se,
    info = paste("avg std.errors differ for", combo$family$family, combo$family$link))
}





















# ########## benchmarking ##########
# library(microbenchmark)
# pkgload::load_all()
#
# # Download data and fit a large model
# dat <- get_dataset("airbnb")
# mod <- glm(TV ~ ., data = dat, family = binomial)
#
# # Average Predictions
# finite <- function() {
#   autodiff(FALSE)
#   predictions(mod, type = "response")
# }
#
# auto <- function() {
#   autodiff(TRUE)
#   predictions(mod, type = "response")
# }
#
# microbenchmark(finite(), auto(), times = 5)
#
# # Average Treatment Effect
# finite <- function() {
#   autodiff(FALSE)
#   avg_comparisons(mod, variables = "Heating")
# }
#
# auto <- function() {
#   autodiff(TRUE)
#   avg_comparisons(mod, variables = "Heating")
# }
#
# microbenchmark(finite(), auto(), times = 5)
