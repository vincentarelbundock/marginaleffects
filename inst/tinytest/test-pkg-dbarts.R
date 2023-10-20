source("helpers.R")
requiet("dbarts")
requiet("modeldata")
requiet("marginaleffects")

dat <- na.omit(modeldata::penguins)

# matrix interface not supported
y <- as.vector(dat$bill_length_mm)
X <- model.matrix(~ ., dat[, -1])
mod <- bart(
    X, y,
    verbose = FALSE) |> suppressWarnings()
expect_error(comparisons(mod, newdata = dat), "bart2") |> suppressWarnings()


# formula interface supported
mod <- bart2(
    bill_length_mm ~ .,
    data = dat,
    keepTrees = TRUE,
    verbose = FALSE)

p <- predictions(mod, by = "species", newdata = dat)
expect_inherits(p, "predictions")
p <- avg_comparisons(mod, newdata = dat)
expect_inherits(p, "comparisons")
