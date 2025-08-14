test_that("pruned object is much smaller than original", {
  dat <- get_dataset("airbnb")
  m <- lm(price ~ ., data = dat)
  p <- avg_predictions(m) |> suppressWarnings()
  l <- prune(p)
  ratio <- object.size(l) / object.size(p)
  expect_lt(ratio, 0.001)
})
