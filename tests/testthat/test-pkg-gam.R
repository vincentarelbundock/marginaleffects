skip_if_not_installed("gam")

requiet("gam")

test_that("gam: marginaleffects: no validity", {
  data(kyphosis, package = "gam")
  model <- gam::gam(Kyphosis ~ s(Age,4) + Number,
                    family = binomial, data = kyphosis)
  mfx <- marginaleffects(model)
  expect_s3_class(mfx, "data.frame")
  expect_false(any(mfx$std.error == 0))
})

test_that("gam: predictions: no validity", {
  data(kyphosis, package = "gam")
  model <- gam::gam(Kyphosis ~ s(Age,4) + Number,
                    family = binomial, data = kyphosis)
  pred1 <- predictions(model)
  pred2 <- predictions(model, newdata = head(kyphosis))
  expect_predictions(pred1, se = TRUE)
  expect_predictions(pred2, n_row = 6, se = TRUE)
})

test_that("gam: marginalmeans: no validity", {
  data(kyphosis, package = "gam")
  tmp <- kyphosis
  tmp$categ <- as.factor(sample(letters[1:5], nrow(tmp), replace = TRUE))
  model <- gam::gam(Kyphosis ~ s(Age,4) + Number + categ,
                    family = binomial, data = tmp)
  mm <- marginalmeans(model)
  expect_marginalmeans(mm, se = FALSE)
})
