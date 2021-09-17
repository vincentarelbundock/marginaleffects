skip_if_not_installed("gam")

requiet("gam")

test_that("gam: no validity check", {
  data(kyphosis, package = "gam")
  model <- gam::gam(Kyphosis ~ s(Age,4) + Number, family = binomial, data=kyphosis)
  mfx <- marginaleffects(model)
  expect_s3_class(mfx, "data.frame")
  expect_false(any(mfx$std.error == 0))
})
