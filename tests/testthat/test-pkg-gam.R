requiet("gam")
requiet("emmeans")
requiet("broom")

test_that("gam: marginaleffects vs. emtrends", {
  data(kyphosis, package = "gam")
  model <- gam::gam(Kyphosis ~ s(Age,4) + Number,
                    family = binomial, data = kyphosis)
  mfx <- marginaleffects(model)
  expect_s3_class(mfx, "data.frame")
  expect_false(any(mfx$std.error == 0))

  # emmeans
  mfx <- marginaleffects(model, newdata = datagrid(Age = 60, Number = 4), variables = "Number", type = "link")
  em <- emtrends(model, ~Number, "Number", at = list(Age = 60, Number = 4))
  em <- tidy(em)
  expect_equal(mfx$dydx, em$Number.trend)
  expect_equal(mfx$std.error, em$std.error)
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

test_that("gam: marginalmeans vs. emmeans", {
  skip("gam: marginalmeans != emmeans")
  data(kyphosis, package = "gam")
  tmp <- kyphosis
  tmp$categ <- as.factor(sample(letters[1:5], nrow(tmp), replace = TRUE))
  model <- gam::gam(Kyphosis ~ s(Age,4) + Number + categ,
                    family = binomial, data = tmp)
  mm <- marginalmeans(model)
  expect_marginalmeans(mm)

  mm <- tidy(mm)
  em <- tidy(emmeans(model, specs = "categ", transform = "response"))
  expect_equal(mm$estimate, em$prob)
  expect_equal(mm$std.error, em$std.error)

  mm <- tidy(marginalmeans(model, type = "link"))
  em <- tidy(emmeans(model, specs = "categ"))
  expect_equal(mm$estimate, em$estimate)
  expect_equal(mm$std.error, em$std.error)
})
