skip_if_not_installed("AER")
requiet("AER")

test_that("tobit: no validity check", {
  data("Affairs", package = "AER")
  mod <- AER::tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating, data = Affairs)
  mfx <- marginaleffects(mod)
  tid <- tidy(mfx)
  expect_s3_class(tid, "data.frame")
  expect_equal(nrow(tid), 5)

  mod <- AER::tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating, right = 4, data = Affairs)
  mfx <- marginaleffects(mod)
  tid <- tidy(mfx)
  expect_s3_class(tid, "data.frame")
  expect_equal(nrow(tid), 5)
})
