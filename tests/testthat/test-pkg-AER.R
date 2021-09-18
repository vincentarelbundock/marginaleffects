skip_if_not_installed("AER")
requiet("AER")

test_that("tobit: no validity check", {
  data("Affairs", package = "AER")

  stata <- readRDS(test_path("stata/stata.rds"))$aer_tobit
  mod1 <- AER::tobit(
    affairs ~ age + yearsmarried + religiousness + occupation + rating, 
    data = Affairs)
  mfx <- merge(tidy(marginaleffects(mod1)), stata)
  expect_mfx(mod1)
  expect_equal(mfx$estimate, mfx$dydxstata)
  expect_equal(mfx$std.error, mfx$std.errorstata)
  
  stata <- readRDS(test_path("stata/stata.rds"))$aer_tobit_right
  mod2 <- AER::tobit(
    affairs ~ age + yearsmarried + religiousness + occupation + rating, 
    right = 4, data = Affairs)
  mfx <- merge(tidy(marginaleffects(mod2)), stata)
  expect_mfx(mod2)
  expect_equal(mfx$estimate, mfx$dydxstata)
  expect_equal(mfx$std.error, mfx$std.errorstata)
})
