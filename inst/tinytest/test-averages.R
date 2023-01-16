source("helpers.R")
using("marginaleffects")

mod <<- lm(mpg ~ (qsec + drat) * am, data = mtcars)
mfx1 <- slopes(mod) |> averages(by = "am")
mfx2 <- averages(slopes(mod, by = "am"))
expect_equal(nrow(mfx1), nrow(mfx2))
expect_error(averages(slopes(mod, by = "am"), by = "cyl"), pattern = "twice")
