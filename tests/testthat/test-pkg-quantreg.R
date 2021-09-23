requiet("quantreg")

test_that("rg() no validity check", {
    mod <- quantreg::rq(mpg ~ hp * wt + factor(cyl), 
                        data = mtcars)
    expect_mfx(mod)
})
