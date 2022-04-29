requiet("sampleSelection")

data("Mroz87", package = "sampleSelection")
Mroz87$kids  <- (Mroz87$kids5 + Mroz87$kids618 > 0)
dat <<- Mroz87

test_that("heckit: se not supported yet", {
    mod <- heckit(lfp ~ age + I( age^2 ) + faminc + kids + educ,
                  wage ~ exper + I( exper^2 ) + educ + city, 
                  data = dat)
    mfx <- marginaleffects(mod)
    expect_s3_class(mfx, "marginaleffects")
    mfx <- marginaleffects(mod, part = "selection", type = "link")
    expect_s3_class(mfx, "marginaleffects")
    mfx <- marginaleffects(mod, part = "outcome", type = "unconditional")
    expect_s3_class(mfx, "marginaleffects")
    expect_true(all(is.na(mfx$std.error)))
})

test_that("selection: no validity", {
    mod <- selection(lfp ~ age + I( age^2 ) + faminc + kids + educ,
                     wage ~ exper + I( exper^2 ) + educ + city, 
                     data = dat)
    mfx <- marginaleffects(mod)
    expect_s3_class(mfx, "marginaleffects")
    mfx <- marginaleffects(mod, part = "selection", type = "link")
    expect_s3_class(mfx, "marginaleffects")
    mfx <- marginaleffects(mod, part = "outcome", type = "unconditional")
    expect_s3_class(mfx, "marginaleffects")
})
