test_that("simple contrasts: no validity check", {
    dat <- mtcars
    dat$am <- as.logical(dat$am)
    mod <- lm(mpg ~ hp + am + factor(cyl), data = dat)
    res <- tidy(marginaleffects(mod))
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(5, 9))
})

test_that("contrast as difference and CI make sense", {
    # problem reported with suggested fix by E.Book in Issue 58
    dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
    dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
    mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, 
               data = dat, family = binomial)
    mfx <- marginaleffects(mod)
    ti <- tidy(mfx)
    reject_ci <- ti$conf.high < 0 | ti$conf.low > 0
    reject_p <- ti$p.value < 0.05
    expect_equal(reject_ci, reject_p)
    expect_true("Gentoo - Chinstrap" %in% ti$contrast)
})
