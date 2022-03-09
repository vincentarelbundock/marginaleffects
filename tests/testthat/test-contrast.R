test_that("simple contrasts: no validity check", {
    dat <- mtcars
    dat$am <- as.logical(dat$am)
    mod <- lm(mpg ~ hp + am + factor(cyl), data = dat)
    res <- tidy(marginaleffects(mod))
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(4, 9))
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
})


test_that("bug be dead: all levels appear", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    mod <- lm(mpg ~ am + factor(cyl), tmp)
    mfx = marginaleffects(mod, newdata = datagrid(cyl = c(4, 6)))
    expect_equal(nrow(mfx), 6)
})


test_that("numeric contrasts", {
    mod <- lm(mpg ~ hp, data = mtcars)
    expect_error(comparisons(mod, contrast_numeric = "bad", variables = "hp"), regexp = "Assertion failed")
    contr1 <- comparisons(mod, contrast_numeric = 1, variables = "hp")
    contr2 <- comparisons(mod, contrast_numeric = "iqr", variables = "hp")
    contr3 <- comparisons(mod, contrast_numeric = "minmax", variables = "hp")
    contr4 <- comparisons(mod, contrast_numeric = "sd", variables = "hp")
    contr5 <- comparisons(mod, contrast_numeric = "2sd", variables = "hp")
    iqr <- diff(quantile(mtcars$hp, probs = c(.25, .75))) * coef(mod)["hp"]
    minmax <- (max(mtcars$hp) - min(mtcars$hp)) * coef(mod)["hp"]
    sd1 <- sd(mtcars$hp) * coef(mod)["hp"]
    sd2 <- 2 * sd(mtcars$hp) * coef(mod)["hp"]
    expect_equal(contr2$estimate, rep(iqr, 32), ignore_attr = TRUE)
    expect_equal(contr3$estimate, rep(minmax, 32), ignore_attr = TRUE)
    expect_equal(contr4$estimate, rep(sd1, 32), ignore_attr = TRUE)
    expect_equal(contr5$estimate, rep(sd2, 32), ignore_attr = TRUE)
})


test_that("factor", {
              skip("WIP: comparisons")

mod <- lm(mpg ~ factor(cyl), data = mtcars)
comparisons(mod, contrast_factor = "reference") |> tidy()
comparisons(mod, contrast_factor = "pairwise") |> tidy()
comparisons(mod, contrast_factor = "revpairwise") |> tidy()
comparisons(mod, contrast_factor = "sequential") |> tidy()
comparisons(mod, contrast_factor = "revsequential") |> tidy()

})

