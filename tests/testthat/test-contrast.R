test_that("contrast functionality works", {
    skip_if_not_installed("emmeans")

    withr_library("emmeans")

    # contrast as difference and CI make sense
    # problem reported with suggested fix by E.Book in Issue 58
    dat <- get_dataset("penguins", "palmerpenguins")
    dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
    mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, data = dat, family = binomial)
    ti <- avg_slopes(mod)
    reject_ci <- ti$conf.high < 0 | ti$conf.low > 0
    reject_p <- ti$p.value < 0.05
    expect_equal(reject_ci, reject_p, ignore_attr = TRUE)

    # bug be dead: all levels appear
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    mod <- lm(mpg ~ am + factor(cyl), tmp)
    mfx = slopes(mod, newdata = datagrid(cyl = c(4, 6)))
    expect_equal(nrow(mfx), 6, ignore_attr = TRUE)

    # numeric contrasts
    mod <- lm(mpg ~ hp, data = mtcars)
    contr1 <- comparisons(mod, variables = list("hp" = 1))
    contr2 <- comparisons(mod, variables = list("hp" = "iqr"))
    contr3 <- comparisons(mod, variables = list("hp" = "minmax"))
    contr4 <- comparisons(mod, variables = list("hp" = "sd"))
    contr5 <- comparisons(mod, variables = list("hp" = "2sd"))
    iqr <- diff(stats::quantile(mtcars$hp, probs = c(.25, .75))) * coef(mod)["hp"]
    minmax <- (max(mtcars$hp) - min(mtcars$hp)) * coef(mod)["hp"]
    sd1 <- sd(mtcars$hp) * coef(mod)["hp"]
    sd2 <- 2 * sd(mtcars$hp) * coef(mod)["hp"]
    expect_equal(contr2$estimate, rep(iqr, 32), ignore_attr = TRUE)
    expect_equal(contr3$estimate, rep(minmax, 32), ignore_attr = TRUE)
    expect_equal(contr4$estimate, rep(sd1, 32), ignore_attr = TRUE)
    expect_equal(contr5$estimate, rep(sd2, 32), ignore_attr = TRUE)

    # factor glm
    mod <- glm(am ~ factor(cyl), data = mtcars, family = binomial)
    pred <- predictions(mod, newdata = datagrid(cyl = mtcars$cyl))
    contr <- avg_comparisons(mod)
    expect_equal(contr$estimate[1], pred$estimate[pred$cyl == 6] - pred$estimate[pred$cyl == 4], ignore_attr = TRUE)
    expect_equal(contr$estimate[2], pred$estimate[pred$cyl == 8] - pred$estimate[pred$cyl == 4], ignore_attr = TRUE)

    # emmeans w/ back-transforms is similar to comparisons with direct delta method
    tol <- 1e-4

    dat <- mtcars
    dat$cyl <- as.factor(dat$cyl)
    mod <- glm(am ~ cyl, data = dat, family = binomial)

    # link scale
    cmp <- comparisons(mod, variables = list(cyl = "pairwise"), type = "link", newdata = datagrid())
    emm <- emmeans(mod, specs = "cyl")
    emm <- emmeans::contrast(emm, method = "revpairwise", df = Inf, adjust = NULL)
    emm <- data.frame(confint(emm))
    expect_equal(cmp$estimate, emm$estimate, ignore_attr = TRUE)
    expect_equal(cmp$std.error, emm$SE, ignore_attr = TRUE)
    expect_equal(cmp$conf.low, emm$asymp.LCL, ignore_attr = TRUE)
    expect_equal(cmp$conf.high, emm$asymp.UCL, ignore_attr = TRUE)

    # response scale
    cmp <- comparisons(mod, type = "response", newdata = datagrid(), variables = list(cyl = "pairwise"))
    emm <- emmeans(mod, specs = "cyl")
    emm <- emmeans::contrast(
        regrid(emm),
        method = "revpairwise",
        df = Inf,
        adjust = NULL,
        type = "response",
        ratios = FALSE
    )
    emm <- data.frame(confint(emm))
    expect_equal(cmp$estimate, emm$estimate, tolerance = tol, ignore_attr = TRUE)
    expect_equal(cmp$std.error, emm$SE, tolerance = tol, ignore_attr = TRUE)
    expect_equal(cmp$conf.low, emm$asymp.LCL, tolerance = tol, ignore_attr = TRUE)
    expect_equal(cmp$conf.high, emm$asymp.UCL, tolerance = tol, ignore_attr = TRUE)

    # smart contrast labels
    dat$am <- as.logical(dat$am)
    dat$cyl <- as.factor(dat$cyl)
    dat$gear <- as.character(dat$gear)
    mod <- lm(mpg ~ hp + am + cyl + gear, data = dat)
    cmp1 <- comparisons(
        mod,
        newdata = "mean"
    ) |>
        dplyr::arrange(term)
    expect_equal(
        cmp1$contrast,
        c("TRUE - FALSE", "6 - 4", "8 - 4", "4 - 3", "5 - 3", "+1"),
        ignore_attr = TRUE
    )
})
