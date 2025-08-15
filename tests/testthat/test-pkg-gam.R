test_that("gam package works", {
    skip_if_not_installed("gam")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("gam")
    withr_library("emmeans")
    withr_library("broom")

    # gam: marginaleffects vs. emtrends
    data(kyphosis, package = "gam")
    model <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
    slo <- slopes(model)
    expect_s3_class(slo, "slopes")

    # emmeans
    mfx <- slopes(model, newdata = datagrid(Age = 60, Number = 4), variables = "Number", type = "link")
    em <- emtrends(model, ~Number, "Number", at = list(Age = 60, Number = 4))
    em <- tidy(em)
    expect_equal(mfx$estimate, em$Number.trend, ignore_attr = TRUE)
    # low tolerance only for CRAN Atlas test
    expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)

    # gam: predictions: no validity
    data(kyphosis, package = "gam")
    model <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number, family = binomial, data = kyphosis)
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(kyphosis))
    expect_s3_class(pred1, "predictions")
    expect_false("std.error" %in% colnames(pred1))
    expect_s3_class(pred2, "predictions")
    expect_equal(nrow(pred2), 6, ignore_attr = TRUE)
    expect_false("std.error" %in% colnames(pred2))

    # gam: marginalmeans vs. emmeans
    # TODO: not clear what happens to smooth
    data(kyphosis, package = "gam")
    tmp <- kyphosis
    tmp$categ <- as.factor(sample(letters[1:5], nrow(tmp), replace = TRUE))
    model <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number + categ, family = binomial, data = tmp)

    # `datagrid()` is smarter than `emmeans()` about integers
    atlist <- list(Age = round(mean(tmp$Age)), Number = round(mean(tmp$Number)))
    mm1 <- predictions(model, by = "categ", newdata = datagrid(grid_type = "balanced"), numderiv = "richardson") |>
        dplyr::arrange(categ)
    em1 <- data.frame(emmeans(model, specs = "categ", type = "response", at = atlist))

    mm1 <- predictions(
        model,
        type = "invlink(link)",
        newdata = datagrid(
            grid_type = "balanced",
            Age = round(mean(tmp$Age)),
            Number = round(mean(tmp$Number))
        ),
        by = "categ"
    )
    mm2 <- predictions(
        model,
        type = "link",
        by = "categ",
        newdata = datagrid(grid_type = "balanced"),
        numderiv = "richardson"
    ) |>
        dplyr::arrange(categ)
    em2 <- data.frame(emmeans(model, specs = "categ", at = atlist))

    expect_equal(mm1$estimate, em1$prob, ignore_attr = TRUE)
    expect_equal(mm2$estimate, em2$emmean, ignore_attr = TRUE)
    expect_equal(mm1$conf.low, em1$asymp.LCL, tolerance = 1e-6, ignore_attr = TRUE)
    expect_equal(mm1$conf.high, em1$asymp.UCL, tolerance = 1e-6, ignore_attr = TRUE)
    expect_equal(mm2$conf.low, em2$asymp.LCL, tolerance = 1e-6, ignore_attr = TRUE)
    expect_equal(mm2$conf.high, em2$asymp.UCL, tolerance = 1e-4, ignore_attr = TRUE)
})
