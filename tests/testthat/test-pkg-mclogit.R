test_that("mclogit package works", {
    skip_if_not_installed("mclogit")
    skip_if_not_installed("MASS")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")
    skip_if_not_installed("splines")

    withr_library("mclogit")
    withr_library("MASS")
    withr_library("emmeans")
    withr_library("broom")
    withr_library("splines")

    # mclogit: no validity
    data(Transport, package = "mclogit")
    dat <- Transport
    void <- capture.output(
        model <- mclogit(cbind(resp, suburb) ~ distance + cost, data = Transport)
    )
    # type = "link" works
    slo <- slopes(model, type = "link")
    expect_s3_class(slo, "slopes")
    pred <- predictions(model, type = "link")
    expect_s3_class(pred, "predictions")

    # mblogit: error on character regressors
    dat <- get_dataset("housing", "MASS")
    dat$x <- rnorm(nrow(dat))
    dat$Sat <- factor(dat$Sat)
    void <- capture.output(
        mod <- mblogit(Sat ~ Infl + Type + Cont + x, weights = Freq, data = dat)
    )
    pre <- predictions(mod)
    expect_s3_class(pre, "predictions")

    # mblogit: works on factor regressors
    dat <- get_dataset("housing", "MASS")
    dat$x <- rnorm(nrow(dat))
    dat$Sat <- factor(dat$Sat)
    dat$Infl <- factor(dat$Infl)
    dat$Cont <- factor(dat$Cont)
    dat$Type <- factor(dat$Type)
    void <- capture.output(
        mod <- mblogit(Sat ~ Infl + Type + Cont + x, weights = Freq, data = dat)
    )
    pre <- predictions(mod)
    expect_s3_class(pre, "predictions")
    mfx <- suppressWarnings(slopes(mod, type = "link"))
    expect_s3_class(mfx, "marginaleffects")

    dat <- read.csv(test_path("modelarchive/data/covid_variants.csv"))
    dat <- dat[dat$variant %in% c("Alpha", "Beta", "Other"), ]
    dat$variant <- factor(dat$variant, levels = c("Other", "Beta", "Alpha"))
    void <- capture.output(suppressWarnings(
        mod <- mblogit(
            formula = variant ~ ns(collection_date_num, df = 2),
            weights = count,
            data = dat,
            from.table = FALSE,
            dispersion = FALSE,
            verbose = FALSE,
            control = mclogit.control(maxit = 100)
        )
    ))

    # response
    p1 <- predict(mod, type = "response", se.fit = TRUE)
    p2 <- predictions(mod)
    expect_equal(p1$fit[160, ], p2[p2$rowid == 160, "estimate"], ignore_attr = TRUE)
    expect_equal(p1$se.fit[160, ], p2[p2$rowid == 160, "std.error"], tolerance = .01, ignore_attr = TRUE)
    expect_equal(names(p1$fit[160, ]), as.character(p2[p2$rowid == 160, "group"]), ignore_attr = TRUE)

    # link
    p1 <- predict(mod, type = "link", se.fit = TRUE)
    p2 <- predictions(mod, type = "link")
    expect_equal(p1$fit[160, ], p2[p2$rowid == 160, "estimate"], ignore_attr = TRUE)
    expect_equal(p1$se.fit[160, ], p2[p2$rowid == 160, "std.error"], tolerance = .001, ignore_attr = TRUE)
    expect_equal(names(p1$fit[160, ]), as.character(p2[p2$rowid == 160, "group"]), ignore_attr = TRUE)

    # latent
    p2 <- predictions(mod, type = "latent") |> dplyr::arrange(group, rowid)
    p3 <- data.frame(
        emmeans(
            mod,
            ~collection_date_num,
            by = "variant",
            at = list(collection_date_num = dat[160, "collection_date_num"]),
            mode = "latent",
            level = 0.95
        )
    )
    p3 <- transform(p3, variant = as.character(variant))
    expect_equal(p3$emmean, p2[p2$rowid == 160, "estimate"], ignore_attr = TRUE)
    expect_equal(p3$SE, p2[p2$rowid == 160, "std.error"], tolerance = 1e-5, ignore_attr = TRUE)
    expect_equal(as.character(p3$variant), as.character(p2[p2$rowid == 160, "group"]), ignore_attr = TRUE)
})
