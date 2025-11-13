testthat::skip_if_not_installed("mclogit")
testthat::skip_if_not_installed("MASS")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")
testthat::skip_if_not_installed("splines")

requiet("mclogit")
requiet("MASS")
requiet("emmeans")
requiet("broom")
requiet("splines")

# Basic expectation tests
data("Transport", package = "mclogit")
void <- capture.output(
    mod_simple <- mclogit::mclogit(cbind(resp, suburb) ~ distance + cost, data = Transport)
)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# mclogit: no validity
data(Transport, package = "mclogit")
dat_mclogit <- Transport
void <- capture.output(
    model <- mclogit(cbind(resp, suburb) ~ distance + cost, data = Transport)
)
# type = "link" works
expect_slopes2(model, type = "link")
expect_predictions2(model, type = "link")


# mblogit: error on character regressors
dat_mclogit2 <- get_dataset("housing", "MASS")
dat_mclogit2$x <- rnorm(nrow(dat_mclogit2))
dat_mclogit2$Sat <- factor(dat_mclogit2$Sat)
dat_mclogit2 <- dat_mclogit2
void <- capture.output(
    mod <- mblogit(Sat ~ Infl + Type + Cont + x, weights = Freq, data = dat_mclogit2)
)
expect_predictions2(mod)

# mblogit: works on factor regressors
dat_mclogit3 <- get_dataset("housing", "MASS")
dat_mclogit3$x <- rnorm(nrow(dat_mclogit3))
dat_mclogit3$Sat <- factor(dat_mclogit3$Sat)
dat_mclogit3$Infl <- factor(dat_mclogit3$Infl)
dat_mclogit3$Cont <- factor(dat_mclogit3$Cont)
dat_mclogit3$Type <- factor(dat_mclogit3$Type)
void <- capture.output(
    mod <- mblogit(Sat ~ Infl + Type + Cont + x, weights = Freq, data = dat_mclogit3)
)
expect_predictions2(mod)
mfx <- suppressWarnings(slopes(mod, type = "link"))
expect_s3_class(mfx, "marginaleffects")


dat_covid <- testing_path("modelarchive/data/covid_variants.csv")
dat_covid <- read.csv(dat_covid)
dat_covid <- dat_covid[dat_covid$variant %in% c("Alpha", "Beta", "Other"), ]
dat_covid$variant <- factor(dat_covid$variant, levels = c("Other", "Beta", "Alpha"))
dat_covid <- dat_covid
void <- capture.output(suppressWarnings(
    mod <- mblogit(
        formula = variant ~ ns(collection_date_num, df = 2),
        weights = count,
        data = dat_covid,
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
        at = list(collection_date_num = dat_covid[160, "collection_date_num"]),
        mode = "latent",
        level = 0.95
    )
)
p3 <- transform(p3, variant = as.character(variant))
expect_equal(p3$emmean, p2[p2$rowid == 160, "estimate"], ignore_attr = TRUE)
expect_equal(p3$SE, p2[p2$rowid == 160, "std.error"], tolerance = 1e-4, ignore_attr = TRUE)
expect_equal(as.character(p3$variant), as.character(p2[p2$rowid == 160, "group"]), ignore_attr = TRUE)
