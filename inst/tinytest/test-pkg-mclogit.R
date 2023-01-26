source("helpers.R")
using("marginaleffects")

exit_if_not(requiet("mclogit"))
exit_if_not(requiet("MASS"))
exit_if_not(requiet("emmeans"))
exit_if_not(requiet("broom"))
exit_if_not(requiet("splines"))

# mclogit: no validity
data(Transport, package = "mclogit")
dat <- Transport
void <- capture.output(
    model <- mclogit(cbind(resp, suburb) ~ distance + cost, data = Transport)
)
# type = "response" produces 0 dydx and standard error. Not sure why
# because `get_predict(newdata)` seems to work
expect_error(slopes(model, type = "response"), pattern = "type. argument")
# type = "link" works
suppressWarnings(expect_slopes(model, type = "link", n_unique = 1))
pred <- predictions(model, type = "link")
expect_predictions(pred)


# mblogit: error on character regressors
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/MASS/housing.csv")
dat$x <- rnorm(nrow(dat))
dat$Sat <- factor(dat$Sat)
dat <- dat
void <- capture.output(
    mod <- mblogit(Sat ~ Infl + Type + Cont + x, weights = Freq, data = dat)
)
expect_predictions(predictions(mod))
expect_error(suppressWarnings(slopes(mod, type = "link")), pattern = "character")

# mblogit: works on factor regressors
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/MASS/housing.csv")
dat$x <- rnorm(nrow(dat))
dat$Sat <- factor(dat$Sat)
dat$Infl <- factor(dat$Infl)
dat$Cont <- factor(dat$Cont)
dat$Type <- factor(dat$Type)
void <- capture.output(
mod <- mblogit(Sat ~ Infl + Type + Cont + x, weights = Freq, data = dat))
expect_predictions(predictions(mod))
mfx <- suppressWarnings(slopes(mod, type = "link"))
expect_inherits(mfx, "marginaleffects")



dat <- "https://github.com/vincentarelbundock/modelarchive/raw/main/data-raw/covid_variants.csv"
dat <- read.csv(dat)
dat <- dat[dat$variant %in% c("Alpha", "Beta", "Other"), ]
dat$variant <- factor(dat$variant, levels = c("Other", "Beta", "Alpha"))
dat <- dat
void <- capture.output(suppressWarnings(
    mod <- mblogit(
        formula = variant ~ ns(collection_date_num, df = 2),
        weights = count,
        data = dat,
        from.table = FALSE, dispersion = FALSE,
        verbose = FALSE,
        control = mclogit.control(maxit = 100))
))

# response
p1 <- predict(mod, type = "response", se.fit = TRUE)
p2 <- predictions(mod)
expect_equivalent(p1$fit[160,], p2[p2$rowid == 160, "estimate"])
expect_equivalent(p1$se.fit[160,], p2[p2$rowid == 160, "std.error"], tolerance = .001)

# link
p1 <- predict(mod, type = "link", se.fit = TRUE)
p2 <- predictions(mod, type = "link")
expect_equivalent(p1$fit[160,], p2[p2$rowid == 160, "estimate"])
expect_equivalent(p1$se.fit[160,], p2[p2$rowid == 160, "std.error"], tolerance = .001)

# latent
p2 <- predictions(mod, type = "latent")
p3 <- data.frame(
    emmeans(mod, ~collection_date_num,
    by = "variant",
    at = list(collection_date_num = dat[160, "collection_date_num"]),
    mode = "latent",
    level = 0.95))
expect_equivalent(p3$emmean, p2[p2$rowid == 160, "estimate"])
expect_equivalent(p3$SE, p2[p2$rowid == 160, "std.error"])