source("helpers.R")
using("marginaleffects")

requiet("crch")
requiet("ordinal")

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/crch/RainIbk.csv")
q <- unique(stats::quantile(dat$rain, seq(0.1, 0.9, 0.1)))
dat$rain_sqrt <- sqrt(dat$rain)
dat$sqrtensmean <- apply(sqrt(dat[,grep('^rainfc',names(dat))]), 1, mean)
dat$sqrtenssd <- apply(sqrt(dat[,grep('^rainfc',names(dat))]), 1, sd)
dat$enssd <- apply(dat[,grep('^rainfc',names(dat))], 1, sd)
dat$ensmean <- apply(dat[,grep('^rainfc',names(dat))], 1, mean)
dat <<- subset(dat, enssd > 0)

# marginaleffects: crch gaussian: no validity
model <- crch(sqrt(rain) ~ sqrtensmean + sqrtenssd, data = dat, dist = "gaussian")
expect_slopes(model, n_unique = 1, type = "location")


# logistic: no validity
model <- crch(sqrt(rain) ~ sqrtensmean | sqrtenssd, data = dat, dist = "logistic", left = 0)
expect_slopes(model, type = "location", n_unique = 1)

mfx <- slopes(model, type = "location", variables = "sqrtensmean")
expect_true(!any(mfx$estimate == 0))

mfx <- slopes(model, type = "location", variables = "sqrtenssd")
expect_true(all(mfx$estimate == 0))

mfx <- slopes(model, type = "scale", variables = "sqrtensmean")
expect_true(all(mfx$estimate == 0))

mfx <- slopes(model, type = "scale", variables = "sqrtenssd")
expect_true(!any(mfx$estimate == 0))


# hlxr: no validity
mod <- hxlr(rain_sqrt ~ sqrtensmean, data = dat, thresholds = sqrt(q))
expect_slopes(mod, type = "location", n_unique = 1)


# predictions: crch gaussian: no validity
model <- crch(sqrt(rain) ~ sqrtensmean + sqrtenssd, data = dat, dist = "gaussian")
pred1 <- predictions(model, newdata = dat)
pred2 <- predictions(model, newdata = head(dat))
expect_predictions(pred1, n_row = nrow(dat))
expect_predictions(pred2, n_row = 6)


# marginalmeans: crch gaussian: no validity
tmp <- dat
tmp$categ <- as.factor(sample(letters[1:5], nrow(tmp), replace = TRUE))
model <- crch(sqrt(rain) ~ sqrtensmean + sqrtenssd + categ, 
          data = tmp, dist = "gaussian")
mm <- marginal_means(model)
expect_marginal_means(mm, n_row = 5)




source("helpers.R")
rm(list = ls())