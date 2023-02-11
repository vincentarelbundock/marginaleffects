source("helpers.R")
using("marginaleffects")

# padding with interactions
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/ggplot2movies/movies.csv")
dat$style <- ifelse(dat$Action == 1, "Action", "Other")
dat$style <- ifelse(dat$Comedy == 1, "Comedy", dat$style)
dat$style <- ifelse(dat$Drama == 1, "Drama", dat$style)
dat$style <- factor(dat$style)
dat$certified_fresh <- dat$rating >= 8
dat <- dat[dat$length < 240,]
mod <- glm(certified_fresh ~ length * style, data = dat, family = binomial)
res <- predictions(mod, type = "response")
expect_predictions(res, n_row = nrow(dat), se = FALSE)



rm(list = ls())