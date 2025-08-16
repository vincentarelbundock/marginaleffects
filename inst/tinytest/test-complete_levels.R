source("helpers.R")
using("marginaleffects")

# padding with interactions
dat <- get_dataset("movies", "ggplot2movies")
dat$style <- ifelse(dat$Action == 1, "Action", "Other")
dat$style <- ifelse(dat$Comedy == 1, "Comedy", dat$style)
dat$style <- ifelse(dat$Drama == 1, "Drama", dat$style)
dat$style <- factor(dat$style)
dat$certified_fresh <- dat$rating >= 8
dat <- dat[dat$length < 240, ]
mod <- glm(certified_fresh ~ length * style, data = dat, family = binomial)
expect_predictions(mod, n_row = nrow(dat), se = FALSE)
