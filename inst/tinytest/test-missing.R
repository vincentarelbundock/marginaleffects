source("helpers.R", local = TRUE)

tmp <- mtcars
tmp$am <- as.logical(tmp$am)
tmp$gear <- as.factor(tmp$gear)
for (i in seq_along(tmp)) {
    tmp[[i]][sample(1:nrow(tmp), 1)] <- NA
}

# original data with NAs do not pose problems in glm and lm.
mod1 <- lm(hp ~ mpg + drat + wt + gear, data = tmp)
mod2 <- glm(vs ~ mpg + drat + wt + gear, data = tmp, family = binomial)
expect_inherits(tidy(marginaleffects(mod1)), "data.frame")
expect_inherits(tidy(marginaleffects(mod2)), "data.frame")


# newdata with NAs do not pose problems in lm.
mod <- lm(hp ~ mpg + drat + wt + factor(gear), data = tmp)
mfx <- marginaleffects(mod, newdata = datagrid(drat = c(NA, 10)))
expect_inherits(tidy(mfx), "data.frame")

