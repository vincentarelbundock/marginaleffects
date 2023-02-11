source("helpers.R")
using("marginaleffects")

set.seed(1)
n <- 10
group_id <- c(rep("A", n / 2), rep("B", n / 2))
x <- runif(n, 0, 10)

xA <- x[group_id == "A"]
xB <- x[group_id == "B"]

yA <- 2 + 2 * xA + rnorm(n / 2, 0, 0.1)
yB <- -1 + 3 * xB + rnorm(n / 2, 0, 0.1)

simdat <- data.frame(group_id = group_id, x = c(xA, xB), y = c(yA, yB))
simdat$group_id <- as.factor(simdat$group_id)
tmp <- simdat

model_additive <- lm(y ~ x + group_id, data = tmp)
model_interaction <- lm(y ~ x * group_id, data = tmp)

simdat_doA <- simdat_doB <- tmp
simdat_doA$group_id <- "A"
simdat_doB$group_id <- "B"

g1 <- mean(predict(model_additive, newdata = simdat_doB)) -
      mean(predict(model_additive, newdata = simdat_doA))
g2 <- mean(predict(model_interaction, newdata = simdat_doB)) -
      mean(predict(model_interaction, newdata = simdat_doA))

c1 <- comparisons(model_additive, variable = "group_id", newdata = tmp)
c1 <- tidy(c1)

c2 <- comparisons(model_interaction, variable = "group_id", newdata = tmp)
c2 <- tidy(c2)

expect_equivalent(g1, c1$estimate)
expect_equivalent(g2, c2$estimate)



rm(list = ls())