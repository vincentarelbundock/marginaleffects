library(mgcv)
library(ggplot2)
library(patchwork)
library(marginaleffects)
library(itsadug)
library(mgcv)
theme_set(theme_bw())

simdat$Subject <- as.factor(simdat$Subject)

m <- bam(
    Y ~ Group + s(Time, by = Group) + s(Subject, bs = "re"),
    data = simdat
)

eps <- 1

grid <- datagrid(Time = seq(0, 2000, by = eps), model = m)

yhat <- predictions(m, newdata = grid)

first <- comparisons(m,
    variables = "Time",
    newdata = grid,
    comparison = "dydx"
)

second <- comparisons(m,
    variables = "Time",
    newdata = grid,
    comparison = "dydx",
    hypothesis = difference ~ sequential,
    transform = \(x) x / eps
)

second <- transform(second, Time = grid$Time[2:nrow(grid)])

p1 <- ggplot(yhat, aes(x = Time, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_ribbon(alpha = .2, color = NA) +
    geom_line() +
    labs(y = "Predicted")
p2 <- ggplot(first, aes(x = Time, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_ribbon(alpha = .2, color = NA) +
    geom_line() +
    labs(y = "First")
p3 <- ggplot(second, aes(x = Time, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_ribbon(alpha = .2, color = NA) +
    geom_line() +
    geom_hline(yintercept = 0, color = "red", linetype = 3) +
    labs(y = "Second")

p1 / p2 / p3
