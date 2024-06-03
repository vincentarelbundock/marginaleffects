source("helpers.R")
requiet("marginaleffects")
using("marginaleffects")
# library("MASS")
# library("brms")

dat = data.table::data.table(iris)
dat[, big := as.numeric(Sepal.Width > mean(Sepal.Width))]
dat = dat[order(Species, big)]
mod <- lm(Sepal.Length ~ big * Species * Petal.Length, data = dat)

cmp <- avg_comparisons(mod, 
    variables = "big",
    hypothesis = ~ reference | Species,
    by = c("big", "Species"))
expect_inherits(cmp, "comparisons")

cmp <- avg_comparisons(mod, 
    variables = "Petal.Length",
    hypothesis = ~ meandev | Species,
    by = c("big", "Species"))
expect_inherits(cmp, "comparisons")

pre <- avg_predictions(mod, 
    hypothesis = ratio ~ sequential | big,
    by = c("big", "Species"))
expect_inherits(pre, "predictions")

