devtools::document()
pkgload::load_all()

mod <- lm(Sepal.Length ~ Sepal.Width + factor(Species), data = iris)
x = avg_predictions(mod, by = "Species")
x = avg_comparisons(mod, by = "Species")

inferences2(x, method = "boot", R = 200)
inferences2(x, method = "rsample", R = 200)
inferences2(x, method = "fwb", R = 200)
inferences2(x, method = "simulation", R = 200)
