# TODO: multiple terms comparisons



source("helpers.R")
using("marginaleffects")
library("MASS")
# library("brms")

dat <- transform(mtcars, gear = factor(gear))
mod <- lm(hp ~ mpg * qsec * am + factor(cyl), data = dat)
# mod <- polr(gear ~ mpg + qsec, data = dat, Hess = TRUE)
# mod <- brm(hp ~ mpg * qsec * am, data = dat, backend = "cmdstanr")


# fun <- hypothesis_helper(by = c("term", "contrast", "am"), hypothesis = "sequential")
# comparisons(mod, hypothesis = fun) |> dplyr::arrange(term) |>  print(nrows = 100)

nd <- datagrid(am = 0:1, qsec = fivenum, model = mod)

predictions(mod, newdata = nd)

hyp <- hypothesis_helper()
pkgload::load_all()
predictions(mod, newdata = nd, hypothesis = hyp)

hyp <- hypothesis_helper(by = "am")
predictions(mod, newdata = nd, hypothesis = hyp)

hyp <- hypothesis_helper(by = "am", hypothesis = "sequential")
predictions(mod, newdata = nd, hypothesis = hyp)

hyp <- hypothesis_helper(hypothesis = "sequential")
predictions(mod, by = c("am", "cyl"))
predictions(mod, newdata = nd, hypothesis = hyp)

hyp <- hypothesis_helper(hypothesis = "reference", by = "am")
predictions(mod, by = c("am", "cyl"), hypothesis = hyp)

hyp <- hypothesis_helper(
    by = "am",
    hypothesis = \(x) (x / x[1])[2:length(x)],
    label = \(x) sprintf("(%s) / (%s)", x, x[1])[2:length(x)],
    label_columns = c("rowid", "mpg")
)
predictions(mod, 
    newdata = datagrid(am = unique, qsec = fivenum, mpg = range), 
    hypothesis = hyp)

