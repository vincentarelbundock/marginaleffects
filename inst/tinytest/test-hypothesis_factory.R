source("helpers.R")
using("marginaleffects")
# library("MASS")
# library("brms")

dat = data.table::data.table(mtcars)
dat[, cyl := factor(cyl)][
    , gear := factor(gear)]
dat = dat[order(gear, cyl, am)]
mod <- lm(mpg ~ vs * cyl * am, data = dat)

pkgload::load_all()
avg_comparisons(mod, 
    variables = "hp",
    hypothesis = ~ reference | gear,
    by = c("cyl", "gear"))

avg_predictions(mod, 
    hypothesis = difference ~ sequential | cyl,
    by = c("cyl", "gear"))

avg_predictions(mod, 
    hypothesis = difference ~ reference | am,
    by = c("cyl", "vs", "am"))


# # Example usage:
# formula <- ratio ~ pairwise | id + group + blah
# f <- ratio ~ pairwise | id + group + blah
# f <- ~ ratio | group
# length(f)
# all.vars(formula)
# sanitize_hypothesis_formula(ratio ~ pairwise | blah + blah)
# print(result)
