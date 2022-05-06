# set.seed(1)
# library(data.table)
# library(survey)
# setDT(nhanes)
# setnames(nhanes, tolower(names(nhanes)))
# nhanes[, female := riagendr == 2]
# nhanes[, race := sprintf("race%s", race)]
# nhanes[, rnd := rnorm(.N, 10, 5)]
# mod <- glm(hi_chol ~ female,
#            data = nhanes,
#            family = binomial)
# pkgload::load_all()
#
# # default
# comparisons(mod) |> summary()
#
# # default: specified manually
# comparisons(mod, transformation = \(hi, lo) hi - lo) |> summary()
#
# #  
# comparisons(mod, transformation = \(hi, lo) hi / lo) |> summary()
#
# comparisons(mod, transformation = \(hi, lo) log(hi) - log(lo)) |> summary()
#
# comparisons(mod, transformation = \(hi, lo) log(hi / lo)) |> summary()
