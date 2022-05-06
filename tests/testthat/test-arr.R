requiet("survey")
data(nhanes, package = "survey")
dat <- setNames(nhanes, tolower(names(nhanes)))
dat$female <- dat$riagendr == 2
dat$race <- sprintf("race%s", dat$race)
mod <- glm(hi_chol ~ female, data = dat, family = binomial)
# pkgload::load_all()

test_that("error when function breaks or returns a bad vector", {
    expect_error(comparisons(mod, transformation = mean),
                 regexp = "numeric vector")
    expect_error(comparisons(mod, transformation = function(hi, lo) head(hi - lo)),
                 regexp = "numeric vector")
})

# # default
# comparisons(mod) |> summary()
#
# # default: specified manually
# comparisons(mod, transformation = \(hi, lo) hi - lo) |> summary()
#
# comparisons(mod, transformation = \(hi, lo) hi / lo) |> summary()
#
# comparisons(mod, transformation = \(hi, lo) log(hi) - log(lo)) |> summary()
#
# comparisons(mod, transformation = \(hi, lo) log(hi / lo)) |> summary()
