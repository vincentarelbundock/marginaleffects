source("helpers.R")
using("marginaleffects")


# type dictionary does not include duplicates
x <- marginaleffects:::type_dictionary
y <- marginaleffects:::type_dictionary_build()
expect_equivalent(x, y) # for codecov
dup <- x[!x$base %in% c("link", "prediction"), ]
dup <- stats::na.omit(dup)
expect_false(any(dup$base == dup$insight))
dup <- stats::na.omit(x[, c("class", "base")])
dup <- paste(dup$class, dup$base)
expect_true(anyDuplicated(dup) == 0)
dup <- stats::na.omit(x[, c("class", "insight")])
dup <- paste(dup$class, dup$insight)
expect_true(anyDuplicated(dup) == 0)



# sanity gives informative error for all the functions
dat <- mtcars
dat$cyl <- factor(dat$cyl)
dat <- dat
mod <- lm(mpg ~ hp + cyl, data = dat)
expect_error(comparisons(mod, type = "junk"), pattern = "type.*argument")
expect_error(predictions(mod, type = "junk"), pattern = "type.*argument")
expect_error(slopes(mod, type = "junk"), pattern = "type.*argument")
expect_error(marginal_means(mod, type = "junk"), pattern = "type.*argument")



# error: multivariate
exit_if_not(requiet("pscl"))
dat2 <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/pscl/bioChemists.csv")
model <- hurdle(art ~ phd + fem | ment, data = dat2, dist = "negbin")
mfx <- slopes(model, type = "prob")
expect_true(all(as.character(0:19) %in% mfx$group))

