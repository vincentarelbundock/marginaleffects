pkgload::load_all()

tmp <- mtcars
tmp$am <- as.logical(tmp$am)
tmp$cyl <- as.factor(tmp$cyl)
mod <- lm(mpg ~ cyl + am + hp, tmp)

# Correct
marginalmeans(mod, variables = c("am", "cyl"))

# Incorrect: emmeans marginalizes over all categoricals automatically, whereas
# this holds `cyl` at its mode.
marginalmeans(mod, variables = "am")

emmeans::emmeans(mod, "cyl")
emmeans::emmeans(mod, "am")
