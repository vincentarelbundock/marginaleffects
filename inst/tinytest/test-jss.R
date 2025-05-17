library(tinytest)

# make sure that the JSS data is always hosted at the same link forever
dat <- read.csv("https://marginaleffects.com/data/titanic.csv")
expect_inherits(dat, "data.frame")
dat <- read.csv("https://marginaleffects.com/data/impartiality.csv")
expect_inherits(dat, "data.frame")
