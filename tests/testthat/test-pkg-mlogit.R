# todo: sanity check if balanced
# todo: sanity check if long format

library(mlogit)
library(AER)
data("TravelMode", package = "AER")

mod <- mlogit(choice ~ wait + gcost | income + size, TravelMode)
dat <- TravelMode
dat2 <- dat[c(2, 1, 3:nrow(dat)),]

comparisons(mod) |> summary()

marginaleffects(mod) |> summary()


