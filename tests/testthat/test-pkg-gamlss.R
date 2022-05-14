# requiet("gamlss")
# data(abdom)
# mod <- gamlss(y ~ pb(x),
#     sigma.fo = ~ pb(x), family = BCT, data = dat,
#     method = mixed(1, 20))
#
# predictions(mod) |> tail()
#
# dim(abdom)
# dat <- abdom
# dat$z <- rnorm(nrow(dat))
#
# comparisons(mod) |> summary()
#
# get_predict(mod, newdata = dat) |> dim()
# marginaleffects(mod) |> head()
