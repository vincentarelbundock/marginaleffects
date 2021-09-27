# this does not work
# all marginal effects are zero. The 

library("mclogit")


data(Transport, package = "mclogit")
void <- capture.output(
    model <- mclogit(cbind(resp, suburb) ~ distance + cost, data = Transport)
)

data(electors, package = "mclogit")
void <- capture.output(
    model <- (mclogit(
      cbind(Freq,interaction(time, class)) ~ econ.left / class + welfare / class + auth / class,
      random = ~ 1 | party.time,
      data = within(electors, party.time <- interaction(party, time))))
)

get_coef(model)

get_predict(model)

get_vcov(model)

beta <- setNames(rep(0, length(get_coef(model))), names(get_coef(model)))
model2 <- set_coef(model, beta)
get_predict(model) |> head()
get_predict(model2) |> head()

