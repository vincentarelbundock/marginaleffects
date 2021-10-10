requiet("ordinal")


devtools::document()
pkgload::load_all()

###############
#  binary DV  #
###############

data(soup, package = "ordinal")
model <- clm(factor(rating) ~ contact + temp, data = wine)

get_predict(model, group_name = "1")

marginaleffects(model) |> summary()
summary(fm1)

m1 <- clm(PROD ~ COLD, scale = ~prod, data = soup, link = "logit")
m2 <- update(m1, link = "probit")
m3 <- update(m1, link = "cloglog")
m4 <- update(m1, link = "loglog")
m5 <- update(m1, link = "cauchit", start = coef(m1))

tidy(m1)
tidy(marginaleffects(m3))

expect_marginaleffects(m1, n_unique = 6)
expect_marginaleffects(m2, n_unique = 6)
expect_marginaleffects(m3, n_unique = 6)
expect_marginaleffects(m4, n_unique = 6)
expect_marginaleffects(m5, n_unique = 6)


## simple model:
####################
#  multinomial DV  #
####################

fm1 <- clm(rating ~ contact + temp, data = wine)
summary(fm1)

## Fitted values with standard errors and confidence intervals:
predict(fm1, se.fit=TRUE, interval=TRUE) # type="prob"
## class predictions for the observations:
predict(fm1, type="class")

newData <- expand.grid(temp = c("cold", "warm"),
                       contact = c("no", "yes"))

## Predicted probabilities in all five response categories for each of
## the four cases in newData:
predict(fm1, newdata = newData, type="prob")

## now include standard errors and intervals:
predict(fm1, newdata = newData, se.fit = TRUE, interval = TRUE, type = "prob")
