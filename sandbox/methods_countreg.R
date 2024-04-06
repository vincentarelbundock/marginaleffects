# marginaleffects extension to zerotrunc model class from countreg package
# Following https://marginaleffects.com/vignettes/extensions.html

library(marginaleffects)
options("marginaleffects_model_classes" = "zerotrunc")

get_coef.zerotrunc <- function(model, ...) {
  b <- coef(model)
  return(b)
}

set_coef.zerotrunc <- function(model, coefs, ...) {
  out <- model
  out$b <- coefs
  return(out)
}

get_vcov.zerotrunc <- function(model, ...) {
  return(model$vcov)
}

get_predict.zerotrunc <- function(model, newdata, ...) {
    Yhat<-as.matrix(predict(model,type="response",newdata=newdata))
    out <- data.frame(
      predicted = as.vector(Yhat),
      rowid = seq_len(nrow(Yhat)))
  return(out)
}