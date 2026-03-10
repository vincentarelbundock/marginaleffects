# extends {censreg}:
# * New prediction type
# * New get_predict() method
# submitted by Oleg Komashko



################### New prediction type for tobit() models.

library(AER)
library(censReg) # for internal check, see at the bottom
library(marginaleffects)
data("Affairs")

# example from help to tobit{AER}
model <-
  tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
        data = Affairs)

options("marginaleffects_model_classes" = "tobitK")

model_custom <- model

class(model_custom) <- c("tobitK", class(model))


get_predict.tobitK <-
  function(model, newdata, type, ...) {
    # for slopes()
    limits_extract <- function(mod) {

      # to compute predictions
      # with arbitrary tobit limits
      ll <- model$call$left
      if (is.null(ll)) ll <- 0
      rl <- model$call$right
      if (is.null(rl)) rl <- Inf

      return(list(ll, rl))
    }

    bounds <- limits_extract(model)
    ll <- bounds[[1]]
    if (!is.numeric(ll)) ll <- as.numeric(deparse1(ll))
    rl <- bounds[[2]]


    xbeta <- predict(model, newdata)
    sigma <- model$scale

    if (type == "response") {
      if (ll == 0 && rl == Inf) {
        z <- xbeta / sigma
        Yhat <- pnorm(z) * xbeta + sigma * dnorm(z)
      } else {
        F1 <- pnorm((ll - xbeta) / sigma)
        F2 <- pnorm((rl - xbeta) / sigma)
        f1 <- dnorm((ll - xbeta) / sigma)
        f2 <- dnorm((rl - xbeta) / sigma)
        Yhat <- F1 * ll + xbeta * (F2 - F1) + sigma * (f1 - f2) + (1 - F2) * rl
      }
    }
    if (type == "link") {
      if (ll == 0 && rl == Inf) {
        z <- xbeta / sigma
        Yhat <- xbeta + sigma * dnorm(z) / pnorm(z)
      } else {
        F1 <- pnorm((ll - xbeta) / sigma)
        F2 <- pnorm((rl - xbeta) / sigma)
        f1 <- dnorm((ll - xbeta) / sigma)
        f2 <- dnorm((rl - xbeta) / sigma)
        Yhat <- xbeta + sigma * (f1 - f2) / (F2 - F1)
      }
    }

    out <- data.frame(
      rowid = length(Yhat),
      estimate = Yhat
    )
    return(out)
  }


newda <- Affairs |>
  dplyr::select(age, yearsmarried, religiousness, occupation, rating) |>
  dplyr::summarise_all(.funs = mean)
newda

means
means <- c(1,as.numeric(Affairs |>
                          dplyr::select(age, yearsmarried, religiousness, occupation, rating) |>
                          dplyr::summarise_all(.funs = mean)))

# compute analytic dY/dX
b <- as.numeric(coef(model_custom))
b[-1]*pnorm(sum(means*b)/model_custom$scale)

options(marginaleffects_numDeriv = list(method = "Richardson", method.args = list(eps = 1e-5)))
options(marginaleffects_numDeriv = NULL)

slopes(model_custom, newdata = "mean")
slopes(model_custom, newdata = newda)







################ get_predict() method for tobitC model

library(marginaleffects)
library(censReg)
library(AER)
options("marginaleffects_model_classes" = "tobitC")

tobitC <-
  function(formula,
           left = 0,
           right = Inf,
           data = sys.frame(sys.parent()),
           subset = NULL,
           start = NULL,
           nGHQ = 8,
           logLikOnly = FALSE,
           ...) {
    if (is.null(subset)) {
      out <-
        censReg(
          formula = formula,
          left = left,
          right = right,
          data = data,
          start = start, nGHQ = nGHQ, logLikOnly = logLikOnly, ... )
    } else {
      out <-
        censReg(
          formula = formula,
          left = left,
          right = right,
          data = data,
          subset = subset, start = start, nGHQ = nGHQ, logLikOnly = logLikOnly, ... )
    }
    auxlm <- lm(formula(out), data = data)
    
    
    out$xlevels <- auxlm$xlevels
    out$coefficients <- coef(out, logSigma = F)
    out$vcov <- vcov(out)
    out$left <-left
    out$right <- right
    out$rank <- auxlm$rank
    out$n <- length(auxlm$residuals)
    out$qr <- qr(auxlm)
    #out$auxlm <- auxlm
    class(out) <- c("tobitC", "censReg","maxLik","maxim","list") 
    return(out)
  }


get_predict.tobitC <- function (object,
                                newdata,
                                type = c("response", "link"),
                                ...)
{
  tt <- stats::terms(object)
  if (missing(newdata) || is.null(newdata)) {
    mm <- X <- model.matrix(object)
  }
  else {
    Terms <- delete.response(tt)
    m <- model.frame(Terms,
                     newdata,
                     xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)
    X <- model.matrix(Terms, m)
  }
  
  n <- object$n
  p <- object$rank
  
  p1 <- seq_len(p)
  piv <- if (p)
    object$qr$pivot[p1]
  # if (p < ncol(X) && !(missing(newdata) || is.null(newdata)))
  #     warning("prediction from a rank-deficient fit may be misleading")
  beta_big <- object$coefficients
  beta <- beta_big[-length(beta_big)]
  sigma <- beta_big[length(beta_big)]
  xbeta <- drop(X[, piv, drop = FALSE] %*% beta)
  xbeta <- X %*% beta
  type <- match.arg(type)
  
  # if (missing(newdata) && !is.null(na.act <- object$na.action)) {
  #   xbeta <- napredict(na.act, xbeta)
  # }
  ll <- object$left
  rl <- object$right
  if (type == "response") {
    if (ll == 0 && rl == Inf) {
      z <- xbeta / sigma
      Yhat <-
        pnorm(z) * xbeta + sigma * dnorm(z)
    } else {
      F1 <- pnorm((ll-xbeta)/sigma)
      F2 <- pnorm((rl-xbeta)/sigma)
      f1 <- dnorm((ll-xbeta)/sigma)
      f2 <- dnorm((rl-xbeta)/sigma)
      Yhat <-
        F1*ll + xbeta*(F2 - F1) + sigma*(f1 - f2) + (1 - F2)*rl
    }
  }
  if (type == "link") {
    
    if (ll == 0 && rl == Inf) {
      z <- xbeta / sigma
      Yhat <- xbeta + sigma*dnorm(z)/pnorm(z)
    } else {
      F1 <- pnorm((ll-xbeta)/sigma)
      F2 <- pnorm((rl-xbeta)/sigma)
      f1 <- dnorm((ll-xbeta)/sigma)
      f2 <- dnorm((rl-xbeta)/sigma)
      Yhat <- xbeta + sigma*(f1 - f2)/(F2-F1)
    }
    
  }
  out <- data.frame(rowid = length(Yhat),
                    estimate = Yhat)
  
  return(out)
}

get_coef.tobitC <- function(model, ...) {
  b <- model$coefficients
  # b <- b[-length(b)]
  # b <- setNames(as.vector(b), names(b))
  b <- setNames(as.vector(b), names(b))
  return(b)
}
class(get_coef.tobitC(tc))



set_coef.tobitC <- function(model, coefs, ...) {
  out <- model
  out$coefficients <- coefs
  return(out)
}

get_vcov.tobitC <- function(model, ...) {
  return(model$vcov)
  
}

data("Affairs")
(tc <- tobitC(affairs ~ age + yearsmarried + religiousness + occupation + rating,
              data = Affairs))


newda <- Affairs |>
  dplyr::select(age, yearsmarried, religiousness, occupation, rating) |>
  dplyr::summarise_all(.funs = mean)

###############################################
# Examples

data("Affairs")
newda <- Affairs |>
  dplyr::select(age, yearsmarried, religiousness, occupation, rating) |>
  dplyr::summarise_all(.funs = mean)
(tc <-
    tobitC(affairs ~ age + yearsmarried + religiousness + occupation + rating,
           data = Affairs))

myslopes <- slopes(tc, newdata = newda)

(check_slopes <- myslopes[c(1,5,4,2,3),1:8])

# Margeff treat each term in formula as an independent variable
# back side of this: effects are analytic and good for check

crc <-
  tobitC(affairs ~ age + yearsmarried + religiousness + occupation + rating,
         data = Affairs)
(his_slopes <- summary(margEff(crc)))

# Comparing s.e

(check <- check_slopes[[4]])
(benchmark <- his_slopes[,2])

check - benchmark

# > check - benchmark
#           age  yearsmarried religiousness    occupation        rating 
# -7.492401e-07  3.779774e-06 -8.762331e-06  2.457499e-06 -1.219243e-05 
