source("helpers.R")
requiet("MatchIt")
using("checkmate")

gen_X <- function(n) {
  X <- matrix(rnorm(9 * n), nrow = n, ncol = 9)
  X[,5] <- as.numeric(X[,5] < .5)
  X
}
gen_A <- function(X) {
  LP_A <- - 1.2 + log(2)*X[,1] - log(1.5)*X[,2] + log(2)*X[,4] - log(2.4)*X[,5] + log(2)*X[,7] - log(1.5)*X[,8]
  P_A <- plogis(LP_A)
  rbinom(nrow(X), 1, P_A)
}
gen_Y_C <- function(A, X) {
  2*A + 2*X[,1] + 2*X[,2] + 2*X[,3] + 1*X[,4] + 2*X[,5] + 1*X[,6] + rnorm(length(A), 0, 5)
}
gen_Y_B <- function(A, X) {
  LP_B <- -2 + log(2.4)*A + log(2)*X[,1] + log(2)*X[,2] + log(2)*X[,3] + log(1.5)*X[,4] + log(2.4)*X[,5] + log(1.5)*X[,6]
  P_B <- plogis(LP_B)
  rbinom(length(A), 1, P_B)
}
gen_Y_S <- function(A, X) {
  LP_S <- -2 + log(2.4)*A + log(2)*X[,1] + log(2)*X[,2] + log(2)*X[,3] + log(1.5)*X[,4] + log(2.4)*X[,5] + log(1.5)*X[,6]
  sqrt(-log(runif(length(A)))*2e4*exp(-LP_S))
}
set.seed(19599)
n <- 2000
X <- gen_X(n)
A <- gen_A(X)
Y_C <- gen_Y_C(A, X)
Y_B <- gen_Y_B(A, X)
Y_S <- gen_Y_S(A, X)
d <- data.frame(A, X, Y_C, Y_B, Y_S)
mF <- matchit(
    A ~ X1 + X2 + X3 + X4 + X5 +
        X6 + X7 + X8 + X9,
    data = d,
    method = "full", estimand = "ATT")
md <- match.data(mF)
fit1 <- lm(Y_C ~ A * (X1 + X2 + X3 + X4 + X5 + 
                        X6 + X7 + X8 + X9),
           data = md, weights = weights)
pre <- avg_predictions(fit1,
    variables = "A",
    vcov = ~subclass,
    newdata = subset(md, A == 1),
    wts = "weights",
    by = "A")
expect_inherits(pre, "predictions")
expect_data_frame(pre, nrows = 2)
cmp <- avg_comparisons(fit1,
    variables = "A",
    vcov = ~subclass,
    newdata = subset(md, A == 1),
    wts = "weights")
expect_inherits(cmp, "comparisons")
expect_data_frame(pre, nrows = 2)
