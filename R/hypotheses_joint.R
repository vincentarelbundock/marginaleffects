joint_test <- function(object, idx, null_hypothesis = 0, test = "f") {
  checkmate::assert_choice(test, c("f", "chisq"))
  checkmate::assert(
    checkmate::check_integerish(idx),
    checkmate::check_character(idx)
  )
  checkmate::assert_number(null_hypothesis)
  
  # theta_hat: P x 1 vector of estimated parameters
  theta_hat <- coef(object) 
  # V_hat: estimated covariance matrix
  V_hat <- stats::vcov(object)
  # n: sample size
  n <- tryCatch(stats::nobs(object), error = function(e) {
    tryCatch(stats::nobs(attr(object, "model")), error = insight::format_error(
        "Could not extract sample size from model object."
    ))
  })
  
  # R: Q x P matrix for testing Q hypotheses on P parameters
  # build R matrix based on idx
  R <- matrix(0, nrow = length(idx), ncol = length(theta_hat))
  for (i in seq_along(idx)) {
    if (is.numeric(idx)) {
        R[i, idx[i]] <- 1
    } else {
        R[i, which(names(theta_hat) == idx[i])] <- 1
    }
  }

  r <- matrix(null_hypothesis, nrow = nrow(R), ncol = 1)

  # Calculate the difference between R*theta_hat and r
  diff <- R %*% theta_hat - r
  
  # Calculate the inverse of R*(V_hat/n)*R'
  inv <- solve(R %*% V_hat %*% t(R))
  
  # Calculate the Wald test statistic
  if (test == "f") {
    wald_statistic <- t(diff) %*% inv %*% diff / dim(R)[1]  # Q is the number of rows in R
  } else if (test == "chisq") {
    wald_statistic <- t(diff) %*% inv %*% diff  # Not normalized for chi-squared test
  }
  
  # Degrees of freedom
  df1 <- dim(R)[1]  # Q
  df2 <- n - length(theta_hat)  # n - P
  
  # Calculate the p-value
  if (test == "f") {
    p_value <- 1 - pf(wald_statistic, df1, df2)
  } else if (test == "chisq") {
    p_value <- 1 - pchisq(wald_statistic, df1)
  }
  
  # Return the Wald test statistic and p-value
  return(list(wald_statistic = drop(wald_statistic), p_value = drop(p_value)))
}
