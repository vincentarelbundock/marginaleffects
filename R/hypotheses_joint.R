joint_test <- function(object, joint_index = NULL, hypothesis = 0, joint_test = "f") {
  checkmate::assert_choice(joint_test, c("f", "chisq"))

  # theta_hat: P x 1 vector of estimated parameters
  theta_hat <- stats::coef(object) 

  # index
  checkmate::assert(
    checkmate::check_integerish(joint_index, lower = 1, upper = length(theta_hat)),
    checkmate::check_character(joint_index),
    checkmate::check_true(joint_index)
  )

  if (isTRUE(joint_index)) {
    joint_index <- seq_along(theta_hat)
  } else if (isTRUE(checkmate::check_string(joint_index))) {
    joint_index <- grep(joint_index, names(theta_hat), perl = TRUE)
  }

  # V_hat: estimated covariance matrix
  V_hat <- stats::vcov(object)

  # n: sample size
  n <- tryCatch(stats::nobs(object), error = function(e) NULL)
  if (is.null(n)) n <- tryCatch(stats::nobs(attr(object, "model")), error = function(e) NULL)
  if (is.null(n)) insight::format_error("Could not extract sample size from model object.")
  
  # R: Q x P matrix for testing Q hypotheses on P parameters
  # build R matrix based on joint_index
  R <- matrix(0, nrow = length(joint_index), ncol = length(theta_hat))
  for (i in seq_along(joint_index)) {
    if (is.numeric(joint_index)) {
        R[i, joint_index[i]] <- 1
    } else {
        R[i, which(names(theta_hat) == joint_index[i])] <- 1
    }
  }

  # null hypothesis
  checkmate::assert(
    checkmate::check_number(hypothesis),
    checkmate::check_numeric(hypothesis, len = nrow(R)),
    checkmate::check_null(hypothesis)
  )
  if (is.null(hypothesis)) hypothesis <- 0
  r <- matrix(hypothesis, nrow = nrow(R), ncol = 1)

  # Calculate the difference between R*theta_hat and r
  diff <- R %*% theta_hat - r
  
  # Calculate the inverse of R*(V_hat/n)*R'
  inv <- solve(R %*% V_hat %*% t(R))
  
  # Calculate the Wald test statistic
  if (joint_test == "f") {
    wald_statistic <- t(diff) %*% inv %*% diff / dim(R)[1]  # Q is the number of rows in R
  } else if (joint_test == "chisq") {
    wald_statistic <- t(diff) %*% inv %*% diff  # Not normalized for chi-squared joint_test
  }
  
  # Degrees of freedom
  df1 <- dim(R)[1]  # Q
  df2 <- n - length(theta_hat)  # n - P
  
  # Calculate the p-value
  if (joint_test == "f") {
    p_value <- 1 - stats::pf(wald_statistic, df1, df2)
  } else if (joint_test == "chisq") {
    p_value <- 1 - stats::pchisq(wald_statistic, df1)
    df2 <- NULL
  }

  # Return the Wald joint_test statistic and p-value
  out <- data.frame(statistic = drop(wald_statistic), p.value = drop(p_value))
  class(out) <- c("hypotheses", "data.frame")
  if (joint_test == "f") {
    attr(out, "statistic_label") <- "F"
  } else if (joint_test == "chisq") {
    attr(out, "statistic_label") <- "ChiSq"
  }

  # degrees of freedom print
  if (joint_test == "f") {
    out$df1 <- df1
    out$df2 <- df2
  } else {
    out$df <- df1
  }

  # Create the print_head string
  print_head <- "\nJoint hypothesis test:\n"
  if (is.character(joint_index)) {
    for (i in joint_index) {
      print_head <- paste0(print_head, i, sprintf(" = %s\n", hypothesis))
    }
  } else if (inherits(object, c("marginaleffects", "comparisons", "slopes", "marginal_means"))) {
    tmp <- paste0(get_term_labels(object, joint_index), sprintf(" = %s\n", hypothesis))
    print_head <- c(print_head, tmp)
  } else {
    tmp <- paste0(get_term_labels(stats::coef(object), joint_index), sprintf(" = %s\n", hypothesis))
    print_head <- c(print_head, tmp)
  }
  attr(out, "print_head") <- print_head

  return(out)
}