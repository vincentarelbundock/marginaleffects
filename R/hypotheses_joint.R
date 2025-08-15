joint_test <- function(
    object,
    joint_index = NULL,
    hypothesis = 0,
    joint_test = "f",
    df = NULL,
    vcov = TRUE) {
    checkmate::assert_choice(joint_test, c("f", "chisq"))

    mfx <- components(object, "all")

    if (joint_test == "f") {
        checkmate::assert_numeric(df, len = 2, null.ok = TRUE)
    } else {
        checkmate::assert_numeric(df, len = 1, null.ok = TRUE)
    }

    # theta_hat: P x 1 vector of estimated parameters
    if (inherits(object, c("slopes", "comparisons"))) {
        nam <- object$term
        if ("contrast" %in% names(object)) {
            nam <- paste(nam, object$contrast)
        }
        theta_hat <- stats::setNames(object$estimate, nam)
    } else {
        theta_hat <- get_coef(object)
    }

    # index
    checkmate::assert(
        checkmate::check_integerish(
            joint_index,
            lower = 1,
            upper = length(theta_hat)
        ),
        checkmate::check_character(joint_index),
        checkmate::check_true(joint_index)
    )

    if (isTRUE(joint_index)) {
        joint_index <- seq_along(theta_hat)
    } else if (isTRUE(checkmate::check_string(joint_index))) {
        joint_index <- grep(joint_index, names(theta_hat), perl = TRUE)
    }

    # V_hat: estimated covariance matrix
    V_hat <- get_vcov(object, vcov = vcov)

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
    if (is.null(hypothesis)) {
        hypothesis <- 0
    }
    r <- matrix(hypothesis, nrow = nrow(R), ncol = 1)

    # Calculate the difference between R*theta_hat and r
    diff <- R %*% theta_hat - r

    # Calculate the inverse of R*(V_hat/n)*R'
    inv <- solve(R %*% V_hat %*% t(R))

    # Calculate the Wald test statistic
    if (joint_test == "f") {
        wald_statistic <- t(diff) %*% inv %*% diff / dim(R)[1] # Q is the number of rows in R
    } else if (joint_test == "chisq") {
        wald_statistic <- t(diff) %*% inv %*% diff # Not normalized for chi-squared joint_test
    }

    # Degrees of freedom
    if (is.null(df)) {
        df1 <- dim(R)[1] # Q

        if (joint_test == "f") {
            # Check for lme models and warn about df heuristics
            if (inherits(object, "lme") || (!is.null(mfx) && inherits(mfx@model, "lme"))) {
                model_class <- if (inherits(object, "lme")) class(object)[1] else class(mfx@model)[1]
                msg <- "The `hypotheses()` functions uses simple heuristics to select degrees of freedom for this test. See the relevant section in `?hypotheses`. These rules are likely to yield inappropriate results for models of class `%s`. Please supply degrees of freedom values explicitly via the `df` argument."
                warn_sprintf(msg, model_class)
            }

            df2 <- tryCatch(
                insight::get_df(mfx@model),
                error = function(e) NULL
            )
            if (is.null(df2)) {
                tryCatch(insight::get_df(object), error = function(e) NULL)
            }
            if (is.null(df2)) {
                # n: sample size
                n <- tryCatch(stats::nobs(object), error = function(e) NULL)
                if (is.null(n)) {
                    n <- tryCatch(
                        stats::nobs(mfx@model),
                        error = function(e) NULL
                    )
                }
                if (is.null(n)) {
                    stop_sprintf(
                        "Could not extract sample size from model object."
                    )
                }

                df2 <- n - length(theta_hat) # n - P
            }
        }
    } else {
        df1 <- df[1]

        if (joint_test == "f") {
            df2 <- df[2]
        }
    }

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
    } else if (
        inherits(
            object,
            c("marginaleffects", "comparisons", "slopes", "marginal_means")
        )
    ) {
        tmp <- paste0(
            get_labels(object, idx = joint_index),
            sprintf(" = %s\n", hypothesis)
        )
        print_head <- c(print_head, tmp)
    } else {
        tmp <- paste0(
            get_labels(stats::coef(object), idx = joint_index),
            sprintf(" = %s\n", hypothesis)
        )
        print_head <- c(print_head, tmp)
    }
    attr(out, "print_head") <- print_head

    return(out)
}
