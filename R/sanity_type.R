sanity_type <- function(model, type) {
    checkmate::assert_character(type, min.len = 1, null.ok = FALSE)

    type_error <- c(
        "multinom" = "probs",
        "clm" = c("prob", "cum.prob", "linear_predictor")
    )

    for (m in names(type_error)) {
        if (m == class(model)[1] && any(type != type_error[m])) {
            valid <- paste(sprintf('"%s"', type_error[m]), collapse = ", ")
            if (length(type_error[m]) > 1) { # plural
                msg <- 'The only prediction types supported for "%s" models are: %s. Please set a different value for the `type` argument.'
            } else { # singular
                msg <- 'The only prediction type supported for "%s" models is: %s. Please set a different value for the `type` argument.'
            }
            stop(sprintf(msg, m, valid))
        }
    }

    return(type)
}
