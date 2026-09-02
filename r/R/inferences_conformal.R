# Shared utilities for conformal prediction methods

get_conformal_score <- function(x, score, mfx = NULL) {
    if (is.null(mfx)) {
        mfx <- attr(x, "marginaleffects")
    }
    model <- mfx@model
    response <- get_conformal_response(x, model)
    if (!is.numeric(response) && score != "softmax") {
        stop_sprintf(
            'The response must be numeric. Did you want to use `conformal_score="softmax"`?'
        )
    }
    if (score == "residual_abs") {
        out <- abs(response - x$estimate)
    } else if (score == "residual_sq") {
        out <- (response - x$estimate)^2
        attr(out, "residual_abs") <- abs(response - x$estimate)
    } else if (score == "softmax") {
        response <- x[[insight::find_response(model)]]
        if (is.numeric(response) && is_binary(response)) {
            # See p.4 of Angelopoulos, Anastasios N., and Stephen Bates. 2022. "A
            # Gentle Introduction to Conformal Prediction and Distribution-Free
            # Uncertainty Quantification." arXiv.
            # https://doi.org/10.48550/arXiv.2107.07511.
            # 1 minus the softmax output of the true class
            out <- ifelse(response == 1, 1 - x$estimate, x$estimate)
        } else if ("group" %in% colnames(x)) {
            # HACK: is this fragile? I think `group` should always be character.
            idx <- as.character(response) == as.character(x$group)
            if (!any(idx)) {
                stop_sprintf("No matching group found for conformal score computation.")
            }
            out <- 1 - x$estimate[idx]
        } else {
            stop_sprintf("Failed to compute the conformity score.")
        }
    }
    return(out)
}


get_conformal_bounds <- function(x, score, conf_level, mfx = NULL) {
    if (is.null(mfx)) {
        mfx <- attr(x, "marginaleffects")
    }
    model <- mfx@model
    response_name <- insight::find_response(model)
    response <- x[[response_name]]
    resid <- attr(score, "residual_abs")
    if (is.null(resid)) {
        resid <- score
    }
    # Finite-sample split-conformal quantile: the ceiling((n + 1) * conf_level)-th
    # smallest calibration score. A sample quantile picked an order statistic
    # one too low for most calibration sizes, which under-covered.
    n <- length(score)
    k <- ceiling((n + 1) * conf_level)
    if (k > n) {
        warn_sprintf(
            "The calibration set has %s observations, which is too small for a %s%% conformal interval: the bounds are infinite. Use a larger calibration set or a lower confidence level.",
            n,
            100 * conf_level
        )
        d <- Inf
        q <- Inf
    } else {
        ord <- order(score)
        d <- resid[ord][k]
        q <- score[ord][k]
    }
    if ("group" %in% colnames(x)) {
        out <- x[x$estimate > (1 - q), ]
        data.table::setDT(out)
        out <- out[,
            .(pred.set = list(unique(group))),
            by = c("rowid", response_name)
        ]
        setorder(out, rowid)
        data.table::setDF(out)
        class(out) <- c("predictions", class(out))
        attr(out, "variables_datagrid") <- response_name
        return(out)
    } else {
        # continuous outcome: conformity half-width
        x$pred.low <- x$estimate - d
        x$pred.high <- x$estimate + d
    }
    return(x)
}


# Response values on the same scale as the predictions. The formula may
# transform the response (e.g., `log(y) ~ x`); `insight::find_response()`
# returns the bare variable name, so the score would otherwise compare a
# log-scale prediction to a raw outcome.
get_conformal_response <- function(x, model) {
    response_name <- insight::find_response(model)
    response <- x[[response_name]]
    expr <- tryCatch(insight::find_terms(model)[["response"]], error = function(e) NULL)
    if (length(expr) == 1 && !identical(expr, response_name)) {
        lang <- tryCatch(str2lang(expr), error = function(e) NULL)
        if (!is.null(lang) && all(all.vars(lang) %in% colnames(x))) {
            tmp <- tryCatch(eval(lang, as.data.frame(x)), error = function(e) NULL)
            if (is.atomic(tmp) && is.null(dim(tmp)) && length(tmp) == nrow(x)) {
                response <- tmp
            }
        }
    }
    response
}
