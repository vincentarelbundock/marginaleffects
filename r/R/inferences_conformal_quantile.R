# Conformalized Quantile Regression (CQR)
#
# This implementation is based on the split conformal inference method described by
# Romano et al. (2019): "Conformalized quantile regression." Advances in neural
# information processing systems 32.
#
# Reference implementation from the `probably` package (MIT License, 2025-11-10):
# Copyright holders: Max Kuhn [aut, cre], Davis Vaughan [aut], Edgar Ruiz [aut],
# Posit Software, PBC [cph, fnd] (ROR ID: https://ror.org/00cvxb145)
# URL: https://github.com/tidymodels/probably
#
# The implementation here is adapted to fit the marginaleffects package API and
# uses quantreg::rq instead of quantregForest for quantile estimation.

conformal_quantile <- function(x, data_test, data_train, data_calib, conf_level = 0.95,
                               mfx = NULL, ...) {
    if (is.null(mfx)) {
        mfx <- attr(x, "marginaleffects")
    }

    # Get model information
    model <- mfx@model
    response_name <- insight::find_response(model)

    # Check response is numeric
    if (!is.numeric(data_train[[response_name]])) {
        stop_sprintf("Quantile conformal inference requires a numeric response variable.")
    }

    # Check calibration has response
    if (!response_name %in% colnames(data_calib)) {
        stop_sprintf(
            "Calibration data must include the response variable '%s'.",
            response_name
        )
    }

    # Step 1: Fit quantile regression forest on training data
    insight::check_if_installed("quantregForest", reason = "to fit quantile regression forest")

    # Compute quantile levels for CQR
    # For coverage (1-alpha), use quantiles at alpha/2 and 1-alpha/2
    # e.g., for 90% coverage (conf_level=0.9): alpha=0.1, quantiles at 0.05 and 0.95
    alpha <- 1 - conf_level

    # Determine predictor columns (exclude response)
    # Use the order from training data consistently across all datasets
    predictor_cols <- setdiff(colnames(data_train), response_name)

    # Prepare predictor matrix (exclude response)
    # Convert to data.frame to avoid data.table subsetting issues
    x_train <- as.data.frame(data_train)[, predictor_cols, drop = FALSE]
    y_train <- data_train[[response_name]]

    # Filter out arguments not relevant to quantregForest::quantregForest
    dots <- list(...)
    # Remove arguments from inferences() that aren't for quantregForest()
    dots <- dots[!names(dots) %in% c("R", "conf_type", "estimator", "score")]

    # Fit quantile regression forest
    qrf <- tryCatch(
        do.call(
            quantregForest::quantregForest,
            c(list(x = x_train, y = y_train), dots)
        ),
        error = function(e) {
            stop_sprintf("Failed to fit quantile regression forest: %s", conditionMessage(e))
        })

    # Step 2: Get quantile predictions on calibration set
    # Use same predictor columns in same order as training
    x_calib <- as.data.frame(data_calib)[, predictor_cols, drop = FALSE]
    q_calib <- stats::predict(qrf, newdata = x_calib, what = c(alpha / 2, 1 - alpha / 2))
    q_calib_low <- q_calib[, 1]
    q_calib_high <- q_calib[, 2]

    # Step 3: Compute conformity scores on calibration set
    # E_i = max(q_low(X_i) - Y_i, Y_i - q_high(X_i))
    y_calib <- data_calib[[response_name]]
    conformity_scores <- pmax(
        q_calib_low - y_calib,
        y_calib - q_calib_high
    )

    # Step 4: Compute inflated empirical quantile
    # Following Romano et al. (2019) and reference implementations:
    # Quantiles at alpha/2 and 1-alpha/2, conformity quantile at (1-alpha)
    # Reference: conformal_quantile_reference.R line 142
    m <- length(conformity_scores)
    k <- ceiling((1 - alpha) * (m + 1))
    k <- min(max(k, 1), m) # clamp to [1, m]

    # Get the k-th smallest score
    Q <- sort(conformity_scores, partial = k)[k]

    # Step 5: Get quantile predictions on test set
    # Use same predictor columns in same order as training
    x_test <- as.data.frame(data_test)[, predictor_cols, drop = FALSE]
    q_test <- stats::predict(qrf, newdata = x_test, what = c(alpha / 2, 1 - alpha / 2))
    q_test_low <- q_test[, 1]
    q_test_high <- q_test[, 2]

    # Step 6: Inflate the quantile predictions
    # C(x*) = [q_low(x*) - Q, q_high(x*) + Q]
    out <- refit(x, newdata = data_test, vcov = FALSE)

    out$pred.low <- q_test_low - Q
    out$pred.high <- q_test_high + Q

    return(out)
}
