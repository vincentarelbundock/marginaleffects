# Full conformal prediction
#
# WARNING: This method is computationally expensive as it refits the model
# for each combination of test observation and trial value. It is typically
# several orders of magnitude slower than split or CV+ conformal methods.
# Use sparingly on small test sets or consider conformal_split/conformal_cv+ instead.

# Helper function to estimate trial value bounds for optimization
get_trial_bounds_conformal <- function(pred_value, train_residuals, multiplier = 10) {
    # Simple quantile-based approach for bounds
    # Use range of training residuals as guide
    resid_range <- max(abs(train_residuals))
    bound <- multiplier * resid_range

    return(c(pred_value - bound, pred_value + bound))
}


# Conformity test for a single trial value - returns difference for uniroot
conformity_test_full <- function(trial_y, x_test, model, train_data, response_name,
                                  conf_level, mfx) {
    # Augment training data with test point and trial outcome
    x_test[[response_name]] <- trial_y

    # Ensure x_test has same columns as train_data in same order
    # Reorder columns to match train_data
    x_test <- x_test[, colnames(train_data), drop = FALSE]

    # Ensure all column types in x_test match train_data before rbind
    # This prevents type coercion issues with recipes/workflows
    for (col in colnames(train_data)) {
        target_class <- class(train_data[[col]])[1]
        current_class <- class(x_test[[col]])[1]
        if (target_class != current_class) {
            x_test[[col]] <- methods::as(x_test[[col]], target_class)
        }
    }

    augmented_data <- rbind(train_data, x_test)
    n <- nrow(train_data)

    # Refit model and get predictions using refit.predictions()
    # This handles workflows, stats::update, and call re-evaluation
    pred_aug <- tryCatch({
        # Create a minimal predictions object to use refit() on
        # We need the mfx attribute for refit() to work
        dummy_pred <- data.frame(estimate = 1)
        class(dummy_pred) <- c("predictions", "data.frame")
        attr(dummy_pred, "marginaleffects") <- mfx

        # Use refit() to refit model and make predictions
        refit.predictions(dummy_pred, data = augmented_data, newdata = augmented_data)
    }, error = function(e) {
        if (getOption("marginaleffects_conformal_debug", FALSE)) {
            warning("Refit/prediction failed: ", conditionMessage(e))
        }
        NULL
    })

    if (is.null(pred_aug) || !inherits(pred_aug, "predictions")) {
        return(NA_real_)
    }

    # Compute absolute residuals for all n+1 points
    residuals <- abs(augmented_data[[response_name]] - pred_aug$estimate)

    # Get residual for new point (last observation)
    resid_new <- residuals[n + 1]

    # Compute quantile of training residuals (excluding new point)
    quantile_val <- stats::quantile(residuals[-length(residuals)], probs = conf_level)

    # Return difference for root finding
    # When this crosses zero, we've found the boundary
    return(resid_new - unname(quantile_val))
}


conformal_full <- function(x, test, score = "residual_abs", conf_level = 0.95,
                           var_multiplier = 10, max_iter = 100,
                           tolerance = .Machine$double.eps^0.25,
                           parallel = FALSE, mfx = NULL, train = NULL, ...) {
    # Assertions
    checkmate::assert_class(x, "predictions")
    checkmate::assert_choice(
        score,
        choices = c("residual_abs", "residual_sq")
    )
    checkmate::assert_data_frame(test, null.ok = FALSE)

    if (is.null(mfx)) {
        mfx <- attr(x, "marginaleffects")
    }

    # Get model
    model <- mfx@model
    response_name <- insight::find_response(model)

    # Get training data
    if (is.null(train)) {
        train_data <- mfx@modeldata
    } else {
        train_data <- train
    }

    # Check that training data has predictors (more than just response)
    if (ncol(train_data) <= 1) {
        stop_sprintf(
            "Full conformal inference requires the original training data with predictors.
For tidymodels workflows, pass the training data explicitly:
  predictions(mod, newdata = test_data) |> inferences(method = 'conformal_full', conformal_train = train_data)"
        )
    }

    # Check response is numeric
    if (!is.numeric(train_data[[response_name]])) {
        stop_sprintf("Full conformal inference requires a numeric response variable.")
    }

    # Get initial predictions and training residuals (for bounds estimation)
    pred_test <- predictions(model, newdata = test, vcov = FALSE)
    pred_train <- predictions(model, newdata = train_data, vcov = FALSE)
    train_residuals <- train_data[[response_name]] - pred_train$estimate

    # Function to compute bounds for a single test observation
    compute_bounds_single <- function(i) {
        x_test <- test[i, , drop = FALSE]
        data.table::setDF(x_test)  # Convert to data.frame for easier handling
        # Remove marginaleffects-added columns like rowid
        keep_cols <- !colnames(x_test) %in% c("rowid", "rowidcf")
        if (any(keep_cols)) {
            x_test <- x_test[, keep_cols, drop = FALSE]
        }
        pred_value <- pred_test$estimate[i]

        # Get trial bounds
        bounds <- get_trial_bounds_conformal(pred_value, train_residuals, var_multiplier)

        # Find upper bound using uniroot
        upper <- tryCatch(
            stats::uniroot(
                conformity_test_full,
                c(pred_value, bounds[2]),
                maxiter = max_iter,
                tol = tolerance,
                extendInt = "upX",
                x_test = x_test,
                model = model,
                train_data = train_data,
                response_name = response_name,
                conf_level = conf_level,
                mfx = mfx
            ),
            error = function(e) {
                insight::format_warning(sprintf(
                    "Failed to find upper bound for observation %d: %s", i, conditionMessage(e)
                ))
                NULL
            }
        )

        # Find lower bound using uniroot
        lower <- tryCatch(
            stats::uniroot(
                conformity_test_full,
                c(bounds[1], pred_value),
                maxiter = max_iter,
                tol = tolerance,
                extendInt = "downX",
                x_test = x_test,
                model = model,
                train_data = train_data,
                response_name = response_name,
                conf_level = conf_level,
                mfx = mfx
            ),
            error = function(e) {
                insight::format_warning(sprintf(
                    "Failed to find lower bound for observation %d: %s", i, conditionMessage(e)
                ))
                NULL
            }
        )

        pred_low <- if (!is.null(lower)) lower$root else NA_real_
        pred_high <- if (!is.null(upper)) upper$root else NA_real_

        return(c(pred.low = pred_low, pred.high = pred_high))
    }

    # Compute bounds for all test observations
    if (parallel) {
        # Use future for parallelization if available
        if (requireNamespace("furrr", quietly = TRUE)) {
            results <- furrr::future_map(
                seq_len(nrow(test)),
                compute_bounds_single,
                .options = furrr::furrr_options(seed = TRUE)
            )
        } else {
            insight::format_warning("Package 'furrr' not available. Running sequentially.")
            results <- lapply(seq_len(nrow(test)), compute_bounds_single)
        }
    } else {
        results <- lapply(seq_len(nrow(test)), compute_bounds_single)
    }

    # Combine results
    bounds_df <- do.call(rbind, lapply(results, function(x) {
        data.frame(pred.low = x["pred.low"], pred.high = x["pred.high"])
    }))

    # Add bounds to output
    pred_test$pred.low <- bounds_df$pred.low
    pred_test$pred.high <- bounds_df$pred.high

    return(pred_test)
}
