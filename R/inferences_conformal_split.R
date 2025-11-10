conformal_split <- function(x, test, calibration, score, conf_level, mfx = NULL, ...) {
    # assertions
    checkmate::assert_class(x, "predictions")
    checkmate::assert_choice(
        score,
        choices = c("residual_abs", "residual_sq", "softmax")
    )
    checkmate::assert_data_frame(test, null.ok = FALSE)
    checkmate::assert_data_frame(calibration, null.ok = FALSE)

    # calibration
    # use original model---fitted on the training set---to make predictions in the calibration set
    # p_calib is the `predictions()` call, which we re-evaluate on newdata=calibration
    p_calib <- mfx@call
    p_calib[["newdata"]] <- calibration
    p_calib[["vcov"]] <- FALSE # faster
    p_calib <- eval(p_calib)
    score <- get_conformal_score(p_calib, score = score, mfx = mfx)

    # test
    # use original model to make predictions in the test set
    p_test <- mfx@call
    p_test[["newdata"]] <- test
    p_test <- eval(p_test)

    # bounds
    out <- get_conformal_bounds(p_test, score = score, conf_level = conf_level, mfx = mfx)

    return(out)
}
