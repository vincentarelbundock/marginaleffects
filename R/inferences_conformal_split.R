conformal_split <- function(x, test, calibration, score, conf_level, mfx = NULL, ...) {
    # calibration
    # use original model---fitted on the training set---to make predictions in the calibration set
    p_calib <- refit(x, newdata = calibration, vcov = FALSE)
    score <- get_conformal_score(p_calib, score = score, mfx = mfx)

    # test
    # use original model to make predictions in the test set
    p_test <- refit(x, newdata = test, vcov = FALSE)

    # bounds
    out <- get_conformal_bounds(p_test, score = score, conf_level = conf_level, mfx = mfx)

    return(out)
}
