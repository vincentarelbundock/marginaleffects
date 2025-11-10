conformal_cv_plus <- function(x, test, R, score, conf_level, mfx = NULL, ...) {
    # cross-validation
    train <- mfx@modeldata
    idx <- sample.int(nrow(train), nrow(train))
    idx <- split(idx, ceiling(seq_along(idx) / (length(idx) / R)))
    scores <- NULL
    for (i in idx) {
        data_cv <- train[-i, ]
        # re-fit the original model on training sets withholding the CV fold
        model_cv <- tryCatch(stats::update(mfx@model, data = data_cv),
            error = function(e) NULL)
        if (is.null(model_cv)) {
            if (is.call(mfx@call_model) && "data" %in% names(mfx@call_model)) {
                # if the model call has a data argument, we can update it
                mfx@call_model$data <- data_cv
                model_cv <- eval(mfx@call_model)
            } else {
                stop_sprintf("Failed to re-fit the model on the cross-validation set.")
            }
        }
        # use the updated model to make out-of-fold predictions
        # call_cv is the `predictions()` call, which we re-evaluate in-fold: newdata=train[i,]
        call_cv <- mfx@call
        call_cv[["model"]] <- model_cv
        call_cv[["newdata"]] <- train[i, ]
        call_cv[["vcov"]] <- FALSE # faster
        pred_cv <- eval(call_cv)
        # save the scores form each fold
        scores <- c(scores, get_conformal_score(pred_cv, score = score, mfx = mfx))
    }

    # test
    out <- refit(x, newdata = test)

    # bounds
    out <- get_conformal_bounds(out, score = scores, conf_level = conf_level, mfx = mfx)

    return(out)
}
