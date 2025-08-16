get_conformal_score <- function(x, score, mfx = NULL) {
    if (is.null(mfx)) {
        mfx <- attr(x, "marginaleffects")
    }
    model <- mfx@model
    response_name <- insight::find_response(model)
    response <- x[[response_name]]
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
    d <- min(resid[score > stats::quantile(score, probs = conf_level)])
    if ("group" %in% colnames(x)) {
        q <- stats::quantile(
            score,
            probs = (length(score) + 1) * conf_level / length(score)
        )
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


conformal_cv_plus <- function(x, test, R, score, conf_level, mfx = NULL, ...) {
    # assertions
    checkmate::assert_class(x, "predictions")
    checkmate::assert_choice(
        score,
        choices = c("residual_abs", "residual_sq", "softmax")
    )
    checkmate::assert_data_frame(test, null.ok = FALSE)
    checkmate::assert_integerish(R, upper = 25)

    # cross-validation
    train <- mfx@modeldata
    idx <- sample.int(nrow(train), nrow(train))
    idx <- split(idx, ceiling(seq_along(idx) / (length(idx) / R)))
    scores <- NULL
    for (i in idx) {
        data_cv <- train[-i, ]
        # re-fit the original model on training sets withholding the CV fold
        model_cv <- stats::update(mfx@model, data = data_cv)
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
    out <- mfx@call
    out[["newdata"]] <- test
    out <- eval(out)

    # bounds
    out <- get_conformal_bounds(out, score = scores, conf_level = conf_level, mfx = mfx)

    return(out)
}
