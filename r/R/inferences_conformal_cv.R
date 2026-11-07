# CV+ conformal prediction (Barber, Candès, Ramdas & Tibshirani 2021, eq. 11).
#
# Each of the K fold models predicts the test points. Observation i in fold
# k(i) contributes its out-of-fold residual R_i and the fold-k(i) prediction
# at the test point x:
#
#   [ q^-_alpha{ mu_{-k(i)}(x) - R_i },  q^+_alpha{ mu_{-k(i)}(x) + R_i } ]
#
# where q^+ is the ceiling((1 - alpha)(n + 1))-th smallest value and q^- is
# the floor(alpha(n + 1))-th smallest. Intervals need not be symmetric around
# the full-data prediction, which is kept in `estimate` as the point
# prediction. Coverage is at least 1 - 2 * alpha - sqrt(2 / n) (Theorem 4).
#
# Classification (`softmax` score with a `group` column) uses the same
# construction: class y is in the set when fewer than (1 - alpha)(n + 1)
# out-of-fold scores are strictly smaller than the test score of y under the
# matching fold model (Romano, Sesia & Candès 2020).
conformal_cv_plus <- function(x,
                              data_train,
                              data_test,
                              R,
                              score,
                              conf_level,
                              mfx = NULL,
                              ...) {
    # point predictions on the test set from the full-data model
    out <- refit(x, newdata = data_test)
    is_group <- "group" %in% colnames(out)
    if (is_group) {
        response_name <- insight::find_response(mfx@model)
        classes <- unique(c(
            as.character(out$group),
            as.character(data_train[[response_name]])))
    }

    idx <- sample.int(nrow(data_train), nrow(data_train))
    idx <- split(idx, ceiling(seq_along(idx) / (length(idx) / R)))

    scores <- list()
    preds_test <- list()
    for (k in seq_along(idx)) {
        i <- idx[[k]]
        data_cv <- data_train[-i, , drop = FALSE]
        model_cv <- conformal_refit_fold(mfx, data_cv)

        # out-of-fold scores
        pred_cv <- conformal_predict_fold(mfx, model_cv, data_train[i, , drop = FALSE])
        if (is_group) {
            pred_cv <- conformal_complete_classes(
                pred_cv, classes, data_cv[[response_name]])
        }
        score_k <- get_conformal_score(pred_cv, score = score, mfx = mfx)
        resid_k <- attr(score_k, "residual_abs")
        scores[[k]] <- if (is.null(resid_k)) as.numeric(score_k) else resid_k

        # fold-model predictions at the test points, aligned to `out`
        pred_test <- conformal_predict_fold(mfx, model_cv, data_test)
        if (is_group) {
            pred_test <- conformal_complete_classes(
                pred_test, classes, data_cv[[response_name]])
        }
        preds_test[[k]] <- conformal_align_fold(out, pred_test, is_group)
    }

    if (is_group) {
        conformal_cv_plus_sets(out, scores, preds_test, conf_level, mfx)
    } else {
        conformal_cv_plus_intervals(out, scores, preds_test, conf_level)
    }
}


conformal_refit_fold <- function(mfx, data_cv) {
    model_cv <- tryCatch(stats::update(mfx@model, data = data_cv),
        error = function(e) NULL)
    if (is.null(model_cv)) {
        if (is.call(mfx@call_model) && "data" %in% names(mfx@call_model)) {
            mfx@call_model$data <- data_cv
            model_cv <- eval(mfx@call_model)
        } else {
            stop_sprintf("Failed to re-fit the model on the cross-validation set.")
        }
    }
    model_cv
}


# Re-evaluate the original `predictions()` call with a fold model.
conformal_predict_fold <- function(mfx, model, newdata) {
    call_cv <- mfx@call
    call_cv[["model"]] <- model
    call_cv[["newdata"]] <- newdata
    call_cv[["vcov"]] <- FALSE
    eval(call_cv)
}


# A fold can lose outcome classes. A class absent from the fold's training
# data has probability 0. A class present in training can still be missing
# from the predictions: when a multinomial model becomes binary within a fold,
# `predictions()` returns only the non-reference class, and the reference
# class has the remaining probability.
conformal_complete_classes <- function(pred, classes, response_train) {
    pred <- as.data.frame(pred)
    pred$group <- as.character(pred$group)
    missing <- setdiff(classes, unique(pred$group))
    if (length(missing) == 0) {
        return(pred)
    }
    in_train <- missing %in% unique(as.character(response_train))
    template <- pred[!duplicated(pred$rowid), , drop = FALSE]
    total <- tapply(pred$estimate, pred$rowid, sum)[as.character(template$rowid)]
    remainder <- pmax(0, 1 - as.numeric(total))
    extra <- lapply(seq_along(missing), function(j) {
        tmp <- template
        tmp$group <- missing[j]
        tmp$estimate <- if (in_train[j]) remainder / sum(in_train) else 0
        tmp
    })
    do.call(rbind, c(list(pred), extra))
}


# Fold-model test predictions in the row order of `out`.
conformal_align_fold <- function(out, pred, is_group) {
    key <- function(d) {
        if (is_group) paste(d$rowid, as.character(d$group), sep = "\r") else d$rowid
    }
    est <- pred$estimate[match(key(out), key(pred))]
    if (anyNA(est)) {
        stop_sprintf("Failed to align cross-validation predictions with the test set.")
    }
    est
}


conformal_cv_plus_intervals <- function(out, scores, preds_test, conf_level) {
    resid <- unlist(scores, use.names = FALSE)
    fold <- rep(seq_along(scores), lengths(scores))
    n <- length(resid)
    # The tolerance stops floating-point error from shifting the order
    # statistic, e.g. (1 - 0.8) * 25 = 4.999... would floor to 4.
    tol <- sqrt(.Machine$double.eps)
    k_hi <- ceiling(conf_level * (n + 1) - tol)
    k_lo <- floor((1 - conf_level) * (n + 1) + tol)
    if (k_hi > n) {
        warn_sprintf(
            "The training set has %s observations, which is too small for a %s%% CV+ conformal interval: the bounds are infinite. Use more training data or a lower confidence level.",
            n,
            100 * conf_level
        )
    }

    # rows: folds, columns: test points
    P <- do.call(rbind, preds_test)
    m <- ncol(P)
    low <- high <- rep(NA_real_, m)

    # Process test points in chunks so that the n x chunk matrix stays small.
    chunk <- max(1L, floor(1e7 / n))
    for (start in seq(1L, m, by = chunk)) {
        cols <- start:min(m, start + chunk - 1L)
        mu <- P[fold, cols, drop = FALSE]
        if (k_hi > n) {
            high[cols] <- Inf
        } else {
            high[cols] <- apply(mu + resid, 2, function(v) sort.int(v, partial = k_hi)[k_hi])
        }
        if (k_lo < 1) {
            low[cols] <- -Inf
        } else {
            low[cols] <- apply(mu - resid, 2, function(v) sort.int(v, partial = k_lo)[k_lo])
        }
    }

    out$pred.low <- low
    out$pred.high <- high
    out
}


conformal_cv_plus_sets <- function(out, scores, preds_test, conf_level, mfx) {
    response_name <- insight::find_response(mfx@model)
    n <- sum(lengths(scores))
    threshold <- conf_level * (n + 1)

    # Count out-of-fold scores strictly below each test score, fold by fold.
    count <- numeric(nrow(out))
    for (k in seq_along(scores)) {
        s_k <- sort(scores[[k]])
        test_score <- 1 - preds_test[[k]]
        count <- count + findInterval(test_score, s_k, left.open = TRUE)
    }

    keep <- out[count < threshold, , drop = FALSE]
    data.table::setDT(keep)
    keep <- keep[,
        .(pred.set = list(unique(group))),
        by = c("rowid", response_name)
    ]
    data.table::setorder(keep, rowid)
    data.table::setDF(keep)
    class(keep) <- c("predictions", class(keep))
    attr(keep, "variables_datagrid") <- response_name
    keep
}
