#' Get predicted values from a model object (internal function)
#'
#' @return A data.frame of predicted values with a number of rows equal to the
#' number of rows in `newdata` and columns "rowid" and "estimate". A "group"
#' column is added for multivariate models or models with categorical outcomes.
#' @rdname get_predict
#' @inheritParams slopes
#' @keywords internal
#' @export
get_predict <- function(
  model,
  newdata,
  type,
  mfx = NULL,
  newparams = NULL,
  ndraws = NULL,
  se.fit = NULL,
  ...
) {
    UseMethod("get_predict", model)
}


#' @rdname get_predict
#' @export
get_predict.default <- function(
  model,
  newdata = insight::get_data(model),
  type = "response",
  mfx = NULL,
  newparams = NULL,
  ndraws = NULL,
  se.fit = NULL,
  ...
) {
    dots <- list(...)

    if (is.null(type)) {
        type <- sanitize_type(model = model, type = type)
    }

    # some predict methods raise warnings on unused arguments
    unused <- c(
        "normalize_dydx",
        "eps",
        "numDeriv_method",
        "internal_call",
        "draw",
        "modeldata",
        "flag",
        "marginaleffects",
        "mfx"
    )
    dots <- dots[setdiff(names(dots), unused)]

    # first argument in the predict methods is not always named "x" or "model"
    dots[["newdata"]] <- newdata
    dots[["type"]] <- type
    if (!is.null(newparams)) {
        dots[["newparams"]] <- newparams
    }
    args <- c(list(model), dots)

    # `pred` is a secret argument called by `predict.lm` to turn a numeric vector into a data frame with correct `rowid`
    if ("pred" %in% ...names()) {
        pred <- dots[["pred"]]
    } else {
        fun <- stats::predict
        pred <- suppressWarnings(safe_do_call(fun, args))
    }

    # 1-d array to vector (e.g., {mgcv})
    if (is.array(pred) && length(dim(pred)) == 1) {
        pred <- as.vector(pred)
    }

    # 1-d array to vector (e.g., Gam from {gam})
    if (
        is.array(pred) &&
            length(dim(pred)) == 3 &&
            dim(pred)[1] == 1 &&
            dim(pred)[2] == 1 &&
            dim(pred)[3] > 1
    ) {
        pred <- as.vector(pred)
    }

    # phylolm
    if (isTRUE(checkmate::check_matrix(pred, ncols = 1))) {
        pred <- drop(pred)
    }

    # atomic vector
    if (isTRUE(checkmate::check_atomic_vector(pred))) {
        # strip weird attributes added by some methods (e.g., predict.svyglm)
        # as.numeric is slow with large objects and we can't use is.numeric
        # to run it conditionally because objects of class "svystat" are
        # already numeric
        class(pred) <- "numeric"
        out <- data.frame(estimate = pred)

        # matrix with outcome levels as columns
    } else if (is.matrix(pred)) {
        if (is.null(colnames(pred))) {
            colnames(pred) <- seq_len(ncol(pred))
        }
        # internal calls always includes "rowid" as a column in `newdata`
        out <- data.frame(
            group = rep(colnames(pred), each = nrow(pred)),
            estimate = c(pred)
        )
        out$group <- group_to_factor(out$group, model)
    } else {
        stop(
            sprintf(
                "Unable to extract predictions of type %s from a model of class %s. Please report this problem, along with reproducible code and data on Github: https://github.com/vincentarelbundock/marginaleffects/issues",
                type,
                class(model)[1]
            ),
            call. = FALSE
        )
    }

    out <- add_rowid(out, newdata)

    return(out)
}


get_predict_error <- function(model, newdata = NULL, ...) {
    pred_result <- myTryCatch(get_predict(model, newdata = newdata, ...))

    if (inherits(pred_result$value, "data.frame")) {
        out <- pred_result$value
        return(out)
    } else {
        if (
            inherits(pred_result$error, "rlang_error") &&
                isTRUE(grepl("the object should be", pred_result$error$message))
        ) {
            stop_sprintf(pred_result$error$message)
        }

        msg <- "Unable to compute predicted values with this model. This error can arise when `insight::get_data()` is unable to extract the dataset from the model object, or when the original data frame (usually in the global environment) was modified since fitting the model. Make sure the original data object is unchanged and/or try to supply a different dataset to the `newdata` argument."
        if (!is.null(pred_result$error)) {
            msg <- c(
                msg,
                "\n\n",
                "In addition, this error message was raised:",
                "\n",
                pred_result$error$message
            )
        }
        msg <- c(
            msg,
            "\n\n",
            "Bug Tracker: https://github.com/vincentarelbundock/marginaleffects/issues"
        )
        stop_sprintf(msg)
    }
}
