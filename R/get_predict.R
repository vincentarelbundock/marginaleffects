#' Get predicted values from a model object (internal function)
#'
#' @return A data.frame of predicted values with a number of rows equal to the
#' number of rows in `newdata` and columns "rowid" and "estimate". A "group"
#' column is added for multivariate models or models with categorical outcomes.
#' @rdname get_predict
#' @inheritParams slopes
#' @keywords internal
#' @export
get_predict <- function(model, newdata, type, ...) {
    UseMethod("get_predict", model)
}


#' @rdname get_predict
#' @export
get_predict.default <- function(
    model,
    newdata = insight::get_data(model),
    type = "response",
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
        "flag"
    )
    dots <- dots[setdiff(names(dots), unused)]

    # first argument in the predict methods is not always named "x" or "model"
    dots[["newdata"]] <- newdata
    dots[["type"]] <- type
    args <- c(list(model), dots)

    # `pred` is a secret argument called by `predict.lm` to turn a numeric vector into a data frame with correct `rowid`
    if ("pred" %in% names(dots)) {
        pred <- dots[["pred"]]
    } else {
        fun <- stats::predict
        pred <- suppressWarnings(do.call(fun, args))
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
        if (length(pred) == nrow(newdata)) {
            # as.numeric is slow with large objects and we can't use is.numeric
            # to run it conditionally because objects of class "svystat" are
            # already numeric
            class(pred) <- "numeric"
            if ("rowid" %in% colnames(newdata)) {
                out <- list(
                    rowid = newdata$rowid,
                    estimate = pred
                )
            } else {
                out <- list(rowid = seq_len(length(pred)), estimate = pred)
            }
        }

        # matrix with outcome levels as columns
    } else if (is.matrix(pred)) {
        if (is.null(colnames(pred))) {
            colnames(pred) <- seq_len(ncol(pred))
        }
        # internal calls always includes "rowid" as a column in `newdata`
        if ("rowid" %in% colnames(newdata)) {
            out <- list(
                rowid = rep(newdata[["rowid"]], times = ncol(pred)),
                group = rep(colnames(pred), each = nrow(pred)),
                estimate = c(pred)
            )
        } else {
            out <- list(
                rowid = rep(seq_len(nrow(pred)), times = ncol(pred)),
                group = rep(colnames(pred), each = nrow(pred)),
                estimate = c(pred)
            )
        }
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

    data.table::setDF(out)

    return(out)
}
