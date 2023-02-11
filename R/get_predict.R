#' Get predicted values from a model object (internal function)
#'
#' @return A data.frame of predicted values with a number of rows equal to the
#' number of rows in `newdata` and columns "rowid" and "estimate". A "group"
#' column is added for multivariate models or models with categorical outcomes.
#' @rdname get_predict
#' @inheritParams slopes
#' @keywords internal
#' @export
get_predict <- function(model, newdata, vcov, conf_level, type, ...) {
    UseMethod("get_predict", model)
}


#' @rdname get_predict
#' @export
get_predict.default <- function(model,
                                newdata = insight::get_data(model),
                                vcov = FALSE,
                                conf_level = 0.95,
                                type = "response",
                                ...) {


    dots <- list(...)

    # some predict methods raise warnings on unused arguments
    unused <- c("normalize_dydx", "eps", "numDeriv_method", "internal_call", "contrast_numeric_slope", "contrast_numeric", "contrast_factor", "draw")
    dots <- dots[setdiff(names(dots), unused)]

    # incompatible arguments
    if (any(c("include_smooth", "include_random") %in% names(dots)) &&
        any(c("re.form", "re_formula") %in% names(dots))) {
        stop("The `include_random` and `include_smooth` arguments can be used together, but not with `re.form` or `re_formula`.", call. = FALSE)
    }

    # first argument in the predict methods is not always named "x" or "model"
    dots[["newdata"]] <- newdata
    dots[["type"]] <- type
    args <- c(list(model), dots)

    fun <- stats::predict
    pred <- suppressWarnings(do.call("fun", args))

    # 1-d array to vector (e.g., {mgcv})
    if (is.array(pred) && length(dim(pred)) == 1) {
        pred <- as.vector(pred)
    }

    # 1-d array to vector (e.g., Gam from {gam})
    if (is.array(pred) &&
        length(dim(pred)) == 3 &&
        dim(pred)[1] == 1 &&
        dim(pred)[2] == 1 &&
        dim(pred)[3] > 1) {
        pred <- as.vector(pred)
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
                out <- list(estimate = pred,
                                  rowid = newdata$rowid)
            } else {
                out <- list(estimate = pred,
                                  rowid = seq_len(length(pred)))
            }
        }

    # matrix with outcome levels as columns
    } else if (is.matrix(pred)) {
        # internal calls always includes "rowid" as a column in `newdata`
        if ("rowid" %in% colnames(newdata)) {
            out <- list(
                rowid = rep(newdata[["rowid"]], times = ncol(pred)),
                group = rep(colnames(pred), each = nrow(pred)),
                estimate = c(pred))
        } else {
            out <- list(
                rowid = rep(seq_len(nrow(pred)), times = ncol(pred)),
                group = rep(colnames(pred), each = nrow(pred)),
                estimate = c(pred))
        }
    } else {
        stop(sprintf("Unable to extract predictions of type %s from a model of class %s. Please report this problem, along with reproducible code and data on Github: https://github.com/vincentarelbundock/marginaleffects/issues", type, class(model)[1]), call. = FALSE)
    }

    setDF(out)

    out <- sort_columns(out, first = c("rowid", "group", "estimate"))

    return(out)
}
