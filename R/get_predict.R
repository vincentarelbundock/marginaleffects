#' Get predicted values from a model object (internal function)
#'
#' @return A data.frame of predicted values with a number of rows equal to the
#' number of rows in `newdata` and columns "rowid" and "predicted". A "group"
#' column is added for multivariate models or models with categorical outcomes. 
#' @rdname get_predict
#' @inheritParams marginaleffects
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

    type <- sanitize_type(model, type)
    type_base <- unname(type)
    type_insight <- names(type)

    dots <- list(...)

    # some predict methods raise warnings on unused arguments
    unused <- c("normalize_dydx", "eps", "numDeriv_method", "internal_call", "contrast_numeric_slope", "contrast_numeric", "contrast_factor")
    dots <- dots[setdiff(names(dots), unused)]

    # incompatible arguments
    if (any(c("include_smooth", "include_random") %in% names(dots)) &&
        any(c("re.form", "re_formula") %in% names(dots))) {
        stop("The `include_random` and `include_smooth` arguments can be used together, but not with `re.form` or `re_formula`.", call. = FALSE)
    }

    # should we try to compute predictions with `insight::get_predicted()`?
    # confidence interval with known `predict` argument
    is_insight <- (!isFALSE(vcov) && !is.na(type_insight)) ||
        any(c("include_random", "include_smooth") %in% names(dots))

    # `insight::get_predicted` yields back-transformed confidence intervals
    if (isTRUE(is_insight)) {
        if ("re_formula" %in% names(dots)) {
            dots[["re.form"]] <- dots[["re_formula"]]
        }

        if ("re.form" %in% names(dots)) {
            if (isTRUE(checkmate::check_formula(dots[["re.form"]]))) {
                dots[["include_random"]] <- dots[["re.form"]]
            } else if (is.na(dots[["re.form"]])) {
                dots[["include_random"]] <- FALSE
            } else {
                dots[["include_random"]] <- TRUE
            }
            dots[["re.form"]] <- NULL
        }

        args <- list(
            x = model,
            data = newdata,
            predict = type_insight,
            ci = conf_level)

        # `get_predicted` issues a warning even with `vcov=NULL` when the
        # argument is not supported, so we do this here instead of in `predictions`
        if (isTRUE(vcov == "satterthwaite") || isTRUE(vcov == "kenward-roger")) {
            args[["ci_method"]] <- vcov
            # lmerTest predict method fails when the DV is not there
            dv <- insight::find_response(model)
            newdata_tmp <- newdata
            newdata_tmp[[dv]] <- mean(insight::get_response(model))
            args[["data"]] <- newdata_tmp
        } else if (is.logical(vcov)) {
            args[["vcov"]] <- NULL
        } else if (!is.logical(vcov) && !is.null(vcov)) {
            args[["vcov"]] <- get_vcov(model, vcov = vcov)
        }

        args <- c(args, dots)

        fun <- insight::get_predicted
        pred <- try(do.call("fun", args), silent = TRUE)

        # return immediately if this worked
        if (inherits(pred, "get_predicted")) {
            out <- data.frame(pred) # cannot use data.table because insight has no as.data.table method
            if ("rowid" %in% colnames(out)) {
                out[["Row"]] <- NULL
            } else {
                colnames(out)[colnames(out) == "Row"] <- "rowid"
            }
            colnames(out)[colnames(out) == "Response"] <- "group"
            colnames(out)[colnames(out) == "SE"] <- "std.error"
            colnames(out)[colnames(out) == "Predicted"] <- "predicted"
            colnames(out)[colnames(out) == "CI_low"] <- "conf.low"
            colnames(out)[colnames(out) == "CI_high"] <- "conf.high"

            if (nrow(out) == nrow(newdata) && "rowid" %in% colnames(newdata)) {
                out$rowid <- newdata$rowid
            }

            out <- sort_columns(out, first = c("rowid", "group", "predicted"))
            return(out)
        }
    }

    # `stats::predict` is faster than `insight::get_predicted`
    # first argument in the predict methods is not always named "x" or "model"
    dots[["newdata"]] <- newdata
    dots[["type"]] <- type_base
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
            if ("rowid" %in% colnames(newdata)) {
                out <- data.table(predicted = as.numeric(pred),
                                  rowid = newdata$rowid)
            } else {
                out <- data.table(predicted = as.numeric(pred),
                                  rowid = seq_len(length(pred)))
            }
        }

    # matrix with outcome levels as columns
    } else if (is.matrix(pred)) {
        # internal calls always includes "rowid" as a column in `newdata`
        if ("rowid" %in% colnames(newdata)) {
            out <- data.table(
                rowid = rep(newdata[["rowid"]], times = ncol(pred)),
                group = rep(colnames(pred), each = nrow(pred)),
                predicted = c(pred))
        } else {
            out <- data.table(
                rowid = rep(seq_len(nrow(pred)), times = ncol(pred)),
                group = rep(colnames(pred), each = nrow(pred)),
                predicted = c(pred))
        }
    } else {
        stop(sprintf("Unable to extract predictions of type %s from a model of class %s. Please report this problem, along with reproducible code and data on Github: https://github.com/vincentarelbundock/marginaleffects/issues", type, class(model)[1]), call. = FALSE)
    }

    setDF(out)

    out <- sort_columns(out, first = c("rowid", "group", "predicted"))

    return(out)
}
