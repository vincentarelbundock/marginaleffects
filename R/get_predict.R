#' Get predicted values from a model object (internal function)
#'
#' @return A vector of predicted values of length equal to the number of rows
#' in `newdata`. For models with multi-level outcomes (e.g., multinomial
#' logit), this function returns a matrix of predicted values with column names
#' equal to each of the levels/groups.
#' @rdname get_predict
#' @inheritParams marginaleffects
#' @keywords internal
#' @export
get_predict <- function(model, newdata, type, ...) {
    UseMethod("get_predict", model)
}


#' @rdname get_predict
#' @export
get_predict.default <- function(model,
                                newdata = insight::get_data(model),
                                type = "response",
                                conf.level = NULL,
                                ...) {


    type <- sanity_type(model, type)
    type_base <- unname(type)
    type_insight <- names(type)

    dots <- list(...)

    # some predict methods raise warnings on unused arguments 
    unused <- c("normalize_dydx", "step_size", "numDeriv_method")
    dots <- dots[setdiff(names(dots), unused)]

    # `insight::get_predicted` yields back-transformed confidence intervals
    if (!is.na(type_insight) && (!is.null(conf.level) || "include_random" %in% names(dots))) {
        if ("include_random" %in% names(dots)) {
            if (any(c("re.form", "re_formula") %in% names(dots))) {
                stop("The `include_random` and `re.form` (or `re_formula`) arguments cannot be used together.")
            }
        }

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
            ci = conf.level)

        args <- c(args, dots)

        f <- insight::get_predicted
        pred <- try(do.call("f", args), silent = TRUE)

        # return immediately if this worked
        if (inherits(pred, "get_predicted")) {
            out <- data.frame(pred)
            colnames(out)[colnames(out) == "Row"] <- "rowid"
            colnames(out)[colnames(out) == "Response"] <- "group"
            colnames(out)[colnames(out) == "SE"] <- "std.error"
            colnames(out)[colnames(out) == "Predicted"] <- "predicted"
            if (!"rowid" %in% colnames(out)) {
                out$rowid <- seq_len(nrow(out))
            }
            return(out)
        }
    }

    # `stats::predict` is faster than `insight::get_predicted`
    # first argument in the predict methods is not always named "x" or "model"
    dots[["newdata"]] <- newdata
    dots[["type"]] <- type_base
    args <- c(list(model), dots)

    fun <- stats::predict
    pred <- do.call("fun", args)

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
        out <- data.frame(
            rowid = 1:nrow(newdata),
            # strip weird attributes added by some methods (e.g., predict.svyglm)
            predicted = as.numeric(pred))

    # matrix with outcome levels as columns
    } else if (is.matrix(pred)) {
        out <- data.frame(
            rowid = rep(1:nrow(pred), times = ncol(pred)),
            group = rep(colnames(pred), each = nrow(pred)),
            predicted = c(pred))

    } else {
        stop(sprintf("Unable to extract predictions of type %s from a model of class %s. Please report this problem, along with reproducible code and data on Github: https://github.com/vincentarelbundock/marginaleffects/issues", type, class(model)[1]))
    }

    return(out)
}
