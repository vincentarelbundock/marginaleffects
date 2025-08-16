#' Get a named model matrix
#'
#' @inheritParams slopes
#' @rdname get_model_matrix
#' @keywords internal
#' @export
get_model_matrix <- function(model, newdata, mfx = NULL) {
    UseMethod("get_model_matrix", model)
}


#' @rdname get_model_matrix
#' @keywords internal
#' @export
get_model_matrix.default <- function(model, newdata, mfx = NULL) {
    # some models require the response variable first value only allows us to
    # handle `newdata="balanced"` and friends. This is a hack, but it probably
    # doesn't matter. There can be many response variables, so we loop.
    if (!is.null(mfx)) {
        dv <- mfx@variable_names_response
        for (d in dv) {
            if (!d %in% colnames(newdata)) {
                if (nrow(newdata) == nrow(mfx@modeldata)) {
                    newdata[[d]] <- mfx@modeldata[[d]]
                } else {
                    # For different sized newdata, use first value as placeholder
                    newdata[[d]] <- mfx@modeldata[[d]][1]
                }
            }
        }
    }

    # faster
    if (class(model)[1] %in% c("lm", "glm")) {
        out <- stats::model.matrix(model, data = newdata)
        # more general
    } else {
        out <- hush(insight::get_modelmatrix(model, data = newdata))
    }

    beta <- get_coef(model)
    if (!isTRUE(nrow(out) == nrow(newdata)) || !isTRUE(ncol(out) == length(beta))) {
        return(NULL)
    } else {
        return(out)
    }
}


#' Add model matrix attribute to newdata
#' @param mfx marginaleffects object
#' @param newdata data frame to add attributes to
#' @keywords internal
#' @noRd
add_model_matrix_attribute <- function(mfx, newdata = NULL) {
    model <- mfx@model

    # predictions() only passes mfx; comparisons() passes mfx and hi/lo
    if (is.null(newdata)) {
        newdata <- mfx@newdata
    }

    if (nrow(newdata) == 0) {
        return(newdata)
    }

    # supported models (no inheritance)
    if (!isTRUE(class(model)[1] %in% c("lm", "glm", "rq"))) {
        return(newdata)
    }

    # stats::model.matrix creates all-0 columns with splines::bs() and other functions
    # this may be too aggressive, but it avoids all functions
    funs <- grep("\\(", names(get_coef(model)), value = TRUE)
    funs <- funs[!grepl("factor\\(|\\(Intercept", funs)]
    if (length(funs) > 0) {
        return(newdata)
    }

    # we don't support offsets, so revert to stats::predict()
    if (!is.null(model[["offset"]])) {
        return(newdata)
    }

    # subset variables for listwise deletion
    vars <- unlist(
        insight::find_predictors(model, verbose = FALSE),
        use.names = FALSE
    )
    vars <- c(vars, unlist(mfx@variable_names_response, use.names = FALSE))
    vars <- intersect(vars, colnames(newdata))

    nd <- as.data.frame(newdata)[, vars, drop = FALSE]

    MM <- hush(get_model_matrix(model, newdata = nd, mfx = mfx))

    attr(newdata, "marginaleffects_model_matrix") <- MM
    return(newdata)
}
