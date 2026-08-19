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
    NULL
}


model_has_effective_offset <- function(model) {
    # The terms and call are consulted before the stored $offset component,
    # not after it: several supported classes (quantreg::rq, rms::ols) fit
    # formula offsets without ever storing an $offset element, so gating on
    # the component would skip the formula check in exactly the case it
    # exists for.
    tt <- tryCatch(stats::terms(model), error = function(e) NULL)
    if (length(attr(tt, "offset")) > 0L) {
        return(TRUE)
    }
    cl <- tryCatch(model$call, error = function(e) NULL)
    # An explicit `offset = NULL` argument is stored in the call but names no
    # offset; only a non-NULL argument disqualifies.
    if ("offset" %in% names(cl) && !is.null(cl[["offset"]])) {
        return(TRUE)
    }
    offset <- tryCatch(model[["offset"]], error = function(e) NULL)
    if (is.null(offset)) {
        return(FALSE)
    }
    !is.numeric(offset) || anyNA(offset) || any(offset != 0)
}


#' Add model matrix attribute to newdata
#' @param mfx marginaleffects object
#' @param newdata data frame to add attributes to
#' @param model model object; used only when `mfx` is unavailable
#' @keywords internal
#' @noRd
add_model_matrix_attribute <- function(mfx = NULL, newdata = NULL, model = NULL) {
    if (is.null(model)) {
        if (is.null(mfx)) {
            return(newdata)
        }
        model <- mfx@model
    }

    # predictions() only passes mfx; comparisons() passes mfx and hi/lo
    if (is.null(newdata)) {
        if (is.null(mfx)) {
            return(newdata)
        }
        newdata <- mfx@newdata
    }

    if (nrow(newdata) == 0) {
        return(newdata)
    }

    # supported models (no inheritance)
    supported <- c(
        "lm", "glm", "rq", "ols", "lrm", "ivreg", "geeglm", "svyglm",
        "negbin", "rlm", "brglmFit"
    )
    if (!isTRUE(class(model)[1] %in% supported)) {
        return(newdata)
    }

    # stats::model.matrix creates all-0 columns with splines::bs() and other functions
    funs <- grep("factor\\(|\\(Intercept|bs\\(", colnames(newdata), value = TRUE)
    if (length(funs) > 0) {
        return(newdata)
    }

    # geeglm stores an all-zero offset even without an offset term. Explicit,
    # nonzero, or malformed offsets still fall back to package prediction.
    if (model_has_effective_offset(model)) {
        return(newdata)
    }

    # subset variables for listwise deletion
    if (is.null(mfx)) {
        # Exact lm/glm retries can let their terms object select the required
        # columns directly, without propagating the full marginaleffects state
        # through every model-specific prediction method.
        nd <- as.data.frame(newdata)
    } else {
        vars <- unlist(mfx@variable_names_predictors, use.names = FALSE)
        vars <- c(vars, unlist(mfx@variable_names_response, use.names = FALSE))
        vars <- intersect(vars, colnames(newdata))
        nd <- as.data.frame(newdata)[, vars, drop = FALSE]
    }

    # This cache is optional. Model-specific matrix methods should fail closed
    # without paying the connection overhead of hush()/capture.output().
    MM <- suppressMessages(suppressWarnings(
        tryCatch(
            get_model_matrix(model, newdata = nd, mfx = mfx),
            error = function(e) NULL
        )
    ))

    # Cached consumers align rows positionally and coefficients by column name.
    # Dropping unused observation labels avoids retaining one string per row in
    # prediction Jacobians and other long-data inference objects. Every method
    # above constructs a fresh matrix, so removing dimnames by reference is safe
    # and avoids copying the complete numeric payload.
    if (is.matrix(MM)) {
        data.table::setattr(MM, "dimnames", list(NULL, colnames(MM)))
    }

    attr(newdata, "marginaleffects_model_matrix") <- MM
    return(newdata)
}


#' Attach a model matrix after `stats::predict()` failed
#'
#' `get_predict()` methods that can compute a linear predictor from
#' `X %*% beta` call this to build the model matrix on demand, rather than
#' giving up on the error raised by `stats::predict()`. When no usable matrix
#' can be built, the original error is re-raised so unrelated failures keep
#' their own message.
#'
#' @param model model object
#' @param newdata data frame to add attributes to
#' @param beta coefficient vector the matrix columns must match
#' @param error condition raised by `stats::predict()`
#' @keywords internal
#' @noRd
model_matrix_retry <- function(model, newdata, beta, error) {
    if (!isTRUE(class(model)[1] %in% c("lm", "glm"))) {
        stop(error)
    }
    newdata <- add_model_matrix_attribute(
        newdata = newdata,
        model = model
    )
    MM <- attr(newdata, "marginaleffects_model_matrix")
    if (!isTRUE(checkmate::check_matrix(MM)) || ncol(MM) != length(beta)) {
        stop(error)
    }
    return(newdata)
}
