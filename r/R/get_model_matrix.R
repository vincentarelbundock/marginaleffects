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
    offset <- model[["offset"]]
    if (is.null(offset)) {
        return(FALSE)
    }
    tt <- tryCatch(stats::terms(model), error = function(e) NULL)
    formula_offset <- length(attr(tt, "offset")) > 0L
    call_offset <- "offset" %in% names(model$call)
    malformed_or_nonzero <-
        !is.numeric(offset) || anyNA(offset) || any(offset != 0)
    formula_offset || call_offset || malformed_or_nonzero
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
    vars <- unlist(mfx@variable_names_predictors, use.names = FALSE)
    vars <- c(vars, unlist(mfx@variable_names_response, use.names = FALSE))
    vars <- intersect(vars, colnames(newdata))

    nd <- as.data.frame(newdata)[, vars, drop = FALSE]

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
