#' @rdname get_group_names
#' @export
get_group_names.nestedLogit <- function(model, type, ...) {
    out <- get_predict(model, type = type, ...)
    if ("group" %in% colnames(out)) {
        out <- unique(out$group)
    } else {
        out <- "main_marginaleffects"
    }
    return(out)
}


#' @rdname get_predict
#' @export
get_predict.nestedLogit <- function(
  model,
  newdata = get_modeldata(model),
  type = "response",
  mfx = NULL,
  newparams = NULL,
  ndraws = NULL,
  se.fit = NULL,
  ...,
  submodel = NULL
) {
    type <- sanitize_type(model, type)
    if (is.null(submodel)) {
        submodel <- "nested"
    }

    pred <- stats::predict(
        model,
        newdata = newdata,
        model = submodel,
        ...
    )
    pred <- as.data.frame(pred)

    estimate_col <- switch(type,
        "response" = "p",
        "link" = "logit"
    )

    if (!estimate_col %in% colnames(pred)) {
        msg <- sprintf(
            'The predicted values do not include a "%s" column. When using `submodel = "%s"`, only `type = "link"` is available.',
            estimate_col, submodel
        )
        stop(msg, call. = FALSE)
    }

    n_groups <- length(unique(pred$response))
    out <- data.table::data.table(
        group = pred$response,
        estimate = pred[[estimate_col]],
        rowid = rep(newdata$rowid, each = n_groups)
    )
    out$group <- group_to_factor(out$group, model)
    return(out)
}


#' @rdname get_coef
#' @export
get_coef.nestedLogit <- function(model, ...) {
    mat <- stats::coef(model, as.matrix = TRUE)
    out <- as.vector(mat)
    names(out) <- paste0(
        rep(colnames(mat), each = nrow(mat)),
        ".",
        rep(rownames(mat), times = ncol(mat))
    )
    return(out)
}


#' @rdname get_vcov
#' @export
get_vcov.nestedLogit <- function(model, vcov = NULL, ...) {
    if (isFALSE(vcov)) {
        return(NULL)
    }
    if (isTRUE(checkmate::check_flag(vcov))) {
        vcov <- NULL
    }
    if (!is.null(vcov)) {
        return(sanitize_vcov(model, vcov))
    }
    stats::vcov(model, as.matrix = TRUE)
}


#' @rdname set_coef
#' @export
set_coef.nestedLogit <- function(model, coefs, ...) {
    for (nm in names(model$models)) {
        prefix <- paste0(nm, ".")
        idx <- startsWith(names(coefs), prefix)
        if (!any(idx)) next
        subcoefs <- coefs[idx]
        names(subcoefs) <- sub(prefix, "", names(subcoefs), fixed = TRUE)
        model$models[[nm]] <- set_coef.glm(model$models[[nm]], subcoefs)
    }
    return(model)
}
