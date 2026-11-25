#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.survreg <- function(model, coefs, ...) {
    # `coefs` holds the regression coefficients first, followed by one
    # `Log(scale)` entry per stratum (`insight::get_parameters.survreg()` builds
    # it from `summary.survreg()`). Peel the scale entries off the tail: indexing
    # by `nvar0` dropped a regression coefficient and read the scale from the
    # wrong slot.

    nvar0 <- length(model$coefficients)
    nvar <- nrow(model$var)
    if (nvar > nvar0) {
        model[["coefficients"]][] <- coefs[seq_len(nvar0)]
        model[["scale"]][] <- exp(coefs[(nvar0 + 1):nvar])
    } else {
        model$coefficients[] <- coefs
    }

    model
}

#' Design matrix that maps a coxph model's coefficients to its linear predictors
#'
#' Returns `stats::model.matrix(model)` when it has one column per coefficient
#' and one row per stored linear predictor, and `NULL` otherwise: when the model
#' frame cannot be rebuilt, when the data changed after the fit, or for
#' multistate (`coxphms`) and sparse `frailty()` fits.
#' @keywords internal
#' @noRd
coxph_design_matrix <- function(model) {
    X <- tryCatch(stats::model.matrix(model), error = function(e) NULL)
    ok <- is.matrix(X) &&
        ncol(X) == length(model[["coefficients"]]) &&
        nrow(X) == length(model[["linear.predictors"]])
    if (!ok) {
        return(NULL)
    }
    # Rows align positionally; row names would only cost memory.
    dimnames(X) <- NULL
    X
}

#' @rdname set_coef
#' @export
set_coef.coxph <- function(model, coefs, ...) {
    # `predict.coxph(type = "expected" or "survival")` estimates the baseline
    # hazard from `exp(model$linear.predictors)`, so the stored linear predictors
    # must move with the coefficients. Otherwise the Jacobian ignores how the
    # baseline hazard depends on the coefficients. They are stored as
    # `X %*% beta + offset - sum(beta * means)`, so only the change in `beta` is
    # needed. `residuals` also depend on the coefficients, but `predict.coxph()`
    # reads them only when `newdata` is missing, and `get_predict()` always
    # supplies it.
    old <- model[["coefficients"]]
    model <- set_coef.default(model, coefs)
    new <- model[["coefficients"]]

    # If `get_coef()` and `coef()` ever disagree, the name-indexed assignment
    # appends entries instead of replacing them. Do not compound that with a
    # recycled subtraction.
    if (is.null(model[["linear.predictors"]]) || length(new) != length(old)) {
        return(model)
    }
    delta <- new - old
    delta[is.na(delta)] <- 0
    if (all(delta == 0)) {
        return(model)
    }

    # `sanitize_model_specific.coxph()` caches the design matrix, because
    # `set_coef()` runs once per coefficient in the Jacobian and once per
    # draw in `inferences(method = "simulation")`. `FALSE` records that no
    # usable matrix exists. When the cache is absent (e.g., a coxph fit nested
    # inside another object, which the sanitizer never sees), rebuild it here.
    X <- attr(model, "marginaleffects_coxph_X")
    if (is.null(X)) {
        X <- coxph_design_matrix(model)
    }
    # If no matrix is available, types that read the linear predictors fail
    # inside `predict()` regardless, and "lp" and "risk" do not need the update.
    if (!is.matrix(X)) {
        return(model)
    }

    model[["linear.predictors"]] <- model[["linear.predictors"]] +
        drop(X %*% delta) - sum(delta * model[["means"]])

    model
}

#' @rdname get_coef
#' @export
get_coef.coxph.penal <- function(model, ...) {
    # `insight::get_parameters()` returns one row per term of the printed
    # summary (e.g., "linear" and "nonlin" for `pspline()`), not one per
    # coefficient.
    stats::coef(model)
}

#' @rdname get_predict
#' @export
get_predict.coxph <- function(
    model,
    newdata = get_modeldata(model),
    type = "lp",
    ...) {
    if (isTRUE(type %in% c("expected", "survival")) && model_has_effective_offset(model)) {
        # With `newdata`, `predict.coxph()` adds the offset outside `exp()` for
        # these types, which gives wrong estimates. `get_predict()` always
        # supplies `newdata`.
        msg <- sprintf(
            '`type = "%s"` is not supported for `coxph` models with an offset, because `survival::predict.coxph()` mishandles the offset when `newdata` is supplied. Use `type = "lp"` or `type = "risk"`.',
            type
        )
        stop_sprintf(msg)
    }
    if (identical(type, "survival") && inherits(model, "coxph.penal")) {
        msg <- '`type = "survival"` is not supported for penalized `coxph` models (e.g., with `ridge()` or `pspline()` terms), because `survival::predict.coxph.penal()` does not support it. Use `type = "lp"`, `"risk"`, or `"expected"`.'
        stop_sprintf(msg)
    }
    out <- stats::predict(model, newdata = newdata, type = type, ...)
    out <- data.table(estimate = out)
    out <- add_rowid(out, newdata)
    return(out)
}


#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.coxph <- function(model, vcov, ...) {
    insight::check_if_installed("survival")
    # `ridge()` and `pspline()` map one column of the design matrix to one
    # coefficient, so `coef()`, `vcov()`, and `predict()` agree. Frailty terms
    # do not: their random effects are stored outside `coef()` (sparse) or
    # mixed into it as `gamma:` coefficients, and the coefficient covariance
    # does not describe them.
    penalized <- names(model[["pterms"]])[model[["pterms"]] > 0]
    unsupported <- penalized[!grepl("^(survival::)?(ridge|pspline)\\(", penalized)]
    if (length(unsupported) > 0) {
        msg <- sprintf(
            "`marginaleffects` does not support `coxph` models with these penalized terms: %s. Supported penalized terms are `ridge()` and `pspline()`.",
            paste(sprintf("`%s`", unsupported), collapse = ", ")
        )
        stop_sprintf(msg)
    }
    flag1 <- !isFALSE(vcov)
    flag2 <- !isTRUE(checkmate::check_choice(vcov, choices = c("rsample", "boot", "fwb")))
    flag3 <- isTRUE(getOption("marginaleffects_safe", default = TRUE))
    if (flag1 && flag2 && flag3) {
        msg <- 'The default delta method standard errors for `coxph` models only take into account uncertainty in the regression coefficients. Standard errors may be too small. Use the `inferences()` function or set `vcov` to "rsample", "boot"  or "fwb" to compute confidence intervals by bootstrapping. Set `vcov` to `FALSE` or `options(marginaleffects_safe=FALSE)` to silence this warning.'
        warning(msg, call. = FALSE)
    }
    # Cache the design matrix for `set_coef.coxph()`. `FALSE` marks that none
    # is usable, so `set_coef()` does not retry on every call.
    if (is.null(attr(model, "marginaleffects_coxph_X")) &&
        !is.null(model[["linear.predictors"]])) {
        X <- coxph_design_matrix(model)
        attr(model, "marginaleffects_coxph_X") <- if (is.null(X)) FALSE else X
    }
    return(model)
}
