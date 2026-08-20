#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.survreg <- function(model, coefs, ...) {
    # Reverse engineering insight::get_get_parameters.survreg(),
    # which uses summary.survreg()

    nvar0 <- length(model$coefficients)
    nvar <- nrow(model$var)
    if (nvar > nvar0) {
        model[["coefficients"]][] <- coefs[-nvar0]
        model[["scale"]][] <- exp(coefs[nvar0])
    } else {
        model$coefficients[] <- coefs
    }

    model
}

#' @rdname get_predict
#' @export
get_predict.coxph <- function(
    model,
    newdata = get_modeldata(model),
    type = "lp",
    ...) {
    out <- stats::predict(model, newdata = newdata, type = type, ...)
    out <- data.table(estimate = out)
    out <- add_rowid(out, newdata)
    return(out)
}


#' @rdname get_predict
#' @export
get_predict.clogit <- function(
    model,
    newdata = get_modeldata(model),
    type = "choice",
    ...) {
    if (!identical(type, "choice")) {
        return(get_predict.coxph(model, newdata = newdata, type = type, ...))
    }

    # conditional logit: P(alternative j) = softmax of the linear predictor
    # within each stratum. `predict.coxph(type = "expected")` only equals this
    # at the observed data, because the Breslow baseline stays frozen.
    lp <- as.numeric(stats::predict(model, newdata = newdata, type = "lp"))

    tt <- model[["terms"]]
    idx <- attr(tt, "specials")[["strata"]]
    svars <- unlist(lapply(idx, function(i) all.vars(attr(tt, "variables")[[i + 1]])))
    miss <- setdiff(svars, colnames(newdata))
    if (length(miss) > 0) {
        stop_sprintf(
            'Cannot compute `type = "choice"`: stratum variable(s) %s missing from `newdata`.',
            paste(sprintf("`%s`", miss), collapse = ", ")
        )
    }

    # `comparisons()` row-binds the grids of several terms into one call, which
    # repeats each stratum. Split on `term`/`contrast` too, when present, so the
    # softmax denominator never spans more than one copy of a choice set.
    keys <- c(svars, intersect(c("term", "contrast"), colnames(newdata)))
    grp <- interaction(as.data.frame(newdata)[keys], drop = TRUE, lex.order = TRUE)

    e <- exp(lp)
    out <- as.numeric(stats::ave(e, grp, FUN = function(z) z / sum(z)))
    out <- data.table(estimate = out)
    out <- add_rowid(out, newdata)
    return(out)
}


#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.coxph <- function(model, vcov, ...) {
    insight::check_if_installed("survival")
    flag1 <- !isFALSE(vcov)
    flag2 <- !isTRUE(checkmate::check_choice(vcov, choices = c("rsample", "boot", "fwb")))
    flag3 <- isTRUE(getOption("marginaleffects_safe", default = TRUE))
    if (flag1 && flag2 && flag3) {
        msg <- 'The default delta method standard errors for `coxph` models only take into account uncertainty in the regression coefficients. Standard errors may be too small. Use the `inferences()` function or set `vcov` to "rsample", "boot"  or "fwb" to compute confidence intervals by bootstrapping. Set `vcov` to `FALSE` or `options(marginaleffects_safe=FALSE)` to silence this warning.'
        warning(msg, call. = FALSE)
    }
    return(model)
}
