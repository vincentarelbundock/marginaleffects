#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.glmmTMB <- function(
    model,
    newdata = get_modeldata(model),
    type = "response",
    mfx = NULL,
    newparams = NULL,
    ...) {
    # hack to avoid re-optimization
    # see https://github.com/vincentarelbundock/marginaleffects/issues/1064
    b_vec <- model$obj$env$parList()$b
    if (length(b_vec) > 0) {
        model$modelInfo$map$b <- factor(rep(NA, length(b_vec)))
    }

    np <- model$fit$par
    if (!is.null(newparams)) {
        if (glmmTMB_is_ordinal(model)) {
            np[glmmTMB_ordinal_index(model)] <- newparams
        } else {
            np[seq_along(newparams)] <- newparams
        }
    }

    out <- get_predict.default(
        model = model,
        newdata = newdata,
        type = type,
        allow.new.levels = TRUE, # otherwise we get errors in marginal_means()
        newparams = np,
        ...
    )
    return(out)
}


#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.glmmTMB <- function(model, vcov, ...) {
    vcov <- sanitize_vcov(model, vcov)

    # glmmTMB's sandwich path can change the TMB environment state used by
    # conditional predictions. Restore it after extracting the covariance matrix.
    env <- model$obj$env
    if (is.environment(env)) {
        last_par <- if (exists("last.par", envir = env, inherits = FALSE)) env$last.par else NULL
        last_par_best <- if (exists("last.par.best", envir = env, inherits = FALSE)) env$last.par.best else NULL
        on.exit({
            if (!is.null(last_par)) env$last.par <- last_par
            if (!is.null(last_par_best)) env$last.par.best <- last_par_best
        }, add = TRUE)
    }

    if (glmmTMB_is_ordinal(model)) {
        # The fixed intercept triggers a spurious rank-deficiency warning.
        out <- withCallingHandlers(
            insight::get_varcov(model, vcov = vcov, component = "full"),
            warning = function(w) {
                if (grepl("rank deficient", conditionMessage(w), fixed = TRUE)) {
                    invokeRestart("muffleWarning")
                }
            }
        )
        # The fixed intercept has an all-NA row; the remaining rows follow
        # `model$fit$par`. Keep fixed effects and cutpoints, drop RE parameters.
        keep <- !apply(is.na(out), 1, all)
        out <- out[keep, keep, drop = FALSE]
        if (nrow(out) != length(model$fit$par)) {
            stop_sprintf("Unable to align the variance-covariance matrix with the parameters of this ordinal `glmmTMB` model.")
        }
        idx <- glmmTMB_ordinal_index(model)
        out <- out[idx, idx, drop = FALSE]
        dimnames(out) <- list(names(get_coef.glmmTMB(model)), names(get_coef.glmmTMB(model)))
        return(out)
    }

    # Extract the full covariance matrix
    out <- insight::get_varcov(model, vcov = vcov, component = "full")

    # Extract the fixed-effect coefficient names from get_coef
    coef_names <- names(get_coef.glmmTMB(model))

    # Handle dispersion and conditional terms
    cleaned_coef_names <- gsub("^cond~", "", coef_names) # Remove cond~ for conditional terms
    cleaned_coef_names <- gsub("^disp~", "d~", cleaned_coef_names) # Map disp~ to d~ for dispersion terms

    # The 'upper cutoff' and 'lower cutoff' will remain in both, so no removal

    # Get the current row and column names from the covariance matrix
    current_names <- rownames(out)

    # Match cleaned coef_names with current names in the covariance matrix
    matched_indices <- match(current_names, cleaned_coef_names)

    # Replace row/column names only where there is a valid match
    valid_indices <- which(!is.na(matched_indices))

    if (length(valid_indices) > 0) {
        # Apply the correct names from coef_names to matched rows/columns in the covariance matrix
        rownames(out)[valid_indices] <- coef_names[matched_indices[valid_indices]]
        colnames(out)[valid_indices] <- coef_names[matched_indices[valid_indices]]
    } else {
        warning(
            "No matching terms found between the covariance matrix and fixed-effect coefficients."
        )
    }

    return(out)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.glmmTMB <- function(model, ...) {
    if (glmmTMB_is_ordinal(model)) {
        # The intercept is not identified in ordinal models: `fixef()` reports
        # it as 0, but it is absent from `model$fit$par`. The cutpoints (`psi`)
        # are absent from `fixef()`. Return the free fixed effects and cutpoints,
        # in `model$fit$par` order.
        lev <- get_group_names(model)
        b <- setdiff(names(glmmTMB::fixef(model)$cond), "(Intercept)")
        nam <- c(
            if (length(b) > 0) paste0("cond~", b) else character(0),
            paste(lev[-length(lev)], lev[-1], sep = "|")
        )
        out <- model$fit$par[glmmTMB_ordinal_index(model)]
        if (length(out) != length(nam)) {
            stop_sprintf("Unable to extract the coefficients of this ordinal `glmmTMB` model.")
        }
        names(out) <- nam
        return(out)
    }

    # Extract the fixed-effect coefficients
    out <- unlist(glmmTMB::fixef(model))

    # Apply the gsub logic to rename terms (cond~, disp~, etc.)
    names(out) <- gsub("^(cond|zi|disp)\\.", "\\1~", names(out))

    # No removal of "lower cutoff" and "upper cutoff" - they remain in place
    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.glmmTMB <- function(model, coefs, ...) {
    # use predict(`newparams`) for this kind of model
    return(model)
}

#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.glmmTMB <- function(model, vcov = TRUE, re.form, ...) {
    if (identical(vcov, "HC0")) {
        insight::check_if_installed("glmmTMB", minimum_version = "1.1.12")
    }

    # re.form=NA
    if (!isTRUE(checkmate::check_flag(vcov)) && !identical(vcov, "HC0")) {
        msg <- 'For this model type, `vcov` must be `TRUE`, `FALSE`, `"HC0"`.'
        stop_sprintf(msg)
    }

    if (!isFALSE(vcov) && (missing(re.form) || (!isTRUE(is.na(re.form))))) {
        msg <- "For this model type, `marginaleffects` only takes into account the uncertainty in fixed-effect parameters. This is often appropriate when `re.form=NA`, but may be surprising to users who set `re.form=NULL` (default) or to some other value. Call `options(marginaleffects_safe = FALSE)` to silence this warning."
        warn_sprintf(msg)
    }

    return(model)
}


#' @include get_group_names.R
#' @rdname get_group_names
#' @export
get_group_names.glmmTMB <- function(model, ...) {
    if (glmmTMB_is_ordinal(model)) {
        return(get_group_names.polr(model))
    }
    return(get_group_names.default(model))
}


glmmTMB_is_ordinal <- function(model) {
    isTRUE(hush(stats::family(model)$family) == "ordinal")
}


# positions in `model$fit$par` of the fixed effects and cutpoints of an ordinal model
glmmTMB_ordinal_index <- function(model) {
    which(names(model$fit$par) %in% c("beta", "psi"))
}
