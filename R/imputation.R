process_mira <- function(miraobj, call_attr) {
    mfxobj <- list()
    for (i in seq_along(miraobj$analyses)) {
        calltmp <- call_attr
        calltmp[["model"]] <- miraobj$analyses[[i]]
        # make sure we get the right dataset
        if (is.null(calltmp[["newdata"]])) { 
            calltmp[["newdata"]] <- miraobj$analyses[[i]]
        }
        browser()
        mfxobj[[i]] <- evalup(calltmp)
        if (i == 1) {
            out <- mfxobj[[1]]
        }
        mfxobj[[i]]$term <- seq_len(nrow(mfxobj[[i]]))
        class(mfxobj[[i]]) <- c("marginaleffects_mi", class(mfxobj[[i]]))
    }
    mipool <- mice::pool(mfxobj)
    for (col in c("estimate", "statistic", "p.value", "conf.low", "conf.high")) {
        if (col %in% colnames(out) && col %in% colnames(mipool$pooled)) {
            out[[col]] <- mipool$pooled[[col]]
        } else {
            out[[col]] <- NULL
        }
    }
    if ("df" %in% colnames(mipool$pooled)) {
        out$df <- mipool$pooled$df
    }
    out$std.error <- sqrt(mipool$pooled$t)
    out <- get_ci(
        out,
        vcov = call_attr[["vcov"]],
        conf_level = call_attr[["conf_level"]],
        df = mipool$pooled$df)
    attr(out, "inferences") <- mipool
    return(out)
}

mi_fit_combine <- function(model, FUN, ...) {
    insight::check_if_installed("tibble")
    dots <- list(...)
    dat_mi <- attr(model, "inferences_midata")
    fit_reg <- function(imp) {
        tmp <- stats::update(model, data = imp)
        attr(tmp, "inferences_method") <- NULL
        out <- do.call(FUN, c(list(tmp), dots))
        return(out)
    }
    est <- lapply(dat_mi, fit_reg)
    
    # mice uses the `term` column to merge and pool
    for (i in seq_along(est)) {
        est[[i]][["term"]] <- seq_len(nrow(est[[i]]))
        class(est[[i]]) <- c("marginaleffects_mi", class(est[[i]]))
    }
    mipool <- mice::pool(est)
    
    # inefficient
    # this is two extra estimations: before inference and now
    # but it is convenient because we get all the object structure
    attr(model, "inferences_method") <- NULL
    out <- do.call(FUN, c(list(model), dots))
    for (col in c("estimate", "statistic", "p.value", "conf.low", "conf.high")) {
        if (col %in% colnames(out) && col %in% colnames(mipool$pooled)) {
            out[[col]] <- mipool$pooled[[col]]
        } else {
            out[[col]] <- NULL
        }
    }
    if ("df" %in% colnames(mipool$pooled)) {
        out$df <- mipool$pooled$df
    }
    out$std.error <- sqrt(mipool$pooled$t)
    out <- get_ci(out, vcov = dots$vcov, conf_level = dots$conf_level)
    attr(out, "inferences") <- mipool
    return(out)
}


#' tidy helper
#' 
#' @noRd
#' @export
tidy.marginaleffects_mi <- function(x, ...) {
    out <- tibble::as_tibble(x)
    return(out)
}