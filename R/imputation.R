process_imputation <- function(x, call_attr) {
    insight::check_if_installed("mice")
    mfx_list <- list()
    for (i in seq_along(x$analyses)) {
        calltmp <- call_attr
        calltmp[["model"]] <- x$analyses[[i]]
        calltmp[["modeldata"]] <- get_modeldata(x$analyses[[i]], additional_variables = FALSE)
        mfx_list[[i]] <- evalup(calltmp)
        if (i == 1) {
            out <- mfx_list[[1]]
        }
        mfx_list[[i]]$term <- seq_len(nrow(mfx_list[[i]]))
        class(mfx_list[[i]]) <- c("marginaleffects_mids", class(mfx_list[[i]]))
    }
    mipool <- mice::pool(mfx_list[1:4])
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


#' tidy helper
#' 
#' @noRd
#' @export
tidy.marginaleffects_mids <- function(x, ...) {
    out <- as.data.frame(x[, c("estimate", "std.error")])
    out$term <- seq_len(nrow(out))
    return(out)
}


#' glance helper
#' 
#' @noRd
#' @export
glance.marginaleffects_mids <- function(x, ...) {
    data.frame()
}