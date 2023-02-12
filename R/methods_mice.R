mi_fit_combine <- function(model, FUN, ...) {
    insight::check_if_installed("tibble")
    dots <- list(...)
    dat_mi <- attr(model, "inferences_midata")
    fit_reg <- function(imp) {
        tmp <- stats::update(model, data = imp)
        attr(model, "inferences_method") <- NULL
        out <- do.call(FUN, c(list(tmp), dots))
        return(out)
    }
    est <- lapply(dat_mi, fit_reg)
    
    # mice uses the `term` column to merge and pool
    idxcols <- grep("^term$|^rowid|^contrast", colnames(est[[1]]), value = TRUE)
    if (length(idxcols) == 0) {
        for (i in seq_along(est)) {
            est[[i]][["term"]] <- seq_len(nrow(est[[i]]))
        }
        class(est[[i]]) <- c("marginaleffects_mi", class(est[[i]]))
    } else {
        idx <- est[[1]][, idxcols, drop = FALSE]
        if (nrow(idx) > 1 && ncol(idx) > 1) {
            for (i in rev(seq_along(idx))) {
                if (length(unique(idx[[i]])) == 1) {
                    idx[[i]] <- NULL
                }
            }
        }
        idx <- apply(idx, MARGIN = 1, paste, collapse = ", ")
        for (i in seq_along(est)) {
            for (k in idxcols) {
                est[[i]][[k]] <- NULL
            }
            est[[i]]$term <- idx
            class(est[[i]]) <- c("marginaleffects_mi", class(est[[i]]))
        }
    }
    mipool <- mice::pool(est)
    out <- mipool$pooled
    out$std.error <- sqrt(out$t)
    class(out) <- c("comparisons", "data.frame")
    
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