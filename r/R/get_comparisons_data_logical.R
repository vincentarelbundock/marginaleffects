get_comparisons_data_logical <- function(model, newdata, variable, ...) {
    # custom data frame or function
    if (
        isTRUE(checkmate::check_function(variable$value)) ||
            isTRUE(checkmate::check_data_frame(variable$value))
    ) {
        out <- contrast_categories_custom(variable, newdata)
        return(out)
    }

    # Default is TRUE - FALSE. A length-2 logical vector is `c(lo, hi)`, and
    # the `rev*` shortcuts flip the default; both used to be silently ignored.
    lo_value <- FALSE
    hi_value <- TRUE
    value <- variable$value
    if (isTRUE(checkmate::check_logical(value, len = 2, any.missing = FALSE))) {
        lo_value <- value[1]
        hi_value <- value[2]
    } else if (isTRUE(checkmate::check_string(value)) && grepl("^rev", value)) {
        lo_value <- TRUE
        hi_value <- FALSE
    }

    lo <- hi <- newdata
    lo[[variable$name]] <- lo_value
    hi[[variable$name]] <- hi_value
    lab <- suppressWarnings(tryCatch(
        sprintf(variable$label, hi_value, lo_value),
        error = function(e) variable$label
    ))
    out <- list(
        rowid = seq_len(nrow(newdata)),
        lo = lo,
        hi = hi,
        original = newdata,
        ter = rep(variable$name, nrow(newdata)),
        lab = rep(lab, nrow(newdata))
    )
    return(out)
}
