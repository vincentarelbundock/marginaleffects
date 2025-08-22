get_comparisons_data_logical <- function(model, newdata, variable, ...) {
    # custom data frame or function
    if (
        isTRUE(checkmate::check_function(variable$value)) ||
            isTRUE(checkmate::check_data_frame(variable$value)) ||
            isTRUE(checkmate::check_data_table(variable$value))
    ) {
        out <- contrast_categories_custom(variable, newdata)
        return(out)
    }

    lo <- hi <- newdata
    lo[[variable$name]] <- FALSE
    hi[[variable$name]] <- TRUE
    lab <- suppressWarnings(tryCatch(
        sprintf(variable$label, TRUE, FALSE),
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
