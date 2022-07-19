get_contrast_data_logical <- function(model,
                                      newdata,
                                      variable,
                                      contrast_label,
                                      ...) {
    lo <- hi <- newdata
    lo[[variable]] <- FALSE
    hi[[variable]] <- TRUE
    lab <- sprintf(contrast_label, TRUE, FALSE)
    out <- list(rowid = seq_len(nrow(newdata)),
        lo = lo,
        hi = hi,
        original = newdata,
        ter = rep(variable, nrow(newdata)),
        lab = rep(lab, nrow(newdata)))
    return(out)
}
