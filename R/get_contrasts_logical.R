get_contrast_data_logical <- function(model,
                                      newdata,
                                      variable,
                                      ...) {
    lo <- hi <- newdata
    lo[[variable]] <- FALSE
    hi[[variable]] <- TRUE
    lab <- rep("TRUE - FALSE", nrow(lo))
    out <- list(rowid = seq_len(nrow(newdata)),
        lo = lo,
        hi = hi,
        original = newdata,
        ter = rep(variable, nrow(newdata)),
        lab = rep("TRUE - FALSE", nrow(newdata)))
    return(out)
}
