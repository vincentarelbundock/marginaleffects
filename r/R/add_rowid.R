add_rowid <- function(p, newdata) {
    if (!"rowid" %in% colnames(newdata)) {
        return(p)
    }
    if (nrow(p) == nrow(newdata)) {
        p$rowid <- newdata$rowid
    } else {
        p$rowid <- rep(newdata$rowid, nrow(p) / nrow(newdata))
    }
    return(p)
}
