add_attributes <- function(out, mfx = NULL, ...) {
    # Always add all attributes from S4 slots
    if (!is.null(mfx)) attr(out, "marginaleffects") <- mfx

    dots <- list(...)
    for (n in names(dots)) {
        if (is.null(attr(out, n))) {
            attr(out, n) <- dots[[n]]
        }
    }

    return(out)
}


prune_attributes <- function(out) {
    if (isTRUE(getOption("marginaleffects_lean", default = FALSE))) {
        out <- prune(out)
    }
    return(out)
}
