add_attributes <- function(out, mfx, ...) {
    # Always add all attributes from S4 slots
    attr(out, "marginaleffects") <- mfx

    # Add draws if present and not already set
    if (!is.null(mfx@draws) && is.null(attr(out, "posterior_draws"))) {
        attr(out, "posterior_draws") <- mfx@draws
    }

    dots <- list(...)
    for (n in names(dots)) {
        if (is.null(attr(out, n))) {
            attr(out, n) <- dots[[n]]
        }
    }

    if (isTRUE(getOption("marginaleffects_lean", default = FALSE))) {
        out <- prune(out)
    }

    return(out)
}
