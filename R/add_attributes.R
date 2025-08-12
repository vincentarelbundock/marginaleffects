add_attributes <- function(out, mfx, lean = getOption("marginaleffects_lean", default = FALSE)) {
    # Always add these attributes as they are needed for print method
    attr(out, "conf_level") <- mfx@conf_level
    attr(out, "type") <- mfx@type
    
    # Add other attributes from S4 slots if not in lean mode
    if (!isTRUE(lean)) {
        attr(out, "call") <- mfx@call
        attr(out, "newdata") <- mfx@newdata
        attr(out, "model") <- mfx@model
        attr(out, "model_type") <- class(mfx@model)[1]
        attr(out, "vcov") <- mfx@vcov_model
        
        # Add draws if present and not already set
        if (!is.null(mfx@draws) && is.null(attr(out, "posterior_draws"))) {
            attr(out, "posterior_draws") <- mfx@draws
        }
        
        # Add df if present and numeric
        if (!is.null(mfx@df) && is.numeric(mfx@df)) {
            attr(out, "df") <- mfx@df
        }
    } else {
        # In lean mode, remove all attributes except essential ones
        for (a in setdiff(
            names(attributes(out)),
            c("names", "row.names", "class", "conf_level", "type")
        )) {
            attr(out, a) <- NULL
        }
    }
    
    return(out)
}