add_attributes <- function(out, mfx) {
    # Always add all attributes from S4 slots
    attr(out, "call") <- mfx@call
    attr(out, "newdata") <- mfx@newdata
    attr(out, "model") <- mfx@model
    attr(out, "model_type") <- class(mfx@model)[1]
    attr(out, "vcov") <- mfx@vcov_model
    attr(out, "conf_level") <- mfx@conf_level
    attr(out, "type") <- mfx@type
    
    # Add draws if present and not already set
    if (!is.null(mfx@draws) && is.null(attr(out, "posterior_draws"))) {
        attr(out, "posterior_draws") <- mfx@draws
    }
    
    # Add df if present and numeric
    if (!is.null(mfx@df) && is.numeric(mfx@df)) {
        attr(out, "df") <- mfx@df
    }
    
    # Add jacobian if present
    if (!is.null(mfx@jacobian)) {
        attr(out, "jacobian") <- mfx@jacobian
    }
    
    # Add weights if present
    if (!is.null(mfx@wts)) {
        attr(out, "weights") <- mfx@wts
    }
    
    return(out)
}