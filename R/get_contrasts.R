get_contrasts <- function(model,
                          newdata = NULL,
                          variable = NULL,
                          type = "response",
                          contrast_factor = "reference",
                          contrast_numeric = 1,
                          vcov = TRUE,
                          ...) {

    # logical and character before factor, because they get picked up by find_categorical
    variable_class <- find_variable_class(variable = variable, newdata = newdata, model = model)
    if (variable_class == "logical") get_contrasts_fun <- get_contrasts_logical
    if (variable_class == "factor") get_contrasts_fun <- get_contrasts_factor
    if (variable_class == "numeric") get_contrasts_fun <- get_contrasts_numeric
    if (variable_class == "character") get_contrasts_fun <- get_contrasts_character

    out <- get_contrasts_fun(
        model = model,
        newdata = newdata,
        variable = variable,
        type = type,
        contrast_factor = contrast_factor,
        contrast_numeric = contrast_numeric,
        internal_call = TRUE,
        ...)

    if (isTRUE(list(...)[["internal_call"]]) && !"group" %in% colnames(out)) {
        out[["group"]] <- "main_marginaleffect"
    }

    return(out)
}
