get_contrasts <- function(model,
                          newdata = NULL,
                          variable = NULL,
                          type = "response",
                          contrast_factor = "reference",
                          contrast_numeric = 1,
                          ...) {

   # logical and character before factor, because they get picked up by find_categorical
    if (is.logical(newdata[[variable]])) {
        get_contrasts_fun <- get_contrasts_logical
    } else if (is.character(newdata[[variable]])) {
        get_contrasts_fun <- get_contrasts_character
    } else if (is.factor(newdata[[variable]]) ||
               variable %in% find_categorical(newdata = newdata, model = model) ||
               isTRUE(attr(newdata[[variable]], "factor"))) {
        get_contrasts_fun <- get_contrasts_factor
    } else if (is.numeric(newdata[[variable]])) {
        get_contrasts_fun <- get_contrasts_numeric
    } else {
        stop(sprintf("Cannot compute contrasts for variable %s of class %s",
                     variable,
                     class(newdata[[variable]])))
    }

    out <- get_contrasts_fun(
        model = model,
        newdata = newdata,
        variable = variable,
        type = type,
        contrast_factor = contrast_factor,
        contrast_numeric = contrast_numeric,
        ...)

    if (isTRUE(list(...)[["internal_call"]]) && !"group" %in% colnames(out)) {
        out[["group"]] <- "main_marginaleffect"
    }

    return(out)
}
