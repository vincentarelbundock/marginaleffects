find_variable_class <- function(variable, newdata, model = NULL) {
   # logical and character before factor, because they get picked up by find_categorical
    if (is.logical(newdata[[variable]])) {
        return("logical")
    }
    if (is.character(newdata[[variable]])) {
        return("character")
    }
    if (is.factor(newdata[[variable]]) ||
        variable %in% find_categorical(newdata = newdata, model = model) || isTRUE(attr(newdata[[variable]], "factor"))) {
        return("factor")
    }
    if (is.numeric(newdata[[variable]])) {
        return("numeric")
    }
    stop(sprintf("Cannot compute contrasts for variable %s of class `%s`.",
                 variable, class(newdata[[variable]])), call. = FALSE)
}

