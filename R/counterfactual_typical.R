#' Generate "counterfactual" datasets for use in `marginaleffects`'s `newdata` argument
#' 
#' @param ... named arguments with vectors of values for the variables to construct (see Examples below.)
#' @param model Model object
#' @param data data.frame (one and only one of the `model` and `data` arguments
#' must be true).
#' @return A `data.frame` where each row of the original data is repeated
#' multiple times for each of the values of the variables in the `at` list. See
#' example below.
#'
#' If users supply a model, the data used to fit that model is retrieved using
#' the `insight::get_data` function, and then replicated with different values
#' of the variables in the `at` list.
#' @export
#' @examples
#' # All rows are repeated twice, with different values of `hp`
#' cd <- counterfactual(data = mtcars, hp = c(100, 110))
#' cd[cd$rowid %in% 1:3,]
#' 
#' # We get the same result by feeding a model instead of a data.frame
#' mod <- lm(mpg ~ hp + wt, mtcars)
#' cd <- counterfactual(model = mod, hp = c(100, 110))
#' cd[cd$rowid %in% 1:3,]
#' 
#' # Use in `marginaleffects` to compute "Counterfactual Average Marginal Effects"
#' marginaleffects(mod, newdata = counterfactual(hp = c(100, 110)))
#'
counterfactual <- function(..., model = NULL, data = NULL) {

    tmp <- prep_counterfactual_typical(..., model = model, data = data)
    at <- tmp$at
    dat <- tmp$dat
    v_all <- tmp$all
    v_manual <- tmp$v_manual
    v_automatic <- tmp$v_automatic

    # `at` -> `data.frame`
    at <- expand.grid(at)

    rowid <- data.frame(rowid = 1:nrow(dat))
    if (length(v_automatic) > 0) {
        dat_automatic <- dat[, v_automatic, drop = FALSE]
        dat_automatic <- cbind(rowid, dat_automatic)
        out <- merge(dat_automatic, at, all = TRUE)
    }  else {
        out <- merge(rowid, at, all = TRUE)
    }

    return(out)
}


#' Generate "typical" datasets for use in `marginaleffects`'s `newdata` argument
#' 
#' @param ... named arguments with vectors of values for the typical variables to construct (see Examples below.)
#' @param model Model object
#' @param data data.frame (one and only one of the `model` and `data` arguments
#' must be true).
#' @return A `data.frame` with the values specified in the `at` list, and where each of the other variables is set at its median or mode.
#' @export
#' @examples
#' # The output only has 2 rows, and all the variables except `hp` are at their
#' # median or mode.
#' typical(data = mtcars, hp = c(100, 110))
#' 
#' # We get the same result by feeding a model instead of a data.frame
#' mod <- lm(mpg ~ hp, mtcars)
#' typical(model = mod, hp = c(100, 110))
#' 
#' # Use in `marginaleffects` to compute "Typical Marginal Effects"
#' marginaleffects(mod, newdata = typical(hp = c(100, 110)))
#'
typical <- function(..., model = NULL, data = NULL) {

    tmp <- prep_counterfactual_typical(..., model = model, data = data)
    at <- tmp$at
    dat <- tmp$dat
    v_all <- tmp$all
    v_manual <- tmp$v_manual
    v_automatic <- tmp$v_automatic
    
    if (length(v_automatic) > 0) {
        dat_automatic <- dat[, v_automatic, drop = FALSE]
        dat_automatic <- stats::na.omit(dat_automatic)
        out <- median_or_mode(dat_automatic)
    } else {
        out <- list()
    }

    if (!is.null(at)) {
        for (n in names(at)) {
            out[n] <- at[n]
        }
    }

    out <- expand.grid(out)
    return(out)

}


prep_counterfactual_typical <- function(..., model = NULL, data = NULL) {

    checkmate::assert_data_frame(data, null.ok = TRUE)

    at <- list(...)
    
    if (!is.null(model) & !is.null(data)) {
        stop("One of the `model` or `data` arguments must be `NULL`.")
    }
    if (is.null(model) & is.null(data)) {
        stop("The `model` or `data` arguments cannot both be `NULL`.")
    }

    if (!is.null(model)) {
        dat <- insight::get_data(model)
        v_all <- insight::find_variables(model)$conditional
    } else {
        dat <- data
        v_all <- colnames(dat)
    }

    v_manual <- names(at)
    v_automatic <- setdiff(v_all, v_manual)

    # check `at` names
    v_bad <- setdiff(names(at), v_all)
    if (length(v_bad) > 0) {
        warning(sprintf("Variables in `at` are missing from the model data: %s", paste(v_bad, collapse = ", ")))
    }

    # check `at` elements and convert them to factor as needed
    for (n in names(at)) {
        if (is.factor(dat[[n]])) {
            at[[n]] <- as.character(at[[n]])
            if (!all(at[[n]] %in% levels(dat[[n]]))) {
                msg <- sprintf('The "%s" element of the `at` list corresponds to a factor variable. The values entered in the `at` list must be one of the factor levels: "%s".', n, paste(levels(dat[[n]]), collapse = '", "'))
                stop(msg)
            } else {
                at[[n]] <- factor(at[[n]], levels = levels(dat[[n]]))
            }
        }
    }

    out <- list("dat" = dat, 
                "at" = at, 
                "v_all" = v_all, 
                "v_manual" = v_manual, 
                "v_automatic" = v_automatic)
    return(out)
}

