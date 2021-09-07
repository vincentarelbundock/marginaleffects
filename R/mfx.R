get_gradient <- function (model, 
                          fitfram, 
                          variable, 
                          prediction_type = "response",
                          numDeriv_method = "simple") {
    UseMethod("get_gradient", model)
}

get_dydx <- function (model, ...) {
    UseMethod("get_dydx", model)
}

get_dydx_se <- function (model, ...) {
    UseMethod("get_dydx_se", model)
}

#' compute marginal effects estimates using numerical derivatives
#' @export
mfx <- function(model, 
                variables = NULL, 
                newdata = NULL, 
                group_names = NULL,
                variance = try(stats::vcov(model), silent = TRUE),
                orientation = "long") {

    # sanity checks and preparation
    checkmate::assert_choice(orientation, choices = c("long", "wide"))
    checkmate::assert_character(group_names, null.ok = TRUE)

    if (is.null(newdata)) {
        newdata <- insight::get_data(model)
    }

    if (is.null(variables)) {
        variables <- insight::find_variables(model)$conditional
    }

    checkmate::assert_data_frame(newdata, any.missing = FALSE)
    checkmate::assert_true(all(variables %in% colnames(newdata)))

    if (!all(sapply(newdata[, variables, drop = FALSE], is.numeric))) {
        stop("All the variables listed in the `variables` argument must be numeric.")
    }

    if (!is.null(variance)) {
        unsupported <- c("lmerMod", "glmerMod", "multinom", "betareg", "polr", "loess")
        if (any(unsupported %in% class(model))) {
            stop(sprintf("Variance estimates are not yet supported for objects of class %s. Please set `variance=NULL` to continue.", class(model))[1])
        }
    }

    # compute and save results in a nested list
    if (is.null(group_names)) {
        group_names <- "main"
    }

    out <- list()
    for (gn in group_names) {
        for (v in variables) {
            tmp <- get_dydx(model = model, 
                            fitfram = newdata,
                            variable = v,
                            variance = variance,
                            group_name = gn)
            if (length(group_names) > 1) {
                tmp$group <- gn
            }
            tmp$term <- v
            out <- c(out, list(tmp))
        }
    }

    # clean output
    out <- do.call("rbind", out)
    newdata$rowid <- 1:nrow(newdata)
    out <- merge(out, newdata, by = "rowid")
    cols <- intersect(c("rowid", "group", "term", "dydx", "std.error"), colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, cols]
    return(out)
}
