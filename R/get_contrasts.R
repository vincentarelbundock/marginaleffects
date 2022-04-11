get_contrast_data <- function(model,
                              newdata = NULL,
                              variables = NULL,
                              contrast_factor = "reference",
                              contrast_numeric = 1,
                              ...) {

    lo <- hi <- ter <- lab <- original <- rowid <- list()

    for (v in variables) {
        # logical and character before factor, because they get picked up by find_categorical
        variable_class <- find_variable_class(variable = v, newdata = newdata, model = model)

        # TODO: DOTS

        if (variable_class == "logical") {
            tmp <- get_contrast_data_logical(
                model,
                newdata,
                v,
                ...)
        } else if (variable_class == "factor") {
            tmp <- get_contrast_data_factor(
                model,
                newdata,
                v,
                contrast_factor = contrast_factor,
                ...)
        } else if (variable_class == "numeric") {
            tmp <- get_contrast_data_numeric(
                model,
                newdata,
                v,
                contrast_numeric = contrast_numeric,
                ...)
        } else if (variable_class == "character") {
            tmp <- get_contrast_data_character(
                model,
                newdata,
                v,
                ...)
        } else {
            stop("variable class not supported.")
        }

        lo <- c(lo, list(tmp$lo))
        hi <- c(hi, list(tmp$hi))
        ter <- c(ter, list(tmp$ter))
        lab <- c(lab, list(tmp$lab))
        original <- c(original, list(tmp$original))
        rowid <- c(rowid, list(tmp$rowid))
    }

    lo <- data.table::rbindlist(lo)
    hi <- data.table::rbindlist(hi)

    out <- list(
        lo = lo,
        hi = hi,
        lab = unlist(lab),
        ter = unlist(ter),
        rowid = unlist(rowid),
        original = data.table::rbindlist(original))

    return(out)
}


get_contrasts <- function(model,
                          newdata = NULL,
                          type = "response",
                          variables = NULL,
                          contrast_factor = "reference",
                          contrast_numeric = 1,
                          cache = NULL,
                          ...) {

    # cache is useful for the delta method Jacobian when we need to manipulate
    # the coefficients but don't need to rebuild the contrast data.
    if (is.null(cache)) {
        cache <- get_contrast_data(model,
                newdata = newdata,
                variables = variables,
                contrast_factor = contrast_factor,
                contrast_numeric = contrast_numeric,
                ...)
    }

    out <- data.table(rowid = cache[["rowid"]], term = cache[["ter"]], contrast = cache[["lab"]])

    # some predict() methods need data frames, and will convert data.tables
    # internally, which can be very expensive if done many times. we do it once
    # here.
    lo <- copy(cache[["lo"]])
    hi <- copy(cache[["hi"]])
    setDF(lo)
    setDF(hi)
    pred_lo <- get_predict(model, type = type, newdata = lo, ...)
    pred_hi <- get_predict(model, type = type, newdata = hi, ...)

    # univariate outcome
    if (nrow(pred_lo) == nrow(out)) {
        out[["comparison"]] <- pred_hi$predicted - pred_lo$predicted
    # group or multivariate outcomes
    } else {
        pred_lo[["comparison"]] <- pred_hi$predicted - pred_lo$predicted
        pred_lo[["predicted"]] <- NULL
        setDT(pred_lo)
        # rowid in out is the original N rowids, but with G groups with have N * G 
        out[, "rowid_counterfactual" := rowid]
        out[, "rowid" := 1:.N]
        # sort FALSE because we rely on order later when assigning values
        out <- merge(pred_lo, out, sort = FALSE)
    }

    # normalize slope
    eps <- getOption("marginaleffects_deriv_eps", default = 1e-5)
    out[contrast == "dydx", "comparison" := comparison / eps]
    attr(out, "original") <- cache[["original"]]
    return(out)
}
