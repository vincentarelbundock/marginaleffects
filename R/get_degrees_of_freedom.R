get_degrees_of_freedom <- function(model, df = Inf, vcov = NULL, newdata = NULL) {
    is_inf <- isTRUE(checkmate::check_numeric(df, len = 1, upper = Inf, lower = Inf))

    # before NULL return
    if (isTRUE(checkmate::check_choice(vcov, c("satterthwaite", "kenward-roger")))) {
        checkmate::assert_data_frame(newdata)
        # predict.lmerTest requires the DV
        if (inherits(model, "lmerMod")) {
            dv <- insight::find_response(model)
            if (!dv %in% colnames(newdata)) {
                newdata[[dv]] <- mean(insight::get_response(model))
            }
        }

        df <- insight::get_df(x = model, data = newdata, vcov = vcov, df_per_observation = TRUE)
    } else if (is_inf || isFALSE(df) || is.null(df)) {
        return(Inf)
    } else if (isTRUE(checkmate::check_numeric(df))) {
        # pass
    } else if (isTRUE(df)) {
        df <- insight::get_df(x = model)
    } else {
        stop("Invalid arguments for `get_df`.")
    }

    if (isTRUE(checkmate::check_data_frame(newdata))) {
        checkmate::assert(
            checkmate::check_numeric(df, len = 1),
            checkmate::check_numeric(df, len = nrow(newdata))
        )
    }

    return(df)
}
