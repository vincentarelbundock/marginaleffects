get_df <- function(model, df = Inf, newdata = NULL) {

    # before NULL return
    if (isTRUE(checkmate::check_choice(vcov, c("satterthwaite", "kenward-roger")))) {
        df <- vcov
    }

    if (isTRUE(checkmate::check_choice(df, c("satterthwaite", "kenward-roger")))) {
        checkmate::assert_data_frame(newdata)
        # predict.lmerTest requires the DV
        if (inherits(model, "lmerMod")) {
            dv <- insight::find_response(model)
            if (!dv %in% colnames(newdata)) {
                newdata[[dv]] <- mean(insight::get_response(model))
            }
        }

        df <- insight::get_df(x = model, data = newdata, type = df, df_per_obs = TRUE)
    } else if (isTRUE(checkmate::check_number(df, lower = Inf)) || isFALSE(df) || isTRUE(checkmate::check_null(df))) {
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


sanitize_df <- function(df, model, newdata, vcov = NULL) {
    # K-W changes both the vcov and the df
    # Satterthwaite changes the df but not the vcov
    if (isTRUE(checkmate::check_choice(vcov, c("satterthwaite", "kenward-roger")))) {
        df <- vcov
    }
    checkmate::assert(
        checkmate::check_true(df),
        checkmate::check_number(df, lower = 1),
        checkmate::check_numeric(df, len = nrow(newdata)),
        checkmate::check_choice(df, c("satterthwaite", "kenward-roger")),
    )

    return(df)
}
