marginaleffects_settings <- new.env()

# https://stackoverflow.com/questions/79786610/solved-r-package-loading-error-onattach-failed-in-attachnamespace
settings_read_persistent <- function() {
    dn <- tools::R_user_dir(package = "marginaleffects", which = "config")
    if (!dir.exists(dn)) dir.create(dn, recursive = TRUE)
    fn <- file.path(dn, "config.rds")
    if (!file.exists(fn)) {
        config <- list()
    } else {
        config <- tryCatch(readRDS(fn), error = function(err) {
          warning("marginaleffects: loading persistence failed, this can happen when ",
            sQuote(file.path(dn, "config.rds"), FALSE), " becomes corrupted.\n",
            "  We suggest you delete this file and/or restore it from another location if able.\n",
            "  The load failure: ", conditionMessage(err))
          list()
        })
    }
    return(invisible(config))
}


settings_get <- function(name) {
    # First check in-memory settings
    if (name %in% names(marginaleffects_settings)) {
        return(get(name, envir = marginaleffects_settings))
    }

    # Then check persistent storage
    persistent_config <- settings_read_persistent()
    if (name %in% names(persistent_config)) {
        return(persistent_config[[name]])
    }

    return(NULL)
}

settings_set <- function(name, value, persistent = FALSE) {
    if (persistent) {
        # Save to persistent storage
        config <- settings_read_persistent()
        config[[name]] <- value
        dn <- tools::R_user_dir(package = "marginaleffects", which = "config")
        fn <- file.path(dn, "config.rds")
        saveRDS(config, fn)
    } else {
        # Save to in-memory storage
        assign(name, value = value, envir = marginaleffects_settings)
    }
}

settings_rm <- function(name = NULL) {
    if (is.null(name)) {
        rm(list = names(marginaleffects_settings), envir = marginaleffects_settings)
    } else if (name %in% names(marginaleffects_settings)) {
        rm(list = name, envir = marginaleffects_settings)
    }
}

settings_equal <- function(name, comparison) {
    k <- settings_get(name)
    if (!is.null(k) && length(comparison) == 1 && k == comparison) {
        out <- TRUE
    } else if (!is.null(k) && length(comparison) > 1 && k %in% comparison) {
        out <- TRUE
    } else {
        out <- FALSE
    }
    return(out)
}
