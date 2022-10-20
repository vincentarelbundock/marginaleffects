marginaleffects_settings <- new.env()

settings_cache <- function(setti) {
    out <- list()
    for (s in setti) {
        out[[s]] <- settings_get(s)
    }
    return(out)
}

settings_restore <- function(cache) {
    for (n in names(cache)) {
        settings_set(n, cache[[n]])
    }
}

settings_init <- function(settings = NULL) {
    settings_rm()

    default_settings <- list(
        marginaleffects_safefun_return1 = FALSE
    )

    checkmate::assert_list(settings, null.ok = TRUE, names = "unique")

    if (!is.null(settings)) {
        settings <- c(settings, default_settings)
    }

    for (i in seq_along(settings)) {
        settings_set(names(settings)[i], settings[[i]])
    }
}

settings_get <- function(name) {
    if (name %in% names(marginaleffects_settings)) {
        get(name, envir = marginaleffects_settings)
    } else {
        NULL
    }
}

settings_set <- function(name, value) {
    assign(name, value = value, envir = marginaleffects_settings)
}

settings_rm <- function(name = NULL) {
    if (is.null(name)) {
        rm(list = names(marginaleffects_settings), envir = marginaleffects_settings)
    } else if ("name" %in% names(marginaleffects_settings)) {
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
