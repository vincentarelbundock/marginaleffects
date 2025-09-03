.onAttach <- function(lib, pkg) {
    msg <- c(
        "Please cite the software developers who make your work possible.",
        'One package:             citation("package_name")',
        "All project packages:    softbib::softbib()"
    )
    msg <- paste(msg, collapse = "\n")

    # once every 24 hours
    last_time <- settings_get("startup_message_time")
    if (inherits(last_time, "POSIXct")) {
        flag_time <- difftime(Sys.time(), last_time, units = "sec") >= 24 * 60 * 60
    } else {
        flag_time <- TRUE
    }

    flag_option <- isTRUE(getOption("marginaleffects_startup_message", TRUE))

    if (interactive() && flag_time && flag_option) {
        packageStartupMessage(msg)
        settings_set("startup_message_time", Sys.time(), persistent = TRUE)
    }

    invisible()
}

.onLoad <- function(lib, pkg) {
    backports::import(pkg)
    if (isNamespaceLoaded("reticulate")) {
        py_require_marginal_effects_ad()
    } else {
        setHook(packageEvent("reticulate", "onLoad"), py_require_marginal_effects_ad)
    }
}

py_require_marginal_effects_ad <- function(...) {
    reticulate::py_require("marginaleffectsAD")
}
