config_get <- function(x) {
    config <- config_read()
    return(config[[x]])
}


config_set <- function(x, value) {
    config <- config_read()
    config[[x]] <- value
    dn <- tools::R_user_dir(package = "marginaleffects", which = "config")
    fn <- file.path(dn, "config.rds")
    saveRDS(config, fn)
}


config_delete <- function() {
    dn <- tools::R_user_dir(package = "marginaleffects", which = "config")
    fn <- file.path(dn, "config.rds")
    if (file.exists(fn)) hush(unlink(fn))
    message("`marginaleffects` returned to default settings.", call. = FALSE)
}


config_read <- function() {
    dn <- tools::R_user_dir(package = "marginaleffects", which = "config")
    if (!dir.exists(dn)) dir.create(dn, recursive = TRUE)
    fn <- file.path(dn, "config.rds")
    if (!file.exists(fn)) {
        config <- list()
    } else {
        config <- readRDS(fn)
    }
    return(invisible(config))
}
