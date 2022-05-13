# License: https://creativecommons.org/licenses/by-sa/3.0/
# Source: https://stackoverflow.com/a/24569739/342331
myTryCatch <- function(expr) {
    warn <- err <- NULL
    value <- withCallingHandlers(
        tryCatch(expr, error = function(e) {
            err <<- e
            NULL
        }),
        warning = function(w) {
            warn <<- w
            invokeRestart("muffleWarning")
        }
    )
    list(value = value, warning = warn, error = err)
}
