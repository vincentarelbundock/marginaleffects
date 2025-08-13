withr_rm <- function(names) {
  withr::defer(rm(
    list = intersect(names, ls(envir = .GlobalEnv)),
    envir = .GlobalEnv))
  withr::defer(rm(
    list = intersect(names, ls(envir = .GlobalEnv)),
    envir = .GlobalEnv))
}

withr_detach <- function(pkg) {
  pkg <- paste0("package:", pkg)
  withr::defer(detach(pkg, unload = TRUE), test_env())
}
