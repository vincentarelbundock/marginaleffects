.onAttach <- function(lib, pkg) {
  msg <- c(
    "Please cite the software developers who make your work possible.",
    'One package:             citation("package_name")',
    "All project packages:    softbib::softbib()"
  )
  msg <- paste(msg, collapse = "\n")

  # once every 24 hours
  last_time <- config_get("startup_message_time")
  if (inherits(last_time, "POSIXct")) {
    flag_time <- difftime(Sys.time(), last_time, units = "sec") >= 24 * 60 * 60
  } else {
    flag_time <- TRUE
  }

  flag_option <- isTRUE(getOption("marginaleffects_startup_message", TRUE))

  if (interactive() && flag_time && flag_option) {
    packageStartupMessage(msg)
    config_set("startup_message_time", Sys.time())
  }

  invisible()
}

.onLoad <- function(lib, pkg) {
  backports::import(pkg)
}
