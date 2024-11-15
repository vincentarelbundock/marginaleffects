.onAttach <- function(lib, pkg) {
  msg <- c(
    "Please cite the software developers who make your work possible.",
    'Bibliographic info: citation("package_name")',
    "Silence this message: options(marginaleffects_startup_message = FALSE)"
  )
  msg <- paste(msg, collapse = "\n")
  if (isTRUE(getOption("marginaleffects_startup_message", TRUE))) {
    packageStartupMessage(msg)
  }
  invisible()
}
