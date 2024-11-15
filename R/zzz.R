.onAttach <- function(lib, pkg) {
  msg <- c(
    "Please cite the software developers who make your work possible.",
    "",
    'One package:             citation("package_name")',
    "All project packages:    softbib::softbib()",
    "",
    "Silence this message: options(marginaleffects_startup_message = FALSE)"
  )
  msg <- paste(msg, collapse = "\n")
  if (interactive() && isTRUE(getOption("marginaleffects_startup_message", TRUE))) {
    packageStartupMessage(msg)
  }
  invisible()
}
