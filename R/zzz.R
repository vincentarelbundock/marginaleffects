.onAttach <- function(lib, pkg) {
  msg <- c(
    "Please cite the software that makes your work possible.",
    'For bibliographic info, type: citation("package_name")',
    "Silence this warning with: options(open_source_promo = FALSE)"
  )
  msg <- paste(msg, collapse = "\n")
  if (isTRUE(getOption("open_source_promo", TRUE))) {
    packageStartupMessage(msg)
  }
  invisible()
}
