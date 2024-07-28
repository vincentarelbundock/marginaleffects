#' @section Global options:
#' 
#' The behavior of `marginaleffects` functions can be modified by setting global options.
#' 
#' Disable some safety checks:
#' 
#' ```r
#' options(marginaleffects_safe = FALSE)`
#' ```
#'
#' Omit some columns from the printed output:
#'
#' ```r
#' options(marginaleffects_print_omit = c("p.value", "s.value"))`
#' ```
