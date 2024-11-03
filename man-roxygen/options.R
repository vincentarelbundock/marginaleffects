#' @section Global options:
#' 
#' The behavior of `marginaleffects` functions can be modified by setting global options.
#' 
#' Disable some safety checks and warnings:
#' 
#' ```r
#' options(marginaleffects_safe = FALSE)
#' ```
#'
#' Omit some columns from the printed output:
#'
#' ```r
#' options(marginaleffects_print_omit = c("p.value", "s.value"))`
#' ```
#'
#' Enforce lean return objects, sans information about the original model and
#' data, and other ancillary attributes. Note that this will disable some
#' advanced post-processing features and functions like [hypotheses].
#'
#' ```r
#' options(marginaleffects_lean = TRUE)`
#' ```
#'
#' Other options:
#'
#' * `marginaleffects_plot_gray`: logical. If `TRUE`, the default color of the plot is gray. Default is `FALSE`.
