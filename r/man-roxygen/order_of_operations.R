#' @section Order of operations:
#'
#' Behind the scenes, the arguments of `marginaleffects` functions are evaluated in this order:
#'
#' 1. `newdata`
#' 2. `variables`
#' 3. `comparison` and `slope`
#' 4. `by`
#' 5. `vcov`
#' 6. `hypothesis`
#' 7. `transform`
#

