#' @section Parallel computation:
#'
#' The `marginaleffects()` and `comparisons()` functions can use parallelism to
#' speed up computation. Operations are parallelized at the "term" or
#' "variable" level. This means that no speed gain is available when the user
#' computes marginal effects for only one right-hand side variable. There is
#' always some overhead when using parallel computation. Thus, parallel
#' computation is most likely to be useful when: 
#' 
#' 1. The model includes many right-hand side variables.
#' 2. `marginaleffects` must compute pairwise contrasts for multiple factor variables.
#'
#' To activate parallel computation, users must load the `future.apply` package
#' and call `plan()` function. For example:
#'
#' ```{r, eval = FALSE}
#' library(future.apply)
#' plan("multisession")
#' marginaleffects(model)
#' ```
#'
