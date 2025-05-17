#' @section Parallel computation:
#'
#' The `slopes()` and `comparisons()` functions can use parallelism to
#' speed up computation. Operations are parallelized for the computation of
#' standard errors, at the model coefficient level. There is always
#' considerable overhead when using parallel computation, mainly involved
#' in passing the whole dataset to the different processes. Thus, parallel
#' computation is most likely to be useful when the model includes many parameters
#' and the dataset is relatively small.
#'
#' Warning: In many cases, parallel processing will not be useful at all.
#'
#' To activate parallel computation, users must load the `future.apply` package,
#' call `plan()` function, and set a global option.
#'
#' `options(marginaleffects_parallel = TRUE)`: parallelize delta method computation of standard errors.
#' `options(marginaleffects_parallel_inferences = TRUE)`: parallelize `"rsample"` or `"fwb"` bootstrap computation in `inferences()`.
#' `options(marginaleffects_parallel_packages = TRUE)`: vector of strings with the names of modelling packages used to fit the model, ex: c("survival", "splines")
#'
#' For example:
#'
#' ```{r, eval = FALSE}
#' library(future.apply)
#' plan("multisession", workers = 4)
#' options(marginaleffects_parallel = FALSE)
#' options(marginaleffects_parallel_inferences = TRUE)
#' options(marginaleffects_parallel_packages = c("survival", "splines"))
#'
#' slopes(model)
#' ```
#'
#' To disable parallelism in `marginaleffects` altogether, you can set a global option:
#'
#' ```{r, eval = FALSE}
#' options(marginaleffects_parallel = FALSE)
#' ```
