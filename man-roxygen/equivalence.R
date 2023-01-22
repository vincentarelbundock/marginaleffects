#' @section Equivalence: Equivalence, non-inferiority, and non-superiority tests
#'
#' \eqn{\theta} is an estimate, \eqn{\sigma_\theta} its estimated standard error, and \eqn{[B_{min}, B_{max}]} are the bounds of the equivalence region supplied to the `equivalence` argument.
#' 
#' The test statistics are:
#' 
#' \itemize{
#' \item Non-inferiority: \eqn{t=(\theta - B_{min})/\sigma_\theta}
#' \item Non-superiority: \eqn{t=(\theta - B_{max})/\sigma_\theta}
#' }
#' 
#' The p values are:
#' 
#' \itemize{
#' \item Non-inferiority: Lower tail
#' \item Non-superiority: Upper tail
#' \item Equivalence (TOST): Maximum of the non-inferiority and non-superiority p values.
#' }
#' 
#' Thanks to Russell V. Lenth for the excellent `emmeans` package and documentation which inspired this feature.
#' 
 
