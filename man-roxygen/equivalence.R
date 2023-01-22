#' @section Equivalence, Inferiority, Superiority:
#'
#' \eqn{\theta} is an estimate, \eqn{\sigma_\theta} its estimated standard error, and \eqn{[B_l, B_u]} are the bounds of the equivalence region supplied to the `equivalence` argument.
#' 
#' The test statistics are:
#' 
#' \itemize{
#' \item Non-inferiority: \eqn{t=(\theta - B_l)/\sigma_\theta}
#' \item Non-superiority: \eqn{t=(\theta - B_u)/\sigma_\theta}
#' }
#' 
#' The p values are:
#' 
#' \itemize{
#' \item Non-inferiority: Upper tail probability
#' \item Non-superiority: Lower tail probability
#' \item Equivalence (TOST): Maximum of the non-inferiority and non-superiority p values.
#' }
#' 
#' Thanks to Russell V. Lenth for the excellent `emmeans` package and documentation which inspired this feature.
#' 
 
