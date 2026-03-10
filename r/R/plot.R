# The reason why I don't supply these functions is that there is too much ambiguity about which kind of plot people are expecting. The `plot_predictions()` function has a bunch of extra arguments, and makes assumptions based on the order of the user-supplied `condition` values, for example. Having another `plot.predictions()` method in addition would require me to add all those arguments, or to make opinionated decisions that would require a ton of `if/else` based on the original `predictions()` call's `by`, `newdata` grid, and others. This is hard, and almost certainly unsatisfactory. Also, it duplicates the plotting user-interface. It feels better to steer all users to a single point of entry for plots.

#' @export
plot.predictions <- function(x, ...) {
    stop_sprintf("Please use the `plot_predictions()` function.")
}

#' @export
plot.comparisons <- function(x, ...) {
    stop_sprintf("Please use the `plot_comparisons()` function.")
}

#' @export
plot.slopes <- function(x, ...) {
    stop_sprintf("Please use the `plot_slopes()` function.")
}
