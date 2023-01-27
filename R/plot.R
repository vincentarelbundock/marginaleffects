#' Plot average comparisons and slopes
#' 
#' `x` is re-evaluated by the corresponding `avg_*()` function the result is drawn as a point-range plot using `ggplot2`.
#' @param x object produced by the `comparisons()` or `slopes()` object.
#' @param ... additional arguments are passed to the `aggregate` function (e.g., `conf_level`).
#' @return A `ggplot2` object
#' @noRd
#' @examples
#' mod <- glm(am ~ hp + wt, data = mtcars)
#' avg_slopes(mod)
#' plot_slopes(mod)
#' 
#' 
#' mod <- glm(am ~ hp + factor(gear), data = mtcars)
#' avg_comparisons(mod)
#' plot_comparisons(cmp)
#
plot_avg <- function(x,  draw = FALSE, ...) {

    insight::check_if_installed("ggplot2")

    dat <- get_averages(x, ...)

    # combine term and contrast to avoid overlap
    if (all(c("term", "contrast") %in% colnames(dat))) {
        dat$contrast <- gsub("mean\\(([\\w|\\/]*)\\)", "\\1", dat$contrast, perl = TRUE)
        dat$term <- ifelse(dat$contrast == "dY/dX",
                           dat$term,
                           sprintf("%s: %s", dat$term, dat$contrast))
    }

    if (isFALSE(draw)) {
        return(as.data.frame(dat))
    }

    if ("conf.low" %in% colnames(dat)) {
        if ("group" %in% colnames(dat)) {
            p <- ggplot2::ggplot(dat, ggplot2::aes(y = term,
                                                   x = estimate,
                                                   xmin = conf.low,
                                                   xmax = conf.high,
                                                   color = group))
        } else {
            p <- ggplot2::ggplot(dat, ggplot2::aes(y = term,
                                                   x = estimate,
                                                   xmin = conf.low,
                                                   xmax = conf.high))
        }
        xlab <- sprintf("Estimates with %s%% confidence intervals", sprintf("%.0f", attr(dat, "conf_level") * 100))
        p <- p +
             ggplot2::geom_pointrange() +
             ggplot2::labs(x = xlab, y = "")
    } else {
        if ("group" %in% colnames(dat)) {
            p <- ggplot2::ggplot(dat, ggplot2::aes(y = term, x = estimate, color = group))
        } else {
            p <- ggplot2::ggplot(dat, ggplot2::aes(y = term, x = estimate))
        }
        xlab <- sprintf("Estimates with %s%% confidence intervals", sprintf("%.0f", attr(dat, "conf_level") * 100))
        p <- p +
             ggplot2::geom_point() +
             ggplot2::labs(x = xlab, y = "")
    }

    # set a new theme only if the default is theme_grey. this prevents user's
    # theme_set() from being overwritten
    if (identical(ggplot2::theme_get(), ggplot2::theme_grey())) {
        p <- p + ggplot2::theme_minimal()
    }

    return(p)
}