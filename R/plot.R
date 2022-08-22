#' Point-range plot of average marginal effects
#' 
#' Uses the `ggplot2` package to draw a point-range plot of the average marginal effects computed by `tidy`.
#' @inheritParams marginaleffects
#' @inheritParams tidy.marginaleffects
#' @inherit tidy.marginaleffects details
#' @return A `ggplot2` object
#' @family plot
#' @export
#'
#' @examples
#' mod <- glm(am ~ hp + wt, data = mtcars)
#' mfx <- marginaleffects(mod)
#' plot(mfx)
#'
plot.marginaleffects <- function(x,
                                 conf_level = 0.95,
                                 ...) {

    assert_dependency("ggplot2")

    dat <- tidy(x, conf_level = conf_level)

    # combine term and contrast to avoid overlap
    if (all(c("term", "contrast") %in% colnames(dat))) {
        dat$term <- ifelse(dat$contrast == "dY/dX",
                           dat$term,
                           sprintf("%s: %s", dat$term, dat$contrast))
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
        xlab <- sprintf("Estimates with %s%% confidence intervals", sprintf("%.0f", conf_level * 100))
        p <- p +
             ggplot2::geom_pointrange() +
             ggplot2::labs(x = xlab, y = "")
    } else {
        if ("group" %in% colnames(dat)) {
            p <- ggplot2::ggplot(dat, ggplot2::aes(y = term, x = estimate, color = group))
        } else {
            p <- ggplot2::ggplot(dat, ggplot2::aes(y = term, x = estimate))
        }
        xlab <- sprintf("Estimates with %s%% confidence intervals", sprintf("%.0f", conf_level * 100))
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
