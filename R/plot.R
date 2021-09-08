#' @title Plot average marginal effects
#' @export
plot.marginaleffects <- function(mfx, 
                                 conf.int = TRUE,
                                 conf.level = 0.95) {

    assert_dependency("ggplot2")

    dat <- tidy(mfx, conf.int = conf.int, conf.level = conf.level)

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
        xlab <- sprintf("Estimates with %s confidence intervals", sprintf("%.0f", conf.level * 100))
        p <- p + 
             ggplot2::geom_pointrange() + 
             ggplot2::labs(x = xlab, y = "Average marginal effects")
    } else {
        if ("group" %in% colnames(dat)) {
            p <- ggplot2::ggplot(dat, ggplot2::aes(y = term, x = estimate, color = group))
        } else {
            p <- ggplot2::ggplot(dat, ggplot2::aes(y = term, x = estimate))
        }
        xlab <- sprintf("Estimates with %s confidence intervals", sprintf("%.0f", conf.level * 100))
        p <- p +
             ggplot2::geom_point() +
             ggplot2::labs(x = xlab, y = "Average marginal effects")
    }

    # theme and return
    p <- p + ggplot2::theme_minimal()
    return(p) 
}
