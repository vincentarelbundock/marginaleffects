plot_build <- function(dat, v_x, v_color = NULL, v_facet = NULL) {
    p <- ggplot2::ggplot()

    # discrete x-axis
    if (is.factor(dat[[v_x]])) {
        ggpoint <- ggplot2::geom_point(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate))
        ggpointcol <- ggplot2::geom_point(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, color = .data[[v_color]]))
        ggpointshape <- ggplot2::geom_point(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, shape = .data[[v_color]]))
        ggpointrange <- ggplot2::geom_pointrange(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high))
        ggpointrangecol <- ggplot2::geom_pointrange(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high, color = .data[[v_color]]))
        ggpointrangeshape <- ggplot2::geom_pointrange(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high, shape = .data[[v_color]]))
        if ("conf.low" %in% colnames(dat)) {
            if (is.null(v_color)) {
                p <- p + ggpointrange
            } else {
                p <- p + ggpointrangecol
            }
        } else {
            if (is.null(v_color)) {
                p <- p + ggpointcol
            } else {
                p <- p + ggpoint
            }
        }

    # continuous x-axis
    } else {
        ggrib <- ggplot2::geom_ribbon(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high), alpha = .2)
        ggribcol <- ggplot2::geom_ribbon(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high, fill = .data[[v_color]]), alpha = .2)
        ggribtype <- ggplot2::geom_ribbon(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high, group = .data[[v_color]]), alpha = .2)
        ggline <- ggplot2::geom_line(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate))
        gglinecol <- ggplot2::geom_line(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, color = .data[[v_color]]))
        gglinetype <- ggplot2::geom_line(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, linetype = .data[[v_color]]))
        if ("conf.low" %in% colnames(dat)) {
            if (is.null(v_color)) {
                p <- p + ggrib
            } else {
                p <- p + ggribcol
            }
        }
        if (is.null(v_color)) {
            p <- p + ggline
        } else {
            p <- p + gglinecol
        }
    }

    return(p)
}