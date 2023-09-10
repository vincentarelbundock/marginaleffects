plot_preprocess <- function(dat, v_x, v_color = NULL, v_facet = NULL, condition = NULL, modeldata = NULL) {
    for (v in names(condition$condition)) {
        fun <- function(x, lab) {
            idx <- match(x, sort(unique(x)))
            factor(lab[idx], levels = lab)
        }
        if (identical(condition$condition[[v]], "threenum")) {
            dat[[v]] <- fun(dat[[v]], c("-SD", "Mean", "+SD"))
        } else if (identical(condition$condition[[v]], "minmax")) {
            dat[[v]] <- fun(dat[[v]], c("Min", "Max"))
        } else if (identical(condition$condition[[v]], "quartile")) {
            dat[[v]] <- fun(dat[[v]], c("Q1", "Q2", "Q3"))
        }
    }
    if (get_variable_class(modeldata, v_x, "categorical")) {
        dat[[v_x]] <- factor(dat[[v_x]])
    }
    # colors, linetypes, and facets are categorical attributes
    if (isTRUE(v_color %in% colnames(dat))) {
        dat[[v_color]] <- factor(dat[[v_color]])
    }
    if (isTRUE(v_facet %in% colnames(dat))) {
        dat[[v_facet]] <- factor(dat[[v_facet]])
    }
    return(dat)
}


plot_build <- function(
    dat,
    v_x,
    v_color = NULL,
    v_facet = NULL,
    dv = NULL,
    modeldata = NULL,
    points = 0,
    rug = FALSE,
    gray = FALSE) {
        
    checkmate::assert_flag(rug)
    checkmate::assert_flag(gray)

    # create index before building ggplot to make sure it is available
    dat$marginaleffects_term_index <- get_unique_index(dat, term_only = TRUE)
    multi_variables <- isTRUE(length(unique(dat$marginaleffects_term_index)) > 1)

    p <- ggplot2::ggplot()

    if (points > 0 &&
        !get_variable_class(modeldata, v_x, "categorical") &&
        !get_variable_class(modeldata, dv, "categorical")) {
        if (!is.null(v_color) && get_variable_class(modeldata, v_color, "categorical")) {
            p <- p + ggplot2::geom_point(
                data = modeldata, alpha = points,
                ggplot2::aes(x = .data[[v_x]], y = .data[[dv]], color = factor(.data[[v_color]])))
        } else {
            p <- p + ggplot2::geom_point(
                data = modeldata, alpha = points,
                ggplot2::aes(x = .data[[v_x]], y = .data[[dv]]))
        }
    }
    
    if (isTRUE(rug)) {
        p <- p + ggplot2::geom_rug(data = modeldata, ggplot2::aes(x = .data[[v_x]]))
        if (!is.null(dv)) {
            p <- p + ggplot2::geom_rug(data = modeldata, ggplot2::aes(y = .data[[dv]]))
        }
    }

    aes_args <- list(
        x = substitute(.data[[v_x]]),
        y = substitute(estimate)
    )

    if ("conf.low" %in% colnames(dat)) {
        aes_args$ymin <- substitute(conf.low)
        aes_args$ymax <- substitute(conf.high)
    }

    aes_args_ribbon <- aes_args
    aes_args_ribbon$fill <- aes_args$color
    aes_args_ribbon$color <- NULL

    # discrete x-axis
    if (is.factor(dat[[v_x]])) {
        if (!is.null(v_color)) {
            if (gray) {
                aes_args$shape <- substitute(factor(.data[[v_color]]))
            } else {
                aes_args$color <- substitute(factor(.data[[v_color]]))
            }
        }
        aes_obj <- do.call(ggplot2::aes, aes_args)
        if ("conf.low" %in% colnames(dat)) {
            p <- p + ggplot2::geom_pointrange(
                data = dat,
                mapping = aes_obj,
                position = ggplot2::position_dodge(.15))
        } else {
            p <- p + ggplot2::geom_point(
                data = dat,
                mapping = aes_obj,
                position = ggplot2::position_dodge(.15))
        }

    # continuous x-axis
    } else {
        if (!is.null(v_color)) {
            if (gray) {
                aes_args$linetype <- substitute(factor(.data[[v_color]]))
                aes_args_ribbon$linetype <- substitute(factor(.data[[v_color]]))
            } else {
                aes_args$color <- substitute(factor(.data[[v_color]]))
                aes_args_ribbon$fill <- substitute(factor(.data[[v_color]]))
            }
        }
        aes_obj_ribbon <- do.call(ggplot2::aes, aes_args_ribbon)
        aes_args$ymin <- aes_args$ymax <- NULL
        aes_obj <- do.call(ggplot2::aes, aes_args)
        if ("conf.low" %in% colnames(dat)) {
            p <- p + ggplot2::geom_ribbon(data = dat, aes_obj_ribbon, alpha = .1)
            p <- p + ggplot2::geom_line(data = dat, aes_obj)
        }
        p <- p + ggplot2::geom_line(data = dat, aes_obj)
    }

    # facets: 3rd variable and/or multiple effects
    if (multi_variables && !is.null(v_facet)) {
        fo <- stats::as.formula(paste("~ marginaleffects_term_index +", v_facet))
        p <- p + ggplot2::facet_wrap(fo, scales = "free")
        
    } else if (multi_variables) {
        fo <- stats::as.formula("~ marginaleffects_term_index")
        p <- p + ggplot2::facet_wrap(fo, scales = "free")
        
    } else if (!is.null(v_facet)) {
        fo <- stats::as.formula(paste("~", v_facet))
        p <- p + ggplot2::facet_wrap(fo, scales = "free")
    }

    return(p)
}