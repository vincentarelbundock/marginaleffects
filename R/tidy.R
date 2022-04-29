#' @importFrom generics tidy
#' @export
generics::tidy


#' @importFrom generics glance
#' @export
generics::glance


#' Tidy a `marginaleffects` object
#'
#' @param x An object produced by the `marginaleffects` function.
#' @param conf.int Logical indicating whether or not to include a confidence interval.
#' @param by Character vector of variable names over which to compute group-averaged marginal effects.
#' @param FUN function used to summarize unit-level marginal effects (e.g., `mean` or
#' `median`). The default value `NULL` uses the `mean` function. See Details
#' below.
#' @inheritParams marginaleffects
#' @return A "tidy" `data.frame` of summary statistics which conforms to the
#' `broom` package specification.
#' @details
#' The `tidy` function calculates average marginal effects by taking the mean
#' of all the unit-level marginal effects computed by the `marginaleffects`
#' function.
#'
#' In frequentist models, the `FUN` function is applied to the unit-level
#' marginal effects. If `FUN` is the mean function (or `NULL` default), this
#' yields an "Average Marginal Effect". If `FUN` is the median function, this
#' yields a "Median Marginal Effect".
#' 
#' To compute standard errors around those quantities, we begin by applying the
#' `FUN` function to each column of the Jacobian. When `FUN` is the mean, this
#' yields a "Jacobian at the mean". Then, we use this matrix in the Delta
#' method to obtained standard errors.
#'
#' In Bayesian models (e.g., `brms`), we compute Average (or Median) Marginal
#' Effects by applying the `FUN` function twice. First, we apply `FUN` to all
#' marginal effects for each posterior draw, thereby estimating one Average (or
#' Median) Marginal Effect per iteration of the MCMC chain. Second, we apply
#' `FUN` and the `quantile` function to the results of Step 1 to obtain the
#' Average (or Median) Marginal Effect and its associated interval.
#'
#' @export
#' @examples
#' mod <- lm(mpg ~ hp * wt + factor(gear), data = mtcars)
#' mfx <- marginaleffects(mod)
#'
#' # average marginal effects
#' tidy(mfx)
#'
#' # average marginal effects by group
#' tidy(mfx, by = "gear")
tidy.marginaleffects <- function(x,
                                 conf.int = TRUE,
                                 conf.level = 0.95,
                                 by = NULL,
                                 FUN = NULL,
                                 ...) {
    x_dt <- copy(x)
    setnames(x_dt, old = "dydx", new = "comparison")
    out <- tidy.comparisons(x_dt,
                            conf.int = conf.int,
                            conf.level = conf.level,
                            by = by,
                            FUN = FUN,
                            ...)
    return(out)
}


#' @export
glance.marginaleffects <- function(x, ...) {
    assert_dependency("modelsummary")
    model <- attr(x, "model")
    gl <- suppressMessages(suppressWarnings(try(
        modelsummary::get_gof(model, ...), silent = TRUE)))
    if (inherits(gl, "data.frame")) {
        out <- data.frame(gl)
    } else {
        out <- NULL
    }
    vcov.type <- attr(x, "vcov.type")
    if (is.null(out) && !is.null(vcov.type)) {
        out <- data.frame("vcov.type" = vcov.type)
    } else if (!is.null(out)) {
        out[["vcov.type"]] <- vcov.type
    }
    return(out)
}


#' Tidy a `marginalmeans` object
#'
#' @param x An object produced by the `marginalmeans` function.
#' @inheritParams tidy.marginaleffects
#' @return A "tidy" `data.frame` of summary statistics which conforms to the
#' `broom` package specification.
#' @export
tidy.marginalmeans <- function(x,
                               conf.int = TRUE,
                               conf.level = 0.95,
                               ...) {

    out <- x
    colnames(out)[colnames(out) == "marginalmean"] <- "estimate"

    if (!"statistic" %in% colnames(out) && "std.error" %in% colnames(out)) {
        out$statistic <- out$estimate / out$std.error
    }

    if (!"p.value" %in% colnames(out) && "std.error" %in% colnames(out)) {
        out$p.value <- 2 * (1 - stats::pnorm(abs(out$statistic)))
    }

    out <- out

    # confidence intervals
    if ("std.error" %in% colnames(out)) {
        if (isTRUE(conf.int) && !"conf.low" %in% colnames(out)) {
            alpha <- 1 - conf.level
            out$conf.low <- out$estimate + stats::qnorm(alpha / 2) * out$std.error
            out$conf.high <- out$estimate - stats::qnorm(alpha / 2) * out$std.error
        }
    }

    # sort and subset columns
    cols <- c("type", "term", "value", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
    out <- out[, intersect(cols, colnames(out)), drop = FALSE]
    out <- as.data.frame(out)

    attr(out, "conf.level") <- conf.level

    return(out)
}


#' @export
glance.marginalmeans <- glance.marginaleffects


#' @export
glance.predictions <- glance.marginaleffects


#' @export
glance.comparisons <- glance.marginaleffects


#' Tidy a `predictions` object
#'
#' Calculate average adjusted predictions by taking the mean of all the
#' unit-level adjusted predictions computed by the `predictions` function.
#'
#' @param x An object produced by the `predictions` function.
#' @inheritParams predictions
#' @return A "tidy" `data.frame` of summary statistics which conforms to the
#' `broom` package specification.
#' @export
#' @examples
#' mod <- lm(mpg ~ hp * wt + factor(gear), data = mtcars)
#' mfx <- predictions(mod)
#' tidy(mfx)
tidy.predictions <- function(x, ...) {

    # Average Adjusted Predictions
    idx <- intersect(colnames(x), c("type", "group"))
    out <- data.table(x)[, .(estimate = mean(predicted, na.rm = TRUE)), by = idx]

    ## This might be a useful implementation of weights
    # if (is.null(attr(x, "weights"))) {
    #     dydx <- stats::aggregate(f, data = x, FUN = mean)
    # } else {
    #     dydx <- stats::aggregate(f, data = x, FUN = weighted.mean, w = attr(x, "weights"))
    # }

    setDF(out)
    return(out)
}


#' Tidy a `comparisons` object
#'
#' Calculate average contrasts by taking the mean of all the
#' unit-level contrasts computed by the `predictions` function.
#'
#' @param x An object produced by the `comparisons` function.
#' @param by Character vector of variable names over which to compute group-averaged contrasts.
#' @param FUN function used to summarize unit-level contrasts (e.g., `mean` or
#' `median`). The default value `NULL` uses the `mean` function. See Details
#' below.
#' @inheritParams comparisons
#' @inheritParams tidy.marginaleffects
#' @return A "tidy" `data.frame` of summary statistics which conforms to the
#' `broom` package specification.
#' @details
#'
#' In frequentist models, the `FUN` function is applied to the unit-level
#' marginal effects. If `FUN` is the mean function (or `NULL` default), this
#' yields an "Average Marginal Effect". If `FUN` is the median function, this
#' yields a "Median Marginal Effect".
#' 
#' To compute standard errors around those quantities, we begin by applying the
#' `FUN` function to each column of the Jacobian. When `FUN` is the mean, this
#' yields a "Jacobian at the mean". Then, we use this matrix in the Delta
#' method to obtained standard errors.
#'
#' In Bayesian models (e.g., `brms`), we compute Average (or Median) Marginal
#' Effects by applying the `FUN` function twice. First, we apply `FUN` to all
#' marginal effects for each posterior draw, thereby estimating one Average (or
#' Median) Marginal Effect per iteration of the MCMC chain. Second, we apply
#' `FUN` and the `quantile` function to the results of Step 1 to obtain the
#' Average (or Median) Marginal Effect and its associated interval.
#'
#' @export
#' @examples
#' mod <- lm(mpg ~ factor(gear), data = mtcars)
#' contr <- comparisons(mod, contrast_factor = "sequential")
#' tidy(contr)
tidy.comparisons <- function(x,
                             conf.int = TRUE,
                             conf.level = 0.95,
                             by = NULL,
                             FUN = NULL,
                             ...) {

    checkmate::assert_numeric(conf.level, len = 1)
    checkmate::assert_true(conf.level > 0)
    checkmate::assert_true(conf.level < 1)
    checkmate::assert_flag(conf.int)
    checkmate::assert_character(by, null.ok = TRUE)
    checkmate::assert_function(FUN, null.ok = TRUE)

    # custom summary function
    if (is.null(FUN)) {
        FUN <- mean
        FUN_label <- "mean"
    } else {
        FUN_label <- NULL
    }

    # group averages
    if (!is.null(by)) {
        flag <- isTRUE(checkmate::check_character(by)) &&
                isTRUE(checkmate::check_true(all(by %in% colnames(x))))
        if (!isTRUE(flag)) {
            msg <- "The `by` argument must be a character vector and every element of the vector must correspond to a column name in the `x` marginal effects object."
            stop(msg, call. = FALSE)
        }
    }

    x_dt <- data.table(x)

    # empty initial mfx data.frame means there were no numeric variables in the
    # model
    if ("term" %in% colnames(x_dt)) {

        J <- attr(x, "J")
        V <- attr(x, "vcov")
        draws <- attr(x, "posterior_draws")

        idx_by <- c("type", "group", "term", "contrast", by,
                    grep("^contrast_\\w+", colnames(x_dt), value = TRUE))
        idx_by <- intersect(idx_by, colnames(x_dt))
        idx_na <- is.na(x_dt$comparison)

        # average marginal effects
        ame <- x_dt[idx_na == FALSE, .(estimate = FUN(comparison, na.rm = TRUE)), by = idx_by]

        if (is.matrix(J) && is.matrix(V)) {
            # Jacobian at the group mean
            # use weird colnames to avoid collision
            idx_pad <- x_dt[, ..idx_by]
            idx_col_old <- colnames(idx_pad)
            idx_col_new <- paste0(idx_col_old, "_marginaleffects_index")
            setnames(idx_pad,
                     old = colnames(idx_pad),
                     new = paste0(colnames(idx_pad), "_marginaleffects_index"))

            J <- data.table(idx_pad, J)

            J <- J[idx_na == FALSE,]
            x_dt <- x_dt[idx_na == FALSE,]


            tmp <- paste0(idx_by, "_marginaleffects_index")
            J_mean <- J[, lapply(.SD, FUN, na.rm = TRUE), by = tmp]
            J_mean <- J_mean[, !..tmp]
            J_mean <- as.matrix(J_mean)

            # HACK: align J_mean and V if they don't match
            if (all(colnames(J_mean) %in% colnames(V))) {
                V <- V[colnames(J_mean), colnames(J_mean)]
            }

            # standard errors at the group mean
            se <- sqrt(colSums(t(J_mean %*% V) * t(J_mean)))
            ame[, std.error := se]

        } else if (!is.null(draws)) {
            draws <- posteriordraws(x)
            setDT(draws)
            ame[, "estimate" := NULL]
            idx_by <- intersect(colnames(draws), idx_by)

            # uncertainty around the average marginal effect in two steps:
            # 1. mean for each draw gives 4000 samples of the average mfx
            # 2. quantiles of the means
            drawavg <- draws[, .(estimate = FUN(draw)), by = c(idx_by, "drawid")]
            es <- drawavg[, .(estimate = FUN(estimate)), by = idx_by]
            if (isTRUE(getOption("marginaleffects_credible_interval", default = "eti") == "hdi")) {
                f_ci <- get_hdi
            } else {
                f_ci <- get_eti
            }
            ci <- drawavg[, as.list(f_ci(estimate, credMass = conf.level)), by = idx_by]
            setnames(ci, old = c("lower", "upper"), new = c("conf.low", "conf.high"))
            ame <- merge(merge(ame, es, sort = FALSE), ci, sort = FALSE)
        }

    } else {
        # avoids namespace conflict with `margins`
        ame <- data.frame()
    }

    if (!"statistic" %in% colnames(ame) && "std.error" %in% colnames(ame)) {
        ame$statistic <- ame$estimate / ame$std.error
    }

    if (!"p.value" %in% colnames(ame) && "std.error" %in% colnames(ame)) {
        ame$p.value <- 2 * (1 - stats::pnorm(abs(ame$statistic)))
    }

    out <- ame

    # confidence intervals
    if ("std.error" %in% colnames(out)) {
        if (isTRUE(conf.int) && !"conf.low" %in% colnames(out)) {
            alpha <- 1 - conf.level
            out$conf.low <- out$estimate + stats::qnorm(alpha / 2) * out$std.error
            out$conf.high <- out$estimate - stats::qnorm(alpha / 2) * out$std.error
        }
    }

    # remove terms with precise zero estimates. typically the case in
    # multi-equation models where some terms only affect one response
    out <- out[out$estimate != 0,]

    # sort and subset columns
    cols <- c("type", "group", "term", "contrast", by,
              grep("^contrast_\\w+", colnames(x_dt), value = TRUE),
              "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
    cols <- intersect(cols, colnames(out))
    out <- out[, cols, drop = FALSE, with = FALSE]

    setDF(out)

    attr(out, "conf.level") <- conf.level
    attr(out, "FUN") <- FUN_label

    if (exists("J_mean")) {
        attr(out, "J") <- J_mean
    }

    return(out)
}
