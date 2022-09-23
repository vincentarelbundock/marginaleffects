#' @rdname get_predict
#' @export
get_predict.fixest <- function(model,
                               newdata = insight::get_data(model),
                               vcov = FALSE,
                               conf_level = 0.95,
                               type = "response",
                               ...) {

    assert_dependency("fixest")

    # names = base, value = insight
    type <- unname(sanitize_type(model, type))

    dots <- list(...)

    # some predict methods raise warnings on unused arguments
    unused <- c("normalize_dydx", "step_size", "numDeriv_method", "conf.int", "internal_call", "contrast_numeric_slope")
    dots <- dots[setdiff(names(dots), unused)]

    args <- list(
        object = model,
        newdata = newdata,
        type = type)

    if (!is.null(conf_level)) {
        args[["level"]] <- conf_level
        # interval can be "none", "confidence", or "prediction"
        if (!"interval" %in% names(dots)) {
            args[["interval"]] <- "confidence"
        } else {
            args[["interval"]] <- dots[["interval"]]
        }

        # vcov
        if (!isTRUE(checkmate::check_matrix(vcov, null.ok = TRUE))) {
            V <- get_vcov(model, vcov = vcov)
        } else {
            V <- NULL
        }

        if (isTRUE(checkmate::check_matrix(V))) {
            args[["vcov"]] <- V
        }
    }

    # args <- c(args, dots)
    # fun <- stats::predict
    # pred <- try(do.call("fun", args), silent = TRUE)

    # fixest is super slow when using do call because of some `deparse()` call
    pred <- try(stats::predict(
        object = args$object,
        newdata = args$newdata,
        type = args$type,
        interval = args$interval,
        level = args$level,
        vcov = args$vcov,
        ...),
    silent = TRUE)

    # unable to compute confidence intervals; try again
    if (!is.null(conf_level) && inherits(pred, "try-error")) {
        args[["interval"]] <- "none"
        args[["level"]] <- NULL
        # fixest is super slow when using do call because of some `deparse()` call
        pred <- try(stats::predict(
            object = args$object,
            newdata = args$newdata,
            type = args$type,
            interval = args$interval,
            vcov = args$vcov,
            ...),
        silent = TRUE)
    }

    if (inherits(pred, "data.frame")) {
        out <- pred
        if (!"rowid" %in% colnames(out)) {
            if ("rowid" %in% colnames(newdata)) {
                out$rowid <- newdata$rowid
            } else {
                out$rowid <- seq_len(nrow(newdata))
            }
        }
        colnames(out)[colnames(out) == "fit"] <- "predicted"
        colnames(out)[colnames(out) == "se.fit"] <- "std.error"
        colnames(out)[colnames(out) == "ci_low"] <- "conf.low"
        colnames(out)[colnames(out) == "ci_high"] <- "conf.high"
    } else if (isTRUE(checkmate::check_atomic_vector(pred)) && !inherits(pred, "try-error")) {
        if ("rowid" %in% colnames(newdata)) {
            out <- data.frame(
                rowid = newdata$rowid,
                predicted = as.numeric(pred))
        } else {
            out <- data.frame(
                rowid = 1:nrow(newdata),
                predicted = as.numeric(pred))
        }
    } else {
        if (inherits(pred, "try-error")) {
            stop(as.character(pred), call. = FALSE)
        }
        msg <- format_msg(
        "Unable to extract predictions from a model of type `fixest`. Please
        report this problem, along with replicable code, on the `marginaleffects` issue tracker:
        https://github.com/vincentarelbundock/marginaleffects/issues")
        stop(msg, call. = FALSE)
    }

    return(out)
}
