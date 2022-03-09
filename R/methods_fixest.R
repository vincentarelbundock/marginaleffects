#' @rdname get_predict
#' @export
get_predict.fixest <- function(model,
                               newdata = insight::get_data(model),
                               type = "response",
                               conf.level = NULL,
                               ...) {

    assert_dependency("fixest")

    # names = base, value = insight
    type <- unname(sanity_type(model, type))

    dots <- list(...)

    # some predict methods raise warnings on unused arguments
    unused <- c("normalize_dydx", "step_size", "numDeriv_method", "conf.int", "internal_call", "contrast_numeric_slope")
    dots <- dots[setdiff(names(dots), unused)]

    args <- list(
        object = model,
        newdata = newdata,
        type = type)

    if (!is.null(conf.level)) {
        args[["level"]] <- conf.level
        # interval can be "none", "confidence", or "prediction"
        if (!"interval" %in% names(dots)) {
            args[["interval"]] <- "confidence"
        } else {
            args[["interval"]] <- dots[["interval"]]
        }
    }

    args <- c(args, dots)
    fun <- stats::predict
    pred <- try(do.call("fun", args), silent = TRUE)

    # unable to compute confidence intervals; try again
    if (!is.null(conf.level) && inherits(pred, "try-error")) {
        args[["interval"]] <- "none"
        args[["level"]] <- NULL
        pred <- try(do.call("fun", args), silent = TRUE)
    }

    if (inherits(pred, "data.frame")) {
        out <- pred
        out$rowid <- seq_len(nrow(newdata))
        colnames(out)[colnames(out) == "fit"] <- "predicted"
        colnames(out)[colnames(out) == "se.fit"] <- "std.error"
        colnames(out)[colnames(out) == "ci_low"] <- "conf.low"
        colnames(out)[colnames(out) == "ci_high"] <- "conf.high"
    } else if (isTRUE(checkmate::check_atomic_vector(pred)) &&
               !inherits(pred, "try-error")) {
        out <- data.frame(
            rowid = 1:nrow(newdata),
            predicted = as.numeric(pred))
    } else {
        stop("Unable to extract predictions from a model of type `fixest`. Please report this problem, along with replicable code, on the `marginaleffects` issue tracker: https://github.com/vincentarelbundock/marginaleffects/issues")
    }

    return(out)
}
