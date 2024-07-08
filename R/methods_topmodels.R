#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.promodel <- function(model,
                                newdata = NULL,
                                type = "mean",
                                ...) {
    insight::check_if_installed("topmodels")
    type <- type[1L]

    # avoid breakage in get_vcov()
    mod <- model
    class(mod) <- setdiff(class(mod), "promodel")

    # pass known arguments to avoid breakage in procast()
    dots <- list(...)
    dots <- dots[intersect(names(dots), names(formals(topmodels::procast)))]
    args <- list(object = mod, newdata = newdata, type = type)
    args <- c(args, dots)
    pc <- do.call(topmodels::procast, args)

    # rename prediction and add row id
    names(pc)[[1L]] <- "estimate"
    pc$rowid <- seq_len(NROW(pc))
    return(pc)
}


#' @rdname get_vcov
#' @export
get_vcov.promodel <- function(model, ...) {
    mod <- model
    class(mod) <- setdiff(class(mod), "promodel")
    out <- get_vcov(mod)
    return(out)
}
