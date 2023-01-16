#' @importFrom generics tidy
#' @export
generics::tidy


#' @importFrom generics glance
#' @export
generics::glance



#' tidy helper
#' 
#' @noRd
#' @export
tidy.comparisons <- function(x, ...) {
    if ("transform_avg" %in% names(list(...))) {
        insight::format_error("The `transform_avg` argument is deprecated. Use `transform_post` instead.")
    }
    out <- averages(x, ...)
    class(out) <- setdiff(class(out), "averages")
    return(out)
}


#' tidy helper
#' 
#' @noRd
#' @export
tidy.slopes <- tidy.comparisons


#' tidy helper
#' 
#' @noRd
#' @export
tidy.predictions <- tidy.comparisons

#' tidy helper
#' 
#' @noRd
#' @export
tidy.hypotheses <- function(x, ...) {
    if (any(!c("term", "estimate") %in% colnames(x)) || !inherits(x, c("hypotheses", "deltamethod", "data.frame"))) {
        insight::format_error("The `tidy()` method only supports `hypotheses` objects produced by the `marginaleffects::hypotheses()` function.")
    }
    # the object is already in a tidy format. We need this method for
    # `modelsummary` and other functions that rely on `tidy()`.
    class(x) <- setdiff(class(x), "averages")
    return(x)
}


#' tidy helper
#' 
#' @noRd
#' @export
tidy.marginalmeans <- function(x, ...) {
    if ("transform_avg" %in% names(list(...))) {
        insight::format_error("The `transform_avg` argument is deprecated. Use `transform_post` instead.")
    }
    first = c("type", "term", "value", "estimate", "std.error",
    "statistic", "p.value", "conf.low", "conf.high")
    out <- sort_columns(x, first)
    class(out) <- setdiff(class(out), "averages")
    attr(out, "conf_level") <- attr(x, "conf_level")
    return(out)
}


#' tidy helper
#' 
#' @noRd
#' @export
tidy.hypotheses <- function(x, ...) {
    out <- recall(x, ...)
    return(out)
}


#' @noRd
#' @export
glance.slopes <- function(x, ...) {
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


#' @noRd
#' @export
glance.marginalmeans <- glance.slopes

#' @noRd
#' @export
glance.predictions <- glance.slopes

#' @noRd
#' @export
glance.comparisons <- glance.slopes

#' @noRd
#' @export
glance.hypotheses <- glance.slopes