#' @importFrom generics glance
#' @export
generics::glance

#' Glance at key characteristics of an object
#' @inheritParams tidy.marginaleffects
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

#' @export
glance.marginalmeans <- glance.marginaleffects

#' @export
glance.predictions <- glance.marginaleffects

#' @export
glance.comparisons <- glance.marginaleffects

#' @export
glance.deltamethod <- glance.marginaleffects
