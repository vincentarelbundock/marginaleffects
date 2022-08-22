#' @importFrom generics glance
#' @family summary
#' @export
generics::glance

#' Glance at key characteristics of an object
#' @inheritParams tidy.marginaleffects
#' @family summary
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

#' @family summary
#' @export
glance.marginalmeans <- glance.marginaleffects

#' @family summary
#' @export
glance.predictions <- glance.marginaleffects

#' @family summary
#' @export
glance.comparisons <- glance.marginaleffects

#' @family summary
#' @export
glance.deltamethod <- glance.marginaleffects
