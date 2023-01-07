#' @importFrom generics glance
#' @family summary
#' @noRd
#' @export
generics::glance

#' Glance at key characteristics of an object
#' @inheritParams aggregate.comparisons
#' @noRd
#' @family summary
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

#' @family summary
#' @noRd
#' @export
glance.marginalmeans <- glance.slopes

#' @family summary
#' @noRd
#' @export
glance.predictions <- glance.slopes

#' @family summary
#' @noRd
#' @export
glance.comparisons <- glance.slopes

#' @family summary
#' @noRd
#' @export
glance.hypotheses <- glance.slopes
