#' @importFrom generics glance
#' @export
generics::glance


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