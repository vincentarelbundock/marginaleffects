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
    insight::check_if_installed("tibble")
    out <- tibble::as_tibble(x)
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
tidy.hypotheses <- tidy.comparisons


#' @noRd
#' @export
glance.slopes <- function(x, ...) {
    insight::check_if_installed("insight")
    insight::check_if_installed("modelsummary")
    model <- tryCatch(attr(x, "model"), error = function(e) NULL)
    mfx <- attr(x, "mfx")
    if (is.null(model) || isTRUE(checkmate::check_string(model))) {
        model <- tryCatch(mfx@call[["model"]], error = function(e) NULL)
    }
    gl <- suppressMessages(suppressWarnings(try(
        modelsummary::get_gof(model, ...),
        silent = TRUE
    )))
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
    out <- tibble::as_tibble(out)
    return(out)
}


#' @noRd
#' @export
glance.predictions <- glance.slopes


#' @noRd
#' @export
glance.comparisons <- glance.slopes


#' @noRd
#' @export
glance.hypotheses <- glance.slopes
