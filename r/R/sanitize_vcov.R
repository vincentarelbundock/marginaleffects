# Normalize special user-facing shorthands before model validation and
# covariance dispatch. Ordinary vcov specifications pass through unchanged.

# `trace()`, `debug()` and coverage instrumentation replace a package function
# with a wrapper that keeps the original in its `original` slot. Comparing the
# wrapper with `identical()` fails, so peel the wrappers off both sides first.
# Test runners which instrument the namespace (scrutin, covr) would otherwise
# send `vcovUnconditional` down the ordinary estimator path, where it is called
# with the model as its `type` argument.
untrace_function <- function(f) {
    while (inherits(f, "functionWithTrace") && !is.null(attr(f, "original"))) {
        f <- attr(f, "original")
    }
    f
}

sanitize_vcov_request <- function(vcov) {
    # Most functions supplied to `vcov` are covariance estimators and are
    # called with the model later. `vcovUnconditional` is instead a request
    # constructor, so normalize its bare-function form before generic function
    # dispatch tries to call it with the model as its `type` argument.
    if (
        is.function(vcov) &&
            identical(untrace_function(vcov), untrace_function(vcovUnconditional))
    ) {
        return(vcovUnconditional())
    }
    if (
        is.character(vcov) &&
            length(vcov) == 1L &&
            !is.na(vcov) &&
            identical(tolower(vcov), "unconditional")
    ) {
        return(vcovUnconditional())
    }
    vcov
}


sanitize_vcov <- function(model, vcov) {
    # TRUE generates a warning in `insight::get_varcov` for some models
    if (isTRUE(checkmate::check_flag(vcov))) {
        return(NULL)
    }

    # no vcov matrix for bayesian models
    if (inherits(model, c("brmsfit", "stanreg", "bart"))) {
        return(NULL)
    }

    # strings should be case-insensitive
    vcov_strings <- c(
        "stata",
        "robust",
        "HC",
        "HC0",
        "HC1",
        "HC2",
        "HC3",
        "HC4",
        "HC4m",
        "HC5",
        "HAC",
        "NeweyWest",
        "kernHAC",
        "OPG",
        "satterthwaite",
        "kenward-roger"
    )
    if (
        isTRUE(checkmate::check_choice(
            hush(tolower(vcov)),
            choices = tolower(vcov_strings)
        ))
    ) {
        idx <- match(tolower(vcov), tolower(vcov_strings))
        return(vcov_strings[idx])
    }

    checkmate::assert(
        checkmate::check_null(vcov),
        checkmate::check_function(vcov),
        checkmate::check_matrix(vcov),
        checkmate::check_formula(vcov),
        checkmate::check_choice(vcov, choices = vcov_strings)
    )

    out <- vcov

    if (isTRUE(checkmate::check_function(out))) {
        # Silence output, messages, and warnings, but propagate errors: an
        # error mapped to `NULL` would silently select the default vcov.
        fun <- out
        out <- NULL
        tryCatch(
            utils::capture.output({
                out <- suppressMessages(suppressWarnings(fun(model)))
            }),
            error = function(e) {
                stop_sprintf(
                    "The function supplied to the `vcov` argument raised an error: %s",
                    conditionMessage(e)
                )
            }
        )
        if (!isTRUE(checkmate::check_matrix(out))) {
            stop(
                "The function supplied to the `vcov` argument must return a matrix.",
                call. = FALSE
            )
        }
    }

    if (isTRUE(checkmate::check_matrix(out))) {
        if (ncol(out) != nrow(out)) {
            stop("The `vcov` matrix must be square.", call. = FALSE)
        }
    }

    return(out)
}
