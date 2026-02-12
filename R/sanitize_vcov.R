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
        "unconditional",
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
        out <- hush(out(model))
    }

    if (isTRUE(checkmate::check_matrix(out))) {
        if (ncol(out) != nrow(out)) {
            stop("The `vcov` matrix must be square.", call. = FALSE)
        }
    }

    return(out)
}
