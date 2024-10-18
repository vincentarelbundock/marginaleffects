knit_print.summary.glm <- function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, signif.stars = FALSE, ...)
{
    if (length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
    }
    else {
        df <- if ("df" %in% names(x))
            x[["df"]]
        else NULL
        if (!is.null(df) && (nsingular <- df[3L] - df[1L]))
            cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n",
                sep = "")
        else cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if (!is.null(aliased <- x$aliased) && any(aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4L, dimnames = list(cn,
                colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }
        printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
            na.print = "NA", eps.Pvalue=.001, ...)
    }
    cat("\n")
    invisible(x)
}
registerS3method("knit_print", "summary.glm", knit_print.summary.glm)
