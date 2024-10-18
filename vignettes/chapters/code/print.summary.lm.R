knit_print.summary.lm <- 
function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
    signif.stars = FALSE, ...) 
{
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
    if (length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
    }
    else {
#        if (nsingular <- df[3L] - df[1L]) 
            #cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", 
                #sep = "")
#        else cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if (any(aliased <- x$aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, 
                colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }
        printCoefmat(coefs, digits = digits, signif.stars = signif.stars, 
            na.print = "NA", eps.Pvalue=.001, ...)
    }
 #   cat("\nResidual standard error:", format(signif(x$sigma, 
        #digits)), "on", rdf, "degrees of freedom")
    #cat("\n")
    #if (nzchar(mess <- naprint(x$na.action))) 
 #       cat("  (", mess, ")\n", sep = "")
#    if (!is.null(x$fstatistic)) {
        #cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
        #cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared, 
            #digits = digits), "\nF-statistic:", formatC(x$fstatistic[1L], 
            #digits = digits), "on", x$fstatistic[2L], "and", 
            #x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L], 
                #x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE), 
                #digits = digits))
        #cat("\n")
#    }
    cat("\n")
    invisible(x)
}
registerS3method("knit_print", "summary.lm", knit_print.summary.lm)
