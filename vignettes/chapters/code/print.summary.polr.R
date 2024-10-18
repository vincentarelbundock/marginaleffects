knit_print.summary.polr <- function (x, digits = x$digits, ...)
{
    coef <- format(round(x$coefficients, digits = digits))
    pc <- x$pc
    if (pc > 0) {
        cat("\nCoefficients:\n")
        print(x$coefficients[seq_len(pc), , drop = FALSE], quote = FALSE,
            digits = digits, ...)
    }
    else {
        cat("\nNo coefficients\n")
    }
    cat("\nIntercepts:\n")
    print(coef[(pc + 1L):nrow(coef), , drop = FALSE], quote = FALSE,
        digits = digits, ...)
    invisible(x)
}
registerS3method("knit_print", "summary.polr", knit_print.summary.polr)
