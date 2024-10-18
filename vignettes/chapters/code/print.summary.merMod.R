knit_print.summary.merMod <- function(x, digits = max(3, getOption("digits") - 3),
                                 correlation = NULL, symbolic.cor = FALSE,
                                 signif.stars = getOption("show.signif.stars"),
                                 ranef.comp = c("Variance", "Std.Dev."),
                                 show.resids = TRUE, ...)
{
   # .prt.methTit(x$methTitle, x$objClass)
	#.prt.family(x)
	#.prt.call(x$call); cat("\n")
	#.prt.aictab(x$AICtab); cat("\n")
	#if (show.resids)
		### need residuals.merMod() rather than residuals():
		###  summary.merMod has no residuals method
		#.prt.resids(x$residuals, digits = digits)
	#.prt.VC(x$varcor, digits = digits, useScale = x$useScale,
			#comp = ranef.comp, ...)
	#.prt.grps(x$ngrps, nobs = x$devcomp$dims[["n"]])

#{
	#.prt.methTit(x$methTitle, x$objClass)
	#.prt.family(x)
	#.prt.call(x$call); cat("\n")
	#.prt.aictab(x$AICtab); cat("\n")
    p <- nrow(x$coefficients)
    cat("\nFixed effects:\n")
    printCoefmat(x$coefficients, # too radical: zap.ind = 3, #, tst.ind = 4
                 eps.Pvalue=.001,
                 digits = digits, signif.stars = signif.stars)
    ## do not show correlation when   summary(*, correlation=FALSE)  was used:
    hasCor <- !is.null(VC <- x$vcov) && !is.null(VC@factors$correlation)
    cat('\n')
    if(is.null(correlation)) { # default
        cor.max <- getOption("lme4.summary.cor.max")
        correlation <- hasCor && p <= cor.max
        if(!correlation && p > cor.max) {
            nam <- deparse(substitute(x))
            if(length(nam) > 1 || nchar(nam) >= 32) nam <- "...."
            message(sprintf(paste(
                "\nCorrelation matrix not shown by default, as p = %d > %d.",
                "Use print(%s, correlation=TRUE)  or",
                "    vcov(%s)        if you need it\n", sep = "\n"),
                            p, cor.max, nam, nam))
        }
    }

	if (show.resids)
		## need residuals.merMod() rather than residuals():
		##  summary.merMod has no residuals method
		#.prt.resids(x$residuals, digits = digits)
		.prt.VC(x$varcor, digits = digits, useScale = x$useScale, comp = ranef.comp, ...)
        cat('\n')
		.prt.grps(x$ngrps, nobs = x$devcomp$dims[["n"]])

    if(length(x$fitMsgs) && any(nchar(x$fitMsgs) > 0)) {
        cat("fit warnings:\n"); writeLines(x$fitMsgs)
    }
    .prt.warn(x$optinfo,summary=FALSE)
    invisible(x)

}
registerS3method("knit_print", "summary.merMod", knit_print.summary.merMod)
