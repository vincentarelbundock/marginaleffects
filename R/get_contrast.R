#' Get contrasts for factor and logical terms in a model object (internal function)
#'
#' @inheritParams marginaleffects
#' @param variable Name of the variable to contrast (string)
#' @return A `broom`-style `data.frame` with the results of the
#' `emmeans::contrast` function.
#' @keywords internal
#' @export
get_contrast <- function(model, variable, prediction_type = "response", ...) {
    assert_dependency("emmeans")
    emm <- emmeans::emmeans(model, specs = variable, type = prediction_type)
    emm <- emmeans::regrid(emm)
    emm <- emmeans::contrast(emm, method = "revpairwise")
    out <- data.frame(summary(emm))
    # dict adapted from `broom`
    dict <- list("lsmean" = "estimate",
                 "emmean" = "estimate",
                 "pmmean" = "estimate",
                 "odds.ratio" = "estimate",
                 "prediction" = "estimate",
                 "effect.size" = "estimate",
                 "SE" = "std.error",
                 "lower.CL" = "conf.low",
                 "asymp.LCL" = "conf.low",
                 "upper.CL" = "conf.high",
                 "asymp.UCL" = "conf.high",
                 "z.ratio" = "statistic",
                 "t.ratio" = "statistic",
                 "F.ratio" = "statistic",
                 "df1" = "num.df",
                 "df2" = "den.df",
                 "model term" = "term")
    for (n in names(dict)) {
        colnames(out)[colnames(out) == n] <- dict[n]
    }
    out$term <- variable
    out <- out[, colnames(out) %in% c("term", "contrast", "estimate",
                                      "std.error", "statistic", "p.value",
                                      "conf.low", "conf.high")]
    return(out)
}
