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
    con <- emmeans::contrast(emm, method = "revpairwise")
    out <- data.frame(summary(con))
    colnames(out)[colnames(out) == "SE"] <- "std.error"
    colnames(out)[colnames(out) == "t.ratio"] <- "statistic"
    out$term <- variable
    out <- out[, colnames(out) %in% c("term", "contrast", "estimate",
                                      "std.error", "statistic", "p.value",
                                      "conf.low", "conf.high")]
    return(out)
}
