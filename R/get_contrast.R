#' @export
get_contrast <- function(model, variable, ...) {
    assert_dependency("emmeans")
    emm <- emmeans::emmeans(model, specs = variable)
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
