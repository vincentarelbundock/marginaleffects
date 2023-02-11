source("helpers.R")
using("marginaleffects")

exit_if_not(requiet("afex"))
exit_if_not(requiet("emmeans"))

data(md_12.1, package = "afex")
mod <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))

# no validity
expect_slopes(mod)
pre <- predictions(mod)
expect_inherits(pre, "predictions")
cmp <- comparisons(mod)
expect_inherits(cmp, "comparisons")

# marginalmeans vs. emmeans
mm <- marginal_means(
    mod,
    variables = c("angle", "noise"),
    cross = FALSE)
em1 <- data.frame(emmeans(mod, ~angle))
em2 <- data.frame(emmeans(mod, ~noise))
expect_equal(mm$estimate, c(em1$emmean, em2$emmean))
expect_equal(mm$std.error, c(em1$SE, em2$SE))

# contrasts vs emmeans
cmp <- comparisons(mod,
    variables = "angle",
    newdata = "marginalmeans")
em <- emmeans(mod, ~angle)
em <- emmeans::contrast(em, method = "trt.vs.ctrl1")
em <- data.frame(em)
expect_equal(cmp$estimate, em$estimate)
expect_equal(cmp$std.error, em$SE)

# predictions vs emmeans
pre <- predictions(
    mod,
    newdata = datagrid(angle = c("X0", "X4", "X8"),
                       noise = md_12.1$noise))
emm <- emmeans(mod, c("noise", "angle"))
emm <- data.frame(emm)
expect_equivalent(pre$estimate, emm$emmean)
expect_equivalent(pre$std.error, emm$SE)


# coefficient matrix (ANOVA on full design)
data(obk.long, package = "afex")
mod <- suppressMessages(aov_car(
    value ~ treatment * gender + Error(id/(phase*hour)), 
    data = obk.long, observed = "gender"))

em <- data.frame(emmeans(mod, ~ phase))
mm <- marginal_means(mod, "phase")
expect_equivalent(mm$estimate, em$emmean)
expect_equivalent(mm$std.error, em$SE)



rm(list = ls())