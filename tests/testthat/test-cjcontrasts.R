# todo:
# prune crosspair to avoid reverse duplicates: cleaner way and not the abs() dup hack
# warn for large contrast matrix
# test nrow > 1
# test brms
# test partial cross-contrasts: variables does not include all the model terms

#
# dat <- mtcars
# dat$cyl <- factor(dat$cyl)
# dat$am <- as.logical(dat$am)
# dat$gear <- as.character(dat$gear)
# mod <- lm(mpg ~ am + cyl + wt + gear, data = dat)
#
# # variables is the full model
# comparisons(mod,
#             newdata = datagrid(),
#             crosscontrast = FALSE)
#
# # variables is a subset of the model
# comparisons(mod,
#             variables = c("am", "gear"),
#             newdata = datagrid(am = c(TRUE, FALSE)),
#             crosscontrast = TRUE)
#
# # nrow > 1
# comparisons(mod,
#             newdata = datagrid(am = c(TRUE, FALSE)),
#             crosscontrast = TRUE)
#
#

# test_that("vs. emmeans", {
#
# requiet("emmeans")
# requiet("datawizard")
# requiet("parameters")
# data(efc, package = "datawizard")
# efc <- data_to_factor(efc, select = c("c172code", "e16sex"), append = TRUE)
# efc$e16sex <- factor(efc$e16sex)
# efc$c172code_f <- data_recode(efc$c172code_f, list(`low` = 1:2, `high` = 3))
# m <- lm(neg_c_7 ~ c172code_f * e16sex_f, data = efc)
#
# em <- emmeans(m, c("c172code_f", "e16sex_f"))
# em <- contrast(em, method = "revpairwise")
# em <- model_parameters(em)
#

# cmp1 <- comparisons(m,
#                     newdata = datagrid(),
#                     variables = c("c172code_f", "e16sex_f"),
#                     contrast_factor = "crosspair",
#                     crosscontrast = TRUE)
# cmp1

# cmp2 <- comparisons(m,
#                     newdata = datagrid(),
#                     variables = c("c172code_f", "e16sex_f"))
# cmp1
# cmp2
#
#  cmp model_parameters()   browser()
#
#
