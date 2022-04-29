# todo:
# warn for large contrast matrix
# test nrow > 1
# test brms
# test partial cross-contrasts: variables does not include all the model terms

# test_that("vs. emmeans", {
#
#     dat <- mtcars
#     dat$cyl <- factor(dat$cyl)
#     dat$am <- as.logical(dat$am)
#     dat$gear <- as.character(dat$gear)
#     mod <- lm(mpg ~ am + cyl + wt + gear, data = dat)
#     cmp <- comparisons(
#         mod,
#         variables = c("cyl", "gear"),
#         contrast_factor = "all",
#         interaction = TRUE)
#     cmp
#     cmp <- comparisons(
#         mod,
#         variables = c("cyl", "gear"),
#         contrast_factor = "reference",
#         interaction = TRUE)
#
# cmp <- comparisons(
#         mod,
#         newdata = datagrid(),
#         variables = c("cyl", "gear"),
#         contrast_factor = "reference",
#         interaction = FALSE)
#
#
#     cmp <- comparisons(
#         mod,
#         newdata = head(dat),
#         variables = c("cyl", "gear"),
#         contrast_factor = "all",
#         interaction = TRUE)
#
#     cmp <- comparisons(
#         mod,
#         newdata = datagrid(),
#         variables = c("cyl", "gear"),
#         contrast_factor = "all",
#         interaction = TRUE)
#
#             
# cmp <- comparisons(
#     mod,
#     newdata = datagrid(),
#     variables = c("gear", "cyl"),
#     contrast_factor = "all",
#     interaction = TRUE)
#
# #
# # # variables is the full model
# # comparisons(mod,
# #             newdata = datagrid(),
# #             interaction = FALSE)
# #
# # # variables is a subset of the model
# # comparisons(mod,
# #             variables = c("am", "gear"),
# #             newdata = datagrid(am = c(TRUE, FALSE)),
# #             interaction = TRUE)
# #
# # # nrow > 1
# # comparisons(mod,
# #             newdata = datagrid(am = c(TRUE, FALSE)),
# #             interaction = TRUE)
# #
# #
#
#
# dat <- mtcar
#
# em <- emmeans(m, c("c172code_f", "e16sex_f"))
# em <- contrast(em, method = "revpairwise")
# em <- model_parameters(em)
# #
#
# cmp1 <- comparisons(m,
#                     newdata = datagrid(),
#                     variables = c("c172code_f", "e16sex_f"),
#                     contrast_factor = "all",
#                     interaction = TRUE)
# cmp1
