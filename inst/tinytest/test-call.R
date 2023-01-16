source("helpers.R")

# recall captures calls to avoid evaluating twice
modd <<- lm(mpg ~ hp + factor(gear), data = mtcars)
cmp1 <- comparisons(modd)
cmp1 <- averages(cmp1)
cmp2 <- averages(comparisons(modd))[, seq_along(cmp1)]
cmp3 <- comparisons(modd) |> averages()
for (col in c("estimate", "std.error", "p.value", "conf.high")) {
    expect_equivalent(cmp1[[col]], cmp2[[col]])
    expect_equivalent(cmp1[[col]], cmp3[[col]])
}

suppressWarnings(rm("modd", .GlobalEnv))
suppressWarnings(rm("modd"))



# #### Are caught calls roughly twice as fast?
# long_avg <- function() {
#     cmp1 <- comparisons(mod)
#     averages(cmp1)
# }
# long_sum <- function() {
#     cmp1 <- comparisons(mod)
#     summary(cmp1)
# }
# long_tid <- function() {
#     cmp1 <- comparisons(mod)
#     tidy(cmp1)
# }
# bench::mark(
#     long_avg(),
#     averages(comparisons(mod)),
#     comparisons(mod) |> averages(),
#     long_tid(),
#     tidy(comparisons(mod)),
#     comparisons(mod) |> tidy(),
#     long_sum(),
#     comparisons(mod) |> averages() |> summary(), # I expected this to be faster but not the case
#     summary(comparisons(mod)),
#     comparisons(mod) |> summary(),
#     check = FALSE,
#     iterations = 25
# )
