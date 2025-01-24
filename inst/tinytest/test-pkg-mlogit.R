# # WARNING: standard errors are different from nnet::multinom() because stats::vcov gives a very difference matrix.
#
# # why `newdata` used to not be supported
# # here the `newdata` does not include the individual or choice variabls at all,
# # but we still get a prediction. Impossible to know what order the rows are in,
# # if `newdata` is balanced, or what group ids to give. `newdata` could be
# # completely malformed and we would still produce results. I could make strong
# # assumptions about group id being a multiple of number of rows with some
# # modulo hacks, but that's bad practice. Example:
# # nd <- TravelMode[, 3:ncol(TravelMode)]
# # predict(mod, newdata = head(nd, 12))
# source("helpers.R")
# using("marginaleffects")
# if (ON_CI) exit_file("on ci")
# requiet("nnet")
# requiet("dfidx")
# requiet("mlogit")
# requiet("data.table")
#
# TravelMode <- get_dataset("TravelMode", "AER")
# TravelMode$rownames <- NULL # {mlogit} assumes first column is the index
# mod <- mlogit(choice ~ wait + gcost | income + size, data = TravelMode)
#
# # no validity
# mod <- mlogit(choice ~ wait + gcost | income + size, TravelMode)
# cmp <- comparisons(mod)
# pre <- predictions(mod)
# tid <- tidy(cmp)
# expect_inherits(cmp, "comparisons")
# expect_inherits(pre, "predictions")
# expect_slopes(mod)
# expect_true("group" %in% colnames(tid))
#
# # error on bad newdata
# mod <- mlogit(choice ~ wait + gcost | income + size, TravelMode)
# nd <- head(TravelMode, 5)
# expect_error(comparisons(mod, newdata = nd), pattern = "number of choices")
#
# # mlogit doesn't install on Github actions, so we can't have it in DESCRIPTION,
# # but if we use the Fishing data, this raises an error in check()
#
# # vs. nnet::multinom
# data("Fishing", package = "mlogit")
# dat <- Fishing
# Fish <- dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")
# m1 <- mlogit(mode ~ 0 | income, data = Fish)
# m2 <- nnet::multinom(mode ~ income, data = Fishing, trace = FALSE)
#
# # predictions() vs. nnet::multinom()
# p1 <- predictions(m1)
# p2 <- predictions(m2, type = "probs")
# p1$group <- as.character(p1$group)
# p2$group <- as.character(p2$group)
# setDT(p1, key = c("rowid", "group"))
# setDT(p2, key = c("rowid", "group"))
# expect_equivalent(p1$estimate, p2$estimate, tolerance = 1e-5)
# expect_true(cor(p1$estimate, p2$estimate) > .98)
#
# # comparisons() vs. nnet::multinom()
# c1 <- comparisons(m1)
# c2 <- comparisons(m2, type = "probs")
# c1$group <- as.character(c1$group)
# c2$group <- as.character(c2$group)
# setDT(c1, key = c("rowid", "term", "group"))
# setDT(c2, key = c("rowid", "term", "group"))
# expect_equivalent(c1$estimate, c2$estimate, tolerance = 1e-5)
# expect_true(cor(c1$estimate, c2$estimate) > .98)
#
# # slopes() vs. nnet::multinom()
# mfx1 <- slopes(m1)
# mfx2 <- slopes(m2, type = "probs")
# mfx1$group <- as.character(mfx1$group)
# mfx2$group <- as.character(mfx2$group)
# setDT(mfx1, key = c("rowid", "term", "group"))
# setDT(mfx2, key = c("rowid", "term", "group"))
# expect_equivalent(mfx1$estimate, mfx2$estimate, tolerance = 1e-5)
# expect_true(cor(mfx1$estimate, mfx2$estimate) > .98)
#
# # Issue #551
# mod1 <- mlogit(choice ~ wait + gcost | income + size, TravelMode) 
# mfx <- slopes(mod1, variables = c("income", "size"))
# expect_inherits(mfx, "marginaleffects")
#
# TravelMode$dsize <- ifelse(TravelMode$size == "1", 1, 0)
# mod2 <- mlogit(choice ~ wait + gcost | income + dsize, TravelMode) 
# mfx <- slopes(mod2, variables = c("income", "dsize"))
# expect_inherits(mfx, "marginaleffects")
#
# TravelMode$dsize <- as.factor(TravelMode$dsize)
# mod3 <- mlogit(choice ~ wait + gcost | income + dsize, TravelMode) 
# mfx <- slopes(mod3, variables = c("income", "dsize"))
# expect_inherits(mfx, "marginaleffects")
#
# # Issue #1086
# requiet("dplyr")
# chocolate <- read.csv("modelarchive/data-raw/choco_candy.csv") |>
#   mutate(
#     dark = case_match(dark, 0 ~ "Milk", 1 ~ "Dark"),
#     dark = factor(dark, levels = c("Milk", "Dark")),
#     soft = case_match(soft, 0 ~ "Chewy", 1 ~ "Soft"),
#     soft = factor(soft, levels = c("Chewy", "Soft")),
#     nuts = case_match(nuts, 0 ~ "No nuts", 1 ~ "Nuts"),
#     nuts = factor(nuts, levels = c("No nuts", "Nuts"))
#   )
# chocolate_idx <- dfidx(
#   chocolate,
#   idx = list("subj", "alt"),
#   choice = "choice",
#   shape = "long"
# )
# m <- mlogit(
#   choice ~ dark + soft + nuts | 0 | 0, 
#   data = chocolate_idx
# )
# by <- data.frame(dark = c("Milk", "Dark"), by = c("Milk", "Dark"))
# p <- predictions(m, newdata = chocolate, by = by)
# expect_inherits(p, "predictions")
# expect_equivalent(p$estimate, c(0.0500000000082118, 0.199999999991788), tolerance = 1e-5)
# expect_equivalent(p$std.error, c(0.0316227771712602, 0.0316227779679258), tolerance = 1e-5)
#
#
#
# # Issue #1125: regression due to factor conversion
# requiet("AER")
# requiet("mlogit")
# data("TravelMode", package = "AER")
# mod <- mlogit(choice ~ wait + gcost | income + size, TravelMode)
# s <- avg_slopes(mod, variables = c("income", "size"))
# expect_true(all(c("air", "bus", "car", "train") %in% s$group))
#
#
#
# rm(list = ls())
#
#
