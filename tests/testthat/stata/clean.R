library(haven)
library(ivreg)
library(betareg)
library(MASS)
library(data.table)
library(testthat)
library(readxl)

results <- list()

#  stats::glm
tmp <- read.table(test_path("stata/results/stats_glm_01.txt"), sep = "\t", skip = 4) |>
    setNames(c("term", "dydxstata", "std.errorstata")) |>
    head(2)
results[["stats_glm_01"]] <- tmp

#  stats::lm
tmp <- read.table(test_path("stata/results/stats_lm_01.txt"), sep = "\t", skip = 4) |>
    setNames(c("term", "dydxstata", "std.errorstata")) |>
    head(2)
results[["stats_lm_01"]] <- tmp

#  MASS::polr 
tmp <- read.table(test_path("stata/results/MASS_polr_01.txt"), sep = "\t", skip = 4) |>
    setNames(c("group", "dydxstata_x1", "std.errorstata_x1", "dydxstata_x2", "std.errorstata_x2")) |>
    head(3) |>
    transform(group = 2:4) |>
    data.table()
cols <- names(tmp)
tmp[, (cols) := lapply(.SD, as.numeric)]
tmp <- melt(tmp, id.vars = "group")
tmp[, c("statistic", "term") := tstrsplit(variable, "_")]
tmp[, variable := NULL]
tmp <- dcast(tmp, group + term ~ statistic, value.var = "value")
results[["MASS_polr_01"]] <- tmp

#  ivreg::ivreg
tmp <- read.table(test_path("stata/results/ivreg_ivreg_01.txt"), sep = "\t", skip = 4) |>
    setNames(c("term", "dydxstata", "std.errorstata")) |>
    head(2)
results[["ivreg_ivreg_01"]] <- tmp

#  betareg::betareg 
tmp <- read.table(test_path("stata/results/betareg_betareg_01.txt"), sep = "\t", skip = 4) |>
    setNames(c("term", "dydxstata", "std.errorstata")) |>
    head(1)
results[["betareg_betareg_01"]] <- tmp

#  MASS::glm.nb
tmp <- read.table(test_path("stata/results/MASS_glm_nb.txt"), sep = "\t", skip = 4) |>
    setNames(c("term", "dydxstata", "std.errorstata")) |>
    head(1)
results[["mass_glm_nb"]] <- tmp

#  AER::tobit
tmp <- read.table(test_path("stata/results/AER_tobit.txt"), sep = "\t", skip = 4) |>
    setNames(c("term", "dydxstata", "std.errorstata")) |>
    head(5)
results[["aer_tobit"]] <- tmp

#  AER::tobit (right-censured)
tmp <- read.table(test_path("stata/results/AER_tobit_right4.txt"), sep = "\t", skip = 4) |>
    setNames(c("term", "dydxstata", "std.errorstata")) |>
    head(5)
results[["aer_tobit_right"]] <- tmp

#  estimatr::lm_robust
tmp <- read.table(test_path("stata/results/estimatr_lm_robust.txt"), sep = "\t", skip = 4) |>
    setNames(c("term", "dydxstata", "std.errorstata")) |>
    head(3)
results[["estimatr_lm_robust"]] <- tmp

#  estimatr::iv_robust
tmp <- read.table(test_path("stata/results/estimatr_iv_robust.txt"), sep = "\t", skip = 4) |>
    setNames(c("term", "dydxstata", "std.errorstata")) |>
    head(2)
tmp$std.errorstata <- as.numeric(gsub("\\(|\\)", "", tmp$std.errorstata))
results[["estimatr_iv_robust"]] <- tmp

# save
saveRDS(results, file = test_path("stata/stata.rds"))
