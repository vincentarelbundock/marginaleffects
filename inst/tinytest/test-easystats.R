source("helpers.R")

# Issue #1573: duplicated column names
penguins <- get_dataset("penguins", "palmerpenguins")
m <- lm(bill_length_mm ~ island * sex + bill_depth_mm + species, data = penguins)
nd <- insight::get_datagrid(m, by = c("island", "sex"), factors = "all")
p <- avg_predictions(
  m,
  by = c("island", "sex"),
  hypothesis = ratio ~ pairwise | sex)
expect_true("sex" %in% colnames(p))
expect_false("sex.1" %in% colnames(p))
