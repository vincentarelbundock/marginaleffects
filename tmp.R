# install.packages("devtools")

pkgload::load_all()
penguins <- get_dataset("penguins", "palmerpenguins")
m <- lm(bill_length_mm ~ island * sex + bill_depth_mm + species, data = penguins)
predictions(m)
nd <- insight::get_datagrid(m, by = c("island", "sex"), factors = "all")
slopes(m)
# avg_predictions(
#   m
#   # by = c("island", "sex"),
#   # hypothesis = ratio ~ pairwise | sex
# )
#
