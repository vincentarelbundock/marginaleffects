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


# Issue #1575: changes in numeric results?
if (requireNamespace("modelbased", quietly = TRUE) && requireNamespace("datawizard", quietly = TRUE)) {
    data(efc, package = "modelbased")
    efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
    levels(efc$c172code) <- c("low", "mid", "high")
    m <- lm(neg_c_7 ~ c12hour + barthtot + e42dep + c161sex * c172code, data = efc)
    nd <- datagrid(model = m, grid_type = "balanced")
    nd$p <- predict(m, newdata = nd)
    manu <- aggregate(p ~ c161sex + c172code, data = nd, mean) |>
        sort_by(~ c161sex + c172code)
    auto <- avg_predictions(
        m,
        newdata = "balanced",
        by = c("c161sex", "c172code")) |>
        sort_by(~ c161sex + c172code)
    expect_equal(auto$estimate, manu$p, tolerance = 1e-6)
}
