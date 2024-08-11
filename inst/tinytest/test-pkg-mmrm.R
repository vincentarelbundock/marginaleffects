source("helpers.R")
using("marginaleffects")

requiet("mmrm")

fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)

pre <- avg_predictions(fit, newdata = fev_data)
expect_inherits(pre, "predictions")

cmp <- avg_comparisons(fit, 
    variables = "SEX",
    newdata = fev_data)
expect_inherits(cmp, "comparisons")
