mod <- lm(mpg ~ wt + factor(am), data = mtcars)

# expect_slopes
slo <- slopes(mod)
expect_s3_class(slo, "slopes")
expect_s3_class(slo, "marginaleffects")
expect_true("std.error" %in% colnames(slo))

# expect_predictions
pre <- predictions(mod)
expect_s3_class(pre, "predictions")
expect_true("std.error" %in% colnames(pre))

# expect_hypotheses
hyp <- hypotheses(mod)
expect_s3_class(hyp, "hypotheses")
expect_true("std.error" %in% colnames(hyp))

# expect_comparisons
cmp <- comparisons(mod)
expect_s3_class(cmp, "comparisons")
expect_true("std.error" %in% colnames(cmp))
