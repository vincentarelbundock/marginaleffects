source("helpers.R")
using("marginaleffects")

requiet("nestedLogit")
requiet("carData")

data(Womenlf, package = "carData")

dichotomies <- nestedLogit::logits(
    work = nestedLogit::dichotomy("not.work", c("parttime", "fulltime")),
    full = nestedLogit::dichotomy("parttime", "fulltime")
)

mod <- nestedLogit::nestedLogit(
    partic ~ hincome + children,
    dichotomies = dichotomies,
    data = Womenlf
)

# predictions match predict()
nd <- head(Womenlf, 3)
pred_me <- predictions(mod, newdata = nd, type = "response")
pred_nl <- as.data.frame(predict(mod, newdata = nd))
pred_nl <- pred_nl[order(pred_nl$response), ]
pred_me <- as.data.frame(pred_me)
pred_me <- pred_me[order(pred_me$group), ]
expect_equivalent(pred_me$estimate, pred_nl$p)
expect_equivalent(pred_me$std.error, pred_nl$se.p)

# basic expectation tests
expect_predictions(mod, type = "response")
expect_slopes(mod, type = "response")
expect_comparisons(mod, type = "response")

# avg_predictions: no NA standard errors
ap <- avg_predictions(mod, type = "response")
expect_inherits(ap, "predictions")
expect_equivalent(nrow(ap), 3)
expect_false(any(is.na(ap$std.error)))

# avg_slopes: no NA standard errors
as <- avg_slopes(mod, type = "response")
expect_inherits(as, "slopes")
expect_false(any(is.na(as$std.error)))

# avg_comparisons: no NA standard errors
ac <- avg_comparisons(mod, type = "response")
expect_inherits(ac, "comparisons")
expect_false(any(is.na(ac$std.error)))
