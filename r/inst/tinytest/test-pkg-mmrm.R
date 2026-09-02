source("helpers.R")
using("marginaleffects")

requiet("mmrm")
requiet("emmeans")

data("fev_data", package = "mmrm")

fit <- mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
)


# get_coef() / set_coef() round-trip
b <- get_coef(fit)
expect_equivalent(get_coef(set_coef(fit, b)), b)

# set_coef() actually changes predictions: guards against cached fitted values,
# which would silently zero out the delta method Jacobian
b2 <- b
b2["SEXFemale"] <- b2["SEXFemale"] + 10
p1 <- get_predict(fit, newdata = fev_data)$estimate
p2 <- get_predict(set_coef(fit, b2), newdata = fev_data)$estimate
expect_false(isTRUE(all.equal(p1, p2)))


# predictions are linear in beta: estimate = X %*% b, se = sqrt(diag(X V X'))
nd <- datagrid(ARMCD = unique, AVISIT = unique, model = fit)
p <- predictions(fit, newdata = nd)
X <- model.matrix(
    delete.response(terms(fit)),
    data = nd,
    contrasts.arg = component(fit, "contrasts"),
    xlev = component(fit, "xlev")
)
expect_equivalent(p$estimate, as.vector(X %*% b))
# tolerance reflects the numeric differentiation Jacobian
expect_equivalent(p$std.error, sqrt(diag(X %*% get_vcov(fit) %*% t(X))), tolerance = 1e-5)


# marginal means match emmeans. `emmeans` marginalizes with equal weights over
# every factor level, so the grid must be balanced over RACE and SEX as well.
em <- data.frame(emmeans(fit, ~ ARMCD | AVISIT))
grid <- datagrid(ARMCD = unique, AVISIT = unique, RACE = unique, SEX = unique, model = fit)
mfx <- predictions(fit, by = c("ARMCD", "AVISIT"), newdata = grid)
em <- em[order(em$AVISIT, em$ARMCD), ]
mfx <- mfx[order(mfx$AVISIT, mfx$ARMCD), ]
expect_equivalent(mfx$estimate, em$emmean, tolerance = 1e-6)
expect_equivalent(mfx$std.error, em$SE, tolerance = 1e-5)

# contrasts match emmeans::pairs()
pw <- data.frame(pairs(emmeans(fit, ~ ARMCD | AVISIT)))
cmp <- comparisons(fit, variables = "ARMCD", by = "AVISIT", newdata = grid)
pw <- pw[order(pw$AVISIT), ]
cmp <- cmp[order(cmp$AVISIT), ]
# emmeans contrasts PBO - TRT; marginaleffects contrasts TRT - PBO
expect_equivalent(cmp$estimate, -pw$estimate, tolerance = 1e-6)
expect_equivalent(cmp$std.error, pw$SE, tolerance = 1e-5)


# basic expectations
pre <- avg_predictions(fit, newdata = fev_data)
expect_inherits(pre, "predictions")

cmp <- avg_comparisons(fit, variables = "SEX", newdata = fev_data)
expect_inherits(cmp, "comparisons")

s <- avg_slopes(fit)
expect_inherits(s, "slopes")
