
library(emmeans)

pkgload::load_all()

mod <- lm(log(conc) ~ source + factor(percent), data = pigs)
rg <- ref_grid(mod)
em <- emmeans(rg, "source")
pa <- pairs(em, df = Inf)

# left
marginalmeans(
    mod,
    variables = "source",
    hypothesis = "pairwise") |>
    transform(equivalence = (marginalmean - 0 - delta) / std.error)
test(pa, delta = delta, adjust = "none", side = -1)

# right
marginalmeans(
    mod,
    variables = "source",
    hypothesis = "pairwise") |>
    transform(equivalence = (marginalmean - 0 + delta) / std.error)
test(pa, delta = delta, adjust = "none", side = 1)

# equivalence
marginalmeans(
    mod,
    variables = "source",
    hypothesis = "pairwise") |>
    transform(equivalence = (abs(marginalmean - 0) - delta) / std.error)
test(pa, delta = delta, adjust = "none")


test(pigs.prs.s, delta = log(1.25), adjust = "none")
