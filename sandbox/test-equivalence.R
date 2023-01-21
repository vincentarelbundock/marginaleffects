# TODO: test negative estimates (reverse = TRUE) and mix of +/- estimates

library(emmeans)

pkgload::load_all()

mod <- lm(log(conc) ~ source + factor(percent), data = pigs)
rg <- ref_grid(mod)
em <- emmeans(rg, "source")
pa <- pairs(em, df = Inf, reverse = TRUE)

delta <- .1


hypotheses(mod, side = ">", null = 0, delta = .1)


mm <- marginalmeans(
    mod,
    variables = "source",
    hypothesis = "revpairwise") 

delta <- log(1.25)

# left
test(pa, delta = delta, adjust = "none", side = -1, df = Inf) |> data.frame()
hypotheses(mm, delta = delta, side = "nonsuperiority")

# right
test(pa, delta = delta, adjust = "none", side = 1) |> data.frame()
hypotheses(mm, delta = delta, side = "noninferiority")

# equivalence
test(pa, delta = delta, adjust = "none") |> data.frame()
hypotheses(mm, delta = delta, side = "equivalence")




delta = 1
pkgload::load_all()
hypotheses(mod, side = "noninferiority", null = .25, delta = .1)
