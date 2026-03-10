source(here::here("tests/r/load.R"))

dat <- get_dataset("interaction_01")
mod <- glm(Y ~ X * M, data = dat, family = binomial)
avg_comparisons(mod, by = c("X", "M")) |> fwrite('test_comparisons_interaction_01.csv')


mod_em <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
em <- emmeans::emmeans(mod_em, c("cyl", "am"))
em <- emmeans::contrast(em, method = "revpairwise")
em <- data.frame(em)
fwrite(em,'test_comparisons_interaction_emmeans.csv')
