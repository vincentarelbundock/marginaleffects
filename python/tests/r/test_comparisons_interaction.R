source(here::here("tests/r/load.R"))
library(emmeans)

# Read the same vendored CSVs as tests/test_comparisons_interaction.py.
dat <- fread(here("tests/data/interaction_01.csv"), na.strings = c("NA", ""))
mod <- glm(Y ~ X * M, data = dat, family = binomial)
avg_comparisons(mod, by = c("X", "M")) |>
    fwrite(here("tests/r/test_comparisons_interaction_01.csv"))

mtcars_dat <- fread(here("tests/data/mtcars.csv"), na.strings = c("NA", ""))
mod_em <- lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars_dat)
em <- emmeans::emmeans(mod_em, c("cyl", "am"))
em <- emmeans::contrast(em, method = "revpairwise")
em <- data.frame(em)
fwrite(em, here("tests/r/test_comparisons_interaction_emmeans.csv"))
