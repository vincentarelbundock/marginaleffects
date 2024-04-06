# No validity test whatsoever

source("helpers.R")
using("marginaleffects")
if (!requiet("DCchoice")) exit_file("DCchoice not available")

data(oohbsyn)
mod <- oohbchoice(R1 + R2 ~ age + gender | log(BL) + log(BH), data = oohbsyn)

# weird result seems to make sense manually
p1 <- transform(oohbsyn, BH = 5)
p2 <- transform(oohbsyn, BH = 7)
p1 <- predict(mod, newdata = p1)
p2 <- predict(mod, newdata = p2)
bh_comparison <- p2 - p1
expect_true(all(bh_comparison == 0))

slo <- avg_slopes(mod)
pre <- predictions(mod, by = "gender")
cmp <- comparisons(mod)

expect_inherits(cmp, "comparisons")
expect_inherits(pre, "predictions")
expect_inherits(slo, "slopes")
