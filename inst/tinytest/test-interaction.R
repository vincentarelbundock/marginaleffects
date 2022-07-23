source("helpers.R")
requiet("ggplot2")

data("diamonds", package = "ggplot2")
dat <- diamonds[1:1000, ]
dat$cut <- factor(as.character(dat$cut), levels = levels(dat$cut))
dat$color <- factor(as.character(dat$color), levels = levels(dat$color))
dat$clarity <- factor(as.character(dat$clarity), levels = levels(dat$clarity))
mod <- lm(price ~ cut * color + clarity  + carat, data = dat)
cmp1 <- tidy(comparisons(mod, variables = c("cut", "color"))) 
cmp2 <- tidy(comparisons(mod, variables = "cut"))

expect_equivalent(nrow(cmp1), 34)
expect_equivalent(nrow(cmp2), 4)
expect_equivalent(anyDuplicated(cmp1$estimate), 0)
expect_equivalent(anyDuplicated(cmp2$estimate), 0)
expect_false(anyNA(cmp1$estimate))
expect_false(anyNA(cmp1$std.error))
expect_false(anyNA(cmp2$estimate))
expect_false(anyNA(cmp2$std.error))

