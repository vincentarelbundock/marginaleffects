source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("dplyr")

mod <- glm(am ~ hp, data = mtcars, family = binomial)
cmp <- comparisons(mod, transform_pre = "lnor")
expect_error(summary(cmp), pattern = "collapsible")
expect_error(tidy(cmp), pattern = "collapsible")
cmp <- comparisons(mod, transform_pre = "lnoravg")
expect_equivalent(nrow(cmp), 1)


exit_file("works interactively")

# simple summary output
mod <- lm(mpg ~ hp + factor(cyl), mtcars)
mfx <- marginaleffects(mod)
known <- "Average marginal effects
  Term Contrast   Effect Std. Error z value  Pr(>|z|)     2.5 %   97.5 %
1   hp    dY/dX -0.02404    0.01541  -1.560 0.1187214  -0.05424  0.00616
2  cyl    6 - 4 -5.96766    1.63928  -3.640 0.0002722  -9.18058 -2.75473
3  cyl    8 - 4 -8.52085    2.32607  -3.663 0.0002491 -13.07987 -3.96183

Model type:  lm
Prediction type:  response"
expect_print(summary(mfx), known)


# summary conf.level
mod <- lm(mpg ~ hp + factor(cyl), mtcars)
mfx <- marginaleffects(mod)

known <- "Average marginal effects
  Term Contrast   Effect Std. Error z value  Pr(>|z|)     5.0 %    95.0 %
1   hp    dY/dX -0.02404    0.01541  -1.560 0.1187214  -0.04938  0.001305
2  cyl    6 - 4 -5.96766    1.63928  -3.640 0.0002722  -8.66403 -3.271283
3  cyl    8 - 4 -8.52085    2.32607  -3.663 0.0002491 -12.34690 -4.694798

Model type:  lm
Prediction type:  response"
expect_print(summary(mfx, conf_level = .9), known)

known <- "Average marginal effects
  Term Contrast   Effect Std. Error z value  Pr(>|z|)   40.0 %   60.0 %
1   hp    dY/dX -0.02404    0.01541  -1.560 0.1187214 -0.02794 -0.02014
2  cyl    6 - 4 -5.96766    1.63928  -3.640 0.0002722 -6.38296 -5.55235
3  cyl    8 - 4 -8.52085    2.32607  -3.663 0.0002491 -9.11016 -7.93155

Model type:  lm
Prediction type:  response"
expect_print(summary(mfx, conf_level = .2), known)


# summary: marginal means
dat <- mtcars
dat$am <- as.logical(dat$am)
dat$vs <- as.logical(dat$vs)
dat$gear <- as.factor(dat$gear)
mod <- lm(mpg ~ gear + am + vs, dat)
mm <- marginalmeans(mod)
known <- "Estimated marginal means
  Term Value  Mean Std. Error z value   Pr(>|z|) 2.5 % 97.5 %
1   am FALSE 17.43      1.416   12.31 < 2.22e-16 14.65  20.20
2   am  TRUE 24.37      1.253   19.45 < 2.22e-16 21.91  26.82
3 gear     3 21.64      1.603   13.50 < 2.22e-16 18.49  24.78
4 gear     4 21.09      1.264   16.69 < 2.22e-16 18.61  23.57
5 gear     5 19.97      1.969   10.14 < 2.22e-16 16.11  23.83
6   vs FALSE 17.47      1.007   17.35 < 2.22e-16 15.49  19.44
7   vs  TRUE 24.33      1.194   20.37 < 2.22e-16 21.99  26.67

Model type:  lm
Prediction type:  response
Results averaged over levels of: gear, am, vs"
expect_print(summary(mm), known)

mod <- glm(am ~ hp, data = mtcars, family = binomial)
cmp <- comparisons(mod, transform_pre = function(hi, lo) hi / lo, transform_post = exp)
known <- "Average contrasts
  Term   Contrast Effect Std. Error z value   Pr(>|z|) 2.5 % 97.5 %
1   hp (x + 1), x  2.705   0.003638   743.7 < 2.22e-16 2.698  2.712

Model type:  glm
Prediction type:  response
Pre-transformation:  function(hi, lo) hi/lo
Post-transformation:  exp"
expect_print(summary(cmp), known)

# bugs stay dead: summary manipulation (destroys attributes, unfortunately)
mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
mfx <- marginaleffects(mod)
known <- "Average marginal effects
  Term    Effect   CI low   CI high
1   hp  0.002654 -0.00115  0.006457
2   wt -0.435727 -0.63569 -0.235764

Model type:
Prediction type:"
expect_print(
    summary(mfx) %>% dplyr::select(term, estimate, conf.low, conf.high),
    known)

