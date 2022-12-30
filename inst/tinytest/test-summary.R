source("helpers.R", local = TRUE)
requiet("dplyr")

dat <<- mtcars
mod <- glm(am ~ hp, data = dat, family = binomial)
cmp <- comparisons(mod, transform_pre = "lnor")
expect_error(summary(cmp), pattern = "collapsible")
expect_error(tidy(cmp), pattern = "collapsible")
cmp <- comparisons(mod, transform_pre = "lnoravg")
expect_equivalent(nrow(cmp), 1)


# simple summary output
mod <- lm(mpg ~ hp + factor(cyl), dat)
mfx <- marginaleffects(mod)
known <- 
"  Term Contrast   Effect Std. Error z value  Pr(>|z|)     2.5 %   97.5 %
1   hp    dY/dX -0.02404    0.01541  -1.560 0.1187214  -0.05424  0.00616
2  cyl    6 - 4 -5.96766    1.63928  -3.640 0.0002722  -9.18058 -2.75473
3  cyl    8 - 4 -8.52085    2.32607  -3.663 0.0002491 -13.07987 -3.96183

Model type:  lm
Prediction type:  response"
expect_print(summary(mfx), known)


# summary conf.level
mod <- lm(mpg ~ hp + factor(cyl), dat)
mfx <- marginaleffects(mod)

known <- 
"  Term Contrast   Effect Std. Error z value  Pr(>|z|)     5.0 %    95.0 %
1   hp    dY/dX -0.02404    0.01541  -1.560 0.1187214  -0.04938  0.001305
2  cyl    6 - 4 -5.96766    1.63928  -3.640 0.0002722  -8.66403 -3.271283
3  cyl    8 - 4 -8.52085    2.32607  -3.663 0.0002491 -12.34690 -4.694798

Model type:  lm
Prediction type:  response"
expect_print(summary(mfx, conf_level = .9), known)

known <- 
"  Term Contrast   Effect Std. Error z value  Pr(>|z|)   40.0 %   60.0 %
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
dat <<- dat
mod <- lm(mpg ~ gear + am + vs, dat)
mm <- marginalmeans(mod)
known <- 
"      type Term Value  Mean Std. Error z value   Pr(>|z|) 2.5 % 97.5 %
1 response gear     3 21.64      1.603   13.50 < 2.22e-16 18.49  24.78
2 response gear     4 21.09      1.264   16.69 < 2.22e-16 18.61  23.57
3 response gear     5 19.97      1.969   10.14 < 2.22e-16 16.11  23.83
4 response   am FALSE 17.43      1.416   12.31 < 2.22e-16 14.65  20.20
5 response   vs FALSE 17.47      1.007   17.35 < 2.22e-16 15.49  19.44
6 response   am  TRUE 24.37      1.253   19.45 < 2.22e-16 21.91  26.82
7 response   vs  TRUE 24.33      1.194   20.37 < 2.22e-16 21.99  26.67

Model type:  lm 
Prediction type:  response 
Results averaged over levels of: gear, am, vs"
expect_print(summary(mm), known)

# bugs stay dead: summary manipulation (destroys attributes, unfortunately)
dat <<- mtcars
mod <- glm(am ~ hp * wt, data = dat, family = binomial)
mfx <- marginaleffects(mod)
known <- 
"  Term    Effect    CI low   CI high
1   hp  0.002653 -0.001147  0.006452
2   wt -0.435783 -0.635822 -0.235744

Model type:  
Prediction type:  "
expect_print(
    summary(mfx) %>% dplyr::select(term, estimate, conf.low, conf.high),
    known)


# Still not working
dat <<- mtcars
mod <- glm(am ~ hp, data = dat, family = binomial)
cmp <- comparisons(mod, transform_pre = function(hi, lo) hi / lo, transform_post = exp)
known <- 
"  Term Contrast Effect Std. Error z value   Pr(>|z|) 2.5 % 97.5 %
1   hp       +1  2.705   0.003638   743.7 < 2.22e-16 2.698  2.712

Model type:  glm 
Prediction type:  response 
Pre-transformation:  function(hi, lo) hi/lo 
Post-transformation:  exp"
expect_print(summary(cmp), known)

