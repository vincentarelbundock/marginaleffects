# simple summary output

    Code
      print(summary(mfx), digits = 3)
    Output
      Average marginal effects 
            type         Term Effect Std. Error z value  Pr(>|z|)    2.5 %   97.5 %
      1 response factor(cyl)6 -5.968     1.6393   -3.64 0.0002722  -9.1806 -2.75473
      2 response factor(cyl)8 -8.521     2.3261   -3.66 0.0002491 -13.0799 -3.96183
      3 response           hp -0.024     0.0154   -1.56 0.1187213  -0.0542  0.00616
      
      Model type:  lm 
      Prediction type:  response 

# summary conf.level

    Code
      print(summary(mfx, conf.level = 0.9))
    Output
      Average marginal effects 
            type         Term   Effect Std. Error z value  Pr(>|z|)     5.0 %
      1 response factor(cyl)6 -5.96766    1.63928  -3.640 0.0002722  -8.66403
      2 response factor(cyl)8 -8.52085    2.32607  -3.663 0.0002491 -12.34690
      3 response           hp -0.02404    0.01541  -1.560 0.1187213  -0.04938
           95.0 %
      1 -3.271283
      2 -4.694798
      3  0.001305
      
      Model type:  lm 
      Prediction type:  response 

---

    Code
      print(summary(mfx, conf.level = 0.2))
    Output
      Average marginal effects 
            type         Term   Effect Std. Error z value  Pr(>|z|)   40.0 %   60.0 %
      1 response factor(cyl)6 -5.96766    1.63928  -3.640 0.0002722 -6.38296 -5.55235
      2 response factor(cyl)8 -8.52085    2.32607  -3.663 0.0002491 -9.11016 -7.93155
      3 response           hp -0.02404    0.01541  -1.560 0.1187213 -0.02794 -0.02014
      
      Model type:  lm 
      Prediction type:  response 

