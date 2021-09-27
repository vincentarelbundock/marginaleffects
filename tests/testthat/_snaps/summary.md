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

# summary: marginal means

    Code
      print(s)
    Output
      Estimated marginal means 
        Term Group  Mean Std. Error z value   Pr(>|z|) 2.5 % 97.5 %
      1 gear     3 21.64      1.603   13.50 < 2.22e-16 18.49  24.78
      2 gear     4 21.09      1.264   16.69 < 2.22e-16 18.61  23.57
      3 gear     5 19.97      1.969   10.14 < 2.22e-16 16.11  23.83
      4   am FALSE 17.43      1.416   12.31 < 2.22e-16 14.65  20.20
      5   am  TRUE 24.37      1.253   19.45 < 2.22e-16 21.91  26.82
      6   vs FALSE 17.47      1.007   17.35 < 2.22e-16 15.49  19.44
      7   vs  TRUE 24.33      1.194   20.37 < 2.22e-16 21.99  26.67
      
      Model type:  lm 
      Prediction type:  expectation 

