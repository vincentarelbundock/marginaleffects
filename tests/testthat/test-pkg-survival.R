library("survival")

test_that("coxph: no validity", {
    mod <- list()
    test1 <- list(time=c(4,3,1,1,2,2,3), 
                  status=c(1,1,1,0,1,1,0), 
                  x=c(0,2,1,1,1,0,0), 
                  sex=c(0,0,0,0,1,1,1)) 
    mod[[1]] <- coxph(Surv(time, status) ~ x + strata(sex), test1) 
    test2 <- list(start=c(1,2,5,2,1,7,3,4,8,8), 
                  stop=c(2,3,6,7,8,9,9,9,14,17), 
                  event=c(1,1,1,1,1,1,1,0,0,0), 
                  x=c(1,0,0,1,0,1,1,1,0,0)) 
    mod[[2]] <- coxph(Surv(start, stop, event) ~ x, test2)
    mod[[3]] <- coxph(Surv(time, status) ~ ph.ecog + tt(age), data=lung,
                      tt=function(x,t,...) pspline(x + t/365.25))
    for (i in seq_along(mod)) {
         mfx <- marginaleffects(mod[[i]], prediction_type = "risk")
         tid <- tidy(mfx)
         expect_s3_class(tid, "data.frame")
         expect_true(nrow(tid) > 0)
    }
})
