requiet("survival")

test_that("coxph vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$survival_coxph_01
    test1 <- list(time=c(4,3,1,1,2,2,3), 
                  status=c(1,1,1,0,1,1,0), 
                  x=c(0,2,1,1,1,0,0), 
                  sex=c(0,0,0,0,1,1,1)) 
    mod <- coxph(Surv(time, status) ~ x + strata(sex), 
                 data = test1,
                 ties = "breslow")
    mfx <- merge(tidy(marginaleffects(mod, type = "lp")), stata)
    expect_mfx(mod, type = "risk", n_unique = 4)
    expect_equal(mfx$estimate, mfx$dydxstata)
    expect_equal(mfx$std.error, mfx$std.errorstata)
})


test_that("coxph: no validity", {
    test2 <- list(start=c(1,2,5,2,1,7,3,4,8,8), 
                  stop=c(2,3,6,7,8,9,9,9,14,17), 
                  event=c(1,1,1,1,1,1,1,0,0,0), 
                  x=c(1,0,0,1,0,1,1,1,0,0)) 
    mod <- coxph(Surv(start, stop, event) ~ x, test2)
    expect_mfx(mod, type = "risk", n_unique = 2)

    mod <- coxph(Surv(time, status) ~ ph.ecog + tt(age), data=lung,
                 tt=function(x,t,...) pspline(x + t/365.25))
    expect_mfx(mod, type = "risk")
})


