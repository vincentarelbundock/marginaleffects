library(data.table)
setwd(here::here())
dat = fread("chapters/data/thornton_hiv.csv")
setnames(dat, 
    old = c("villnum", "got", "distvct", "any", "tinc", "age"),
    new = c("village", "outcome", "distance", "incentive", "amount", "age"))
dat = dat[!is.na(outcome) & !is.na(village) & !is.na(age)]
dat[, rownames := NULL]

dat[, agecat := fcase(
    age < 18, "<18",
    age >= 18 & age <= 35, "18-35",
    age > 35, ">35"
)]

dat = dat[order(incentive, age),]
dat[, agecat := factor(agecat, unique(agecat))]

fwrite(dat, "chapters/data/hiv.csv", na = NA)
saveRDS(dat, "chapters/data/hiv.rds")
