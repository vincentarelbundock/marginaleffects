dat <- read.csv("military.csv")
dat <- transform(dat, officer = as.numeric(grepl("officer", grade)))
saveRDS(dat, "military.RDS")
