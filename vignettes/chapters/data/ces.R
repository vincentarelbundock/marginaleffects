set.seed(42)
source("code/load.R")
load('data/CES-2020-All.RData')
setDT(ces)
ces <- ces[, .(
    state = abb, defund = defund_police, gender, age = age_cat,
    education = educ, military = military_household)]
ces[, education := sub("_", " ", education)]
ces[, education := factor(education, c("No HS", "High school", "Some college", "2 year", "4 year", "Post grad"))]
ces[, gender := ifelse(gender == "Male", "Man", "Woman")]
ces[, age := factor(age, c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"))]
ces = ces[order(state, age, education, gender, military)]
survey <- ces[sample(seq_len(.N), 3000), ]
demographics <- ces[, .(percent = .N), by = .(state, age, education, gender, military)]
demographics[, percent := as.numeric(percent)]
demographics[, percent := percent / sum(percent) * 100, by = state]

saveRDS(survey, "data/ces_survey.rds")
saveRDS(demographics, "data/ces_demographics.rds")
