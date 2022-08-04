#install.packages("bigrquery")
library(bigrquery)
library(tidyverse)

#Pull data from BigQuery
bigrquery::bq_auth()   ##0 to reset the token/email login (tick all boxes)
1

# Set your query
sql_1 <- paste0("
SELECT *
FROM `mechanicalventillation1.mv_1.mv_35235805`
WHERE subject_id IN (", paste0(10000000:10050000, collapse=","), ")")

sql_2 <- "
SELECT *
FROM `mechanicalventillation1.mv_1.mv_35235805`"


sql_3 <- paste0("SELECT * FROM `mechanicalventillation1.mv2.5_v2_finaltab_retain_ventsubject_stays` 
ORDER BY subject_id, stay_id, hr_only
LIMIT ", LIMIT)

sql_4 <- "SELECT * FROM `mechanicalventillation1.mv2.5_v2_finaltab_retain_ventsubject_stays` 
ORDER BY subject_id, stay_id, hr_only"

sql_5 <- "SELECT * FROM `mech-vent-data.mv.mv_trial_first_ventilated_stay_per_subject`
ORDER BY subject_id, stay_id, hr"

sql_6 <- "SELECT * FROM `mech-vent-data.mv.mv_trial`
ORDER BY subject_id, stay_id, hr"

sql_test <- "SELECT * FROM `mech-vent-data.mv.mv_trial_first_ventilated_stay_per_subject`
ORDER BY subject_id, stay_id, hr"

sql_7 <- "SELECT subject_id, hr, death_outcome, discharge_outcome, discharge_location, invasive, noninvasive,
highflow, fio2, heart_rate, sbp, dbp, mbp, resp_rate, temperature, spo2, sepsis3, sofa_24hours,
gender, anchor_age, insurance, ethnicity, admission_type, los, elixhauser_SID30 FROM `mech-vent-data.mv.mv_trial_first_ventilated_stay_per_subject`
ORDER BY subject_id, stay_id, hr"


# Run the query and store the data in a tibble
query <- bq_project_query("mech-vent-data", sql_7)  ##yes login initiated here
cohort <- bq_table_download(query) %>%
  arrange(., subject_id, hr) 
save(cohort, file='cohort.Rdata')


# cohort_1 <- bq_table_download(query, start_index=0L, n_max=10000000L)  %>%
#   arrange(., subject_id, hr) 
# cohort_2 <- bq_table_download(query, start_index=10000000L, n_max=10000000L)  %>%
#   arrange(., subject_id, hr)
# cohort_3 <- bq_table_download(query, start_index=20000000L, n_max=10000000L)  %>%
#   arrange(., subject_id, hr)
# cohort_4 <- bq_table_download(query, start_index=30000000L, n_max=10000000L)  %>%
#   arrange(., subject_id, hr)
# 
# save(cohort_1, file='cohort_1.Rdata')
# save(cohort_2, file='cohort_2.Rdata')
# save(cohort_3, file='cohort_3.Rdata')
# save(cohort_4, file='cohort_4.Rdata')
# #save(cohort, file='cohort.Rdata')
# 
# 
# ################################
# ################################
# ################################
# 
# source("./src/dependencies.R")
# source("./src/functions.R")
# 
# 
# load("~/Projects/Thesis/Paper 2 analysis/cohort_1.Rdata")
# load("~/Projects/Thesis/Paper 2 analysis/cohort_2.Rdata")
# load("~/Projects/Thesis/Paper 2 analysis/cohort_3.Rdata")
# load("~/Projects/Thesis/Paper 2 analysis/cohort_4.Rdata")
# 
# #remove some columns
# 
# cohort_1 <- 
#   cohort_1 %>% dplyr::select(-c(hadm_id, stay_id, temperature_site, glucose, label, vaso_rate, vaso_amount,
#                                 anchor_year, anchor_year_group, language, marital_status, 
#                                 admittime, dischtime, deathtime, admission_location, first_careunit,
#                                 last_careunit, intime, outtime,
#                                 first_careunit, last_careunit, elixhauser_vanwalraven, elixhauser_SID29))
# cohort_2 <- 
#   cohort_2 %>% dplyr::select(-c(hadm_id, stay_id, temperature_site, glucose, label, vaso_rate, vaso_amount,
#                                 anchor_year, anchor_year_group, language, marital_status, 
#                                 admittime, dischtime, deathtime, admission_location, first_careunit,
#                                 last_careunit, intime, outtime,
#                                 first_careunit, last_careunit, elixhauser_vanwalraven, elixhauser_SID29))
# cohort_3 <- 
#   cohort_3 %>% dplyr::select(-c(hadm_id, stay_id, temperature_site, glucose, label, vaso_rate, vaso_amount,
#                                 anchor_year, anchor_year_group, language, marital_status, 
#                                 admittime, dischtime, deathtime, admission_location, first_careunit,
#                                 last_careunit, intime, outtime,
#                                 first_careunit, last_careunit, elixhauser_vanwalraven, elixhauser_SID29))
# cohort_4 <- 
#   cohort_4 %>% dplyr::select(-c(hadm_id, stay_id, temperature_site, glucose, label, vaso_rate, vaso_amount,
#                                 anchor_year, anchor_year_group, language, marital_status, 
#                                 admittime, dischtime, deathtime, admission_location, first_careunit,
#                                 last_careunit, intime, outtime,
#                                 first_careunit, last_careunit, elixhauser_vanwalraven, elixhauser_SID29))
# 
# cohort <- bind_rows(cohort_1, cohort_2, cohort_3, cohort_4)
# 
# cohort <- cohort %>% arrange(., subject_id, hr)
# 
# save(cohort, file='cohort.Rdata')
