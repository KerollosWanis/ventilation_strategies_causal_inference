source("./src/dependencies.R")
source("./src/functions.R")

#load data

load("./data/cohort.Rdata")

#change hour indexing

cohort$hr <- cohort$hr + 1

#choose first hour of follow-up and max follow-up and optionally indicate filtering 
#to patients who were treated with high flow or noninvasive prior to baseline

appropriate_baseline <- F

first_hr <- 4
max_fu <- 720

#restrict to MICU

cohort <- 
  cohort %>%
  filter(first_careunit == 'Medical Intensive Care Unit (MICU)')

#rename vars

cohort <- 
  cohort %>%
  mutate(female = case_when(gender == 'M' ~ 0,
                            gender == 'F' ~ 1),
         medicare_medicaid = case_when(insurance == 'Medicaid' | insurance == 'Medicare' ~ 1,
                                       insurance == 'Other' ~ 0)) %>% 
  rename(id = subject_id,
         hour = hr,
         age = anchor_age,
         HR = heart_rate,
         SBP = sbp,
         DBP = dbp,
         MBP = mbp,
         RR = resp_rate,
         temp = temperature,
         spO2 = spo2,
         fiO2 = fio2,
         glucose = glucose,
         vasopressor = vasopressor,
         sofa = sofa_24hours,
         elixhauser = elixhauser_vanwalraven) 

#fix some missing glucose values

cohort <- cohort %>%
  mutate(glucose = case_when(glucose == 999999.0 ~ NA_real_,
         TRUE ~ glucose)) %>% 
  group_by(id) %>%
  fill(glucose) %>%
  ungroup()

#indicate discharge from icu

cohort <- 
  cohort %>% 
  group_by(id) %>% 
  mutate(left_icu = case_when(hour == ceiling(los*24) ~ 1, TRUE ~ 0)) %>% 
  mutate(left_icu = cumsum(left_icu)) %>%
  ungroup()

#define outcome variable

cohort <- 
  cohort %>% mutate(outcome = as.numeric(death_outcome)) 

#remove rows after outcome occurs

cohort <- 
  cohort %>% group_by(id) %>%
  mutate(post_outcome = cumsum(outcome)) %>%
  filter(post_outcome == 0 | lag(post_outcome) == 0) %>%
  dplyr::select(-post_outcome) %>%
  ungroup()

#remove unnecessary variables

cohort <- 
  cohort %>% 
  dplyr::select(c(id, 
                  hour,
                  female,
                  race,
                  medicare_medicaid,
                  age,
                  HR,
                  SBP,
                  DBP,
                  MBP,
                  RR,
                  temp,
                  spO2,
                  fiO2,
                  glucose,
                  sofa,
                  vasopressor,
                  elixhauser,
                  invasive,
                  noninvasive,
                  highflow,
                  left_icu,
                  outcome))

#lag all time-varying covariates since modelling will depend on past covariate values

cohort <- 
  cohort %>% 
  group_by(id) %>%
  mutate(HR = lag(HR),
         SBP = lag(SBP),
         MBP = lag(MBP),
         RR = lag(RR),
         temp = lag(temp),
         spO2 = lag(spO2),
         fiO2 = lag(fiO2),
         glucose = lag(glucose),
         sofa = lag(sofa),
         vasopressor = lag(vasopressor)) %>%
  ungroup()

#optionally filter to those who have received high flow or noninvasive prior to the first hr

if (first_hr > 1 & appropriate_baseline == T) {
  
  cohort <- 
    cohort %>%
    filter(id %in% (cohort %>% group_by(id) %>%
                      mutate(meets_criteria = case_when(spO2 <= 90 | 
                                                          fiO2 >= 40 | 
                                                          RR >= 30 |
                                                          highflow == 1 |
                                                          noninvasive == 1 ~ 1, 
                                                        TRUE ~ 0)) %>%
                      filter(hour < first_hr & 
                               (cumsum(meets_criteria) >= 1)) %>% 
                      ungroup() %>% 
                      {.$id}))
  
}

#filter data.frame to those who have spent at least first_hr hours in ICU 
#and have not initiated invasive ventilation prior to first_hr or left ICU by first hr

if (first_hr > 1) {
  
  cohort <- 
    cohort %>%
    filter(! id %in% (cohort %>% group_by(id) %>%
                        filter(hour < first_hr & 
                                 cumsum(invasive) >= 1) %>% ungroup() %>% {.$id}))
  
}

cohort <- 
  cohort %>%
  filter(hour >= first_hr)

cohort <-
  cohort %>% 
  filter(! id %in% (cohort %>% 
                      group_by(id) %>% 
                      filter(hour == first_hr & left_icu == 1) %>% {.$id}))

#end follow-up after max_fu in ICU

cohort <- 
  cohort %>%
  filter(hour <= max_fu)

#filter to patients with complete baseline information

cohort <- 
  cohort %>% 
  filter(!race %in% c('UNKNOWN', 'UNABLE TO OBTAIN')) %>%
  mutate(race = as.factor(race))

cohort <- 
  cohort %>%
  filter(! id %in% (cohort %>% 
                      filter(hour == first_hr & 
                               (is.na(HR) | 
                                  is.na(SBP) |
                                  is.na(DBP) |
                                  is.na(MBP) |
                                  is.na(RR) |
                                  is.na(spO2) |
                                  is.na(temp) |
                                  is.na(glucose) |
                                  is.na(sofa) |
                                  is.na(elixhauser))) %>% {.$id}))

cohort <- cohort %>% 
  group_by(id)  %>% 
  mutate(started_invasive = case_when(cumsum(invasive) >= 1 ~ 1, TRUE ~ 0),
         started_noninvasive = case_when(cumsum(noninvasive) >= 1 ~ 1, TRUE ~ 0),
         started_highflow = case_when(cumsum(highflow) >= 1 ~ 1, TRUE ~ 0)) %>%
  ungroup()

if (appropriate_baseline) {
  
  save(cohort, file='cohort_analysis_appropriate_baseline.Rdata')
  
} else {
  
  save(cohort, file='cohort_analysis.Rdata')
  
}
