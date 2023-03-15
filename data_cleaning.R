source("./src/dependencies.R")
source("./src/functions.R")

#load data

cohort <- readRDS("./data/cohort_MICU.rds") 

#change hour indexing

cohort$hr <- cohort$hr + 1

#choose first hour of follow-up and max follow-up and optionally indicate filtering 
#to patients who were treated with high flow or noninvasive prior to baseline

appropriate_baseline <- F

first_hr <- 4
max_fu <- 720

#rename vars

cohort <- 
  cohort %>%
  mutate(female = case_when(gender == 'M' ~ 0,
                            gender == 'F' ~ 1),
         medicare_medicaid = case_when(insurance == 'Medicaid' | insurance == 'Medicare' ~ 1,
                                       insurance == 'Other' ~ 0)) %>% 
  rename(id = stay_id,
         hour = hr,
         age = anchor_age,
         HR = heart_rate,
         SBP = sbp,
         DBP = dbp,
         RR = resp_rate,
         temp = temperature,
         spO2 = spo2,
         fiO2 = fio2,
         glucose = glucose,
         gcs = gcs_min,
         elixhauser = elixhauser_vanwalraven) %>%
  arrange(id, hour)

#fix some missing values

cohort <- cohort %>%
  mutate(glucose = case_when(glucose == 999999.0 ~ NA_real_,
         TRUE ~ glucose),
         fiO2 = case_when(hour == 1 & is.na(fiO2) ~ 0,
                          TRUE ~ fiO2),
         gcs = case_when(hour == 1 & is.na(gcs) ~ 15,
                          TRUE ~ gcs),
         vasopressor = case_when(is.na(vasopressor) ~ 0,
                                 TRUE ~ as.numeric(vasopressor)))

#carry forward variable values

cohort <- cohort %>%
  group_by(id) %>%
  fill(HR, SBP, DBP, RR, temp, spO2, fiO2, glucose, gcs) %>%
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
                  RR,
                  temp,
                  spO2,
                  fiO2,
                  glucose,
                  gcs,
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
         RR = lag(RR),
         temp = lag(temp),
         spO2 = lag(spO2),
         fiO2 = lag(fiO2),
         glucose = lag(glucose),
         gcs = lag(gcs),
         vasopressor = lag(vasopressor)) %>%
  ungroup()

#optionally filter to those meeting respiratory criteria

if (first_hr > 1 & appropriate_baseline == T) {
  
  cohort <- 
    cohort %>%
    filter(id %in% (cohort %>% group_by(id) %>%
                      mutate(meets_criteria = case_when(spO2 <= 90 | 
                                                          fiO2 >= 30 | 
                                                          RR > 25 |
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

# create categories for some variables

cohort <- cohort %>%
  mutate(race = case_when(
    race %in% c('AMERICAN INDIAN/ALASKA NATIVE','NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER') ~ 'AMERICAN INDIAN/ALASKA NATIVE',
    race %in% c('ASIAN', 'ASIAN - ASIAN INDIAN', 'ASIAN - CHINESE',
                'ASIAN - KOREAN', 'ASIAN - SOUTH EAST ASIAN') ~ 'ASIAN',
    race %in% c('BLACK/AFRICAN', 'BLACK/AFRICAN AMERICAN', 'BLACK/CAPE VERDEAN',
                'BLACK/CARIBBEAN ISLAND') ~ 'BLACK/AFRICAN AMERICAN',
    race %in% c('HISPANIC OR LATINO', 'HISPANIC/LATINO - CENTRAL AMERICAN', 'HISPANIC/LATINO - DOMINICAN', 'HISPANIC/LATINO - COLUMBIAN',
                'HISPANIC/LATINO - GUATEMALAN', 'HISPANIC/LATINO - HONDURAN', 'HISPANIC/LATINO - MEXICAN', 'HISPANIC/LATINO - CUBAN',
                'HISPANIC/LATINO - PUERTO RICAN', 'HISPANIC/LATINO - SALVADORAN', 'PORTUGUESE', 'SOUTH AMERICAN') ~ 'HISPANIC/LATINO',
    race %in% c('OTHER', 'MULTIPLE RACE/ETHNICITY') ~ 'OTHER',
    race %in% c('WHITE', 'WHITE - BRAZILIAN', 'WHITE - EASTERN EUROPEAN', 
                'WHITE - OTHER EUROPEAN', 'WHITE - RUSSIAN') ~ 'WHITE',
    race %in% c('PATIENT DECLINED TO ANSWER', 'UNKNOWN', 'UNABLE TO OBTAIN') ~ 'UNKNOWN'
  ),
  gcs = as_factor(
    case_when(
      gcs >= 13 ~ 'mild',
      gcs >= 9 & gcs < 13 ~ 'moderate',
      gcs < 9 ~ 'severe'
    )))

#filter to patients with complete baseline information

cohort <- 
  cohort %>% 
  filter(!race == 'UNKNOWN') %>%
  mutate(race = as.factor(race))

cohort <- 
  cohort %>%
  filter(! id %in% (cohort %>% 
                      filter(hour == first_hr & 
                               (is.na(HR) | 
                                  is.na(SBP) |
                                  is.na(DBP) |
                                  is.na(RR) |
                                  is.na(spO2) |
                                  is.na(temp) |
                                  is.na(glucose) |
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