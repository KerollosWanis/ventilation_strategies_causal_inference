source("./src/dependencies.R")
source("./src/functions.R")

#################################
# Target Trial 1
#################################

load("./cohort_analysis_MICU_only.Rdata")

#############################
# intubate by k hours
#############################

tt1_save_results <- 
  sapply(seq(8,48,8), function(time) {
    resultsfnc(data = cohort,
               treatment = 'started_invasive',
               strategy_type = 'start_by_hour',
               strategy_time = time,
               max_hour = 720,
               truncate_time = T,
               pooled_time_treatment_model = T,
               vars_to_lag_trt = c('HR', 'SBP', 'DBP', 'RR', 
                                   'temp', 'glucose', 'spO2', 'sofa', 'vasopressor'),
               num_lags_trt = 2,
               model_formula_vars_trt = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                          'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser'),
               parametric_model_trt = F,
               nfold_trt = 5,
               tree_depth_trt = seq(1,3,2),
               shrinkage_factor_trt = seq(0.01,0.03,0.02),
               num_trees_trt = seq(100,300,100),
               num_cores_trt = 5,
               parametric_outcome_non_DR = NULL,
               vars_to_lag_outcome = c('HR', 'SBP', 'DBP', 'RR', 
                                       'temp', 'glucose', 'spO2', 'sofa', 'vasopressor'),
               num_lags_outcome = 2,
               model_formula_vars_outcome = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                              'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser'),
               parametric_model_outcome = F,
               nfold_outcome = 5,
               tree_depth_outcome = seq(1,3,2),
               shrinkage_factor_outcome = seq(0.01,0.03,0.02),
               num_trees_outcome = seq(100,300,100),
               num_cores_outcome = 5,
               DR = T,
               nsplits=2,
               dynamic_var = NULL,
               dynamic_val = NULL,
               dynamic_above = NULL) 
  })

tt1_save_results_light <- tt1_save_results[2:4,]

save(tt1_save_results_light, file='tt1_start_by_hour_results_MICU_only.Rdata')
save(tt1_save_results, file='tt1_start_by_hour_results_MICU_only_with_data.Rdata')

#############################
# dont intubate until k hours
#############################

tt1_save_results2 <- 
  sapply(seq(8,48,8), function(time) {
    resultsfnc(data = cohort,
               treatment = 'started_invasive',
               strategy_type = 'dont_start_until',
               strategy_time = time,
               max_hour = 720,
               truncate_time = T,
               pooled_time_treatment_model = T,
               vars_to_lag_trt = c('HR', 'SBP', 'DBP', 'RR', 
                                   'temp', 'glucose', 'spO2', 'sofa', 'vasopressor'),
               num_lags_trt = 2,
               model_formula_vars_trt = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                          'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser'),
               parametric_model_trt = F,
               nfold_trt = 5,
               tree_depth_trt = seq(1,3,2),
               shrinkage_factor_trt = seq(0.01,0.03,0.02),
               num_trees_trt = seq(100,300,100),
               num_cores_trt = 5,
               parametric_outcome_non_DR = NULL,
               vars_to_lag_outcome = c('HR', 'SBP', 'DBP', 'RR', 
                                       'temp', 'glucose', 'spO2', 'sofa', 'vasopressor'),
               num_lags_outcome = 2,
               model_formula_vars_outcome = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                              'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser'),
               parametric_model_outcome = F,
               nfold_outcome = 5,
               tree_depth_outcome = seq(1,3,2),
               shrinkage_factor_outcome = seq(0.01,0.03,0.02),
               num_trees_outcome = seq(100,300,100),
               num_cores_outcome = 5,
               DR = T,
               nsplits=2,
               dynamic_var = NULL,
               dynamic_val = NULL,
               dynamic_above = NULL) 
  })

tt1_save_results2_light <- tt1_save_results2[2:4,]

save(tt1_save_results2_light, file='tt1_dont_start_until_results_MICU_only.Rdata')
save(tt1_save_results2, file='tt1_dont_start_until_results_MICU_only_with_data.Rdata')


#################################
# Target Trial 2
#################################

load("./cohort_analysis_MICU_only.Rdata")

#############################
# intubate by k hours
#############################

tt2_save_results <- 
  sapply(seq(8,48,8), function(time) {
    resultsfnc(data = cohort,
               treatment = 'started_invasive',
               strategy_type = 'start_by_hour',
               strategy_time = time,
               max_hour = 720,
               truncate_time = T,
               pooled_time_treatment_model = T,
               vars_to_lag_trt = c('HR', 'SBP', 'DBP', 'RR', 
                                   'temp', 'glucose', 'spO2', 'sofa', 'vasopressor'),
               num_lags_trt = 2,
               model_formula_vars_trt = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                          'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser'),
               parametric_model_trt = F,
               nfold_trt = 5,
               tree_depth_trt = seq(1,3,2),
               shrinkage_factor_trt = seq(0.01,0.03,0.02),
               num_trees_trt = seq(100,300,100),
               num_cores_trt = 5,
               parametric_outcome_non_DR = NULL,
               vars_to_lag_outcome = c('HR', 'SBP', 'DBP', 'RR', 
                                       'temp', 'glucose', 'spO2', 'sofa', 'vasopressor'),
               num_lags_outcome = 2,
               model_formula_vars_outcome = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                              'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser'),
               parametric_model_outcome = F,
               nfold_outcome = 5,
               tree_depth_outcome = seq(1,3,2),
               shrinkage_factor_outcome = seq(0.01,0.03,0.02),
               num_trees_outcome = seq(100,300,100),
               num_cores_outcome = 5,
               DR = T,
               nsplits=2,
               dynamic_var = 'sofa',
               dynamic_val = 7,
               dynamic_above = FALSE) 
  })

tt2_save_results_light <- tt2_save_results[2:4,]

save(tt2_save_results_light, file='tt2_start_by_hour_results_MICU_only.Rdata')
save(tt2_save_results, file='tt2_start_by_hour_results_MICU_only_with_data.Rdata')

#############################
# dont intubate until k hours
#############################

tt2_save_results2 <- 
  sapply(seq(8,48,8), function(time) {
    resultsfnc(data = cohort,
               treatment = 'started_invasive',
               strategy_type = 'dont_start_until',
               strategy_time = time,
               max_hour = 720,
               truncate_time = T,
               pooled_time_treatment_model = T,
               vars_to_lag_trt = c('HR', 'SBP', 'DBP', 'RR', 
                                   'temp', 'glucose', 'spO2', 'sofa', 'vasopressor'),
               num_lags_trt = 2,
               model_formula_vars_trt = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                          'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser'),
               parametric_model_trt = F,
               nfold_trt = 5,
               tree_depth_trt = seq(1,3,2),
               shrinkage_factor_trt = seq(0.01,0.03,0.02),
               num_trees_trt = seq(100,300,100),
               num_cores_trt = 5,
               parametric_outcome_non_DR = NULL,
               vars_to_lag_outcome = c('HR', 'SBP', 'DBP', 'RR', 
                                       'temp', 'glucose', 'spO2', 'sofa', 'vasopressor'),
               num_lags_outcome = 2,
               model_formula_vars_outcome = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                              'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser'),
               parametric_model_outcome = F,
               nfold_outcome = 5,
               tree_depth_outcome = seq(1,3,2),
               shrinkage_factor_outcome = seq(0.01,0.03,0.02),
               num_trees_outcome = seq(100,300,100),
               num_cores_outcome = 5,
               DR = T,
               nsplits=2,
               dynamic_var = 'sofa',
               dynamic_val = 6,
               dynamic_above = TRUE) 
  })

tt2_save_results2_light <- tt2_save_results2[2:4,]

save(tt2_save_results2_light, file='tt2_dont_start_until_results_MICU_only.Rdata')
save(tt2_save_results2, file='tt2_dont_start_until_results_MICU_only_with_data.Rdata')


#################################
# Target Trial 3
#################################

load("./cohort_analysis_appropriate_baseline_MICU_only.Rdata")

#############################
# intubate by k hours
#############################

tt3_save_results <- 
  sapply(seq(8,48,8), function(time) {
    resultsfnc(data = cohort,
               treatment = 'started_invasive',
               strategy_type = 'start_by_hour',
               strategy_time = time,
               max_hour = 720,
               truncate_time = T,
               pooled_time_treatment_model = T,
               vars_to_lag_trt = c('HR', 'SBP', 'DBP', 'RR', 
                                   'temp', 'glucose', 'spO2', 'sofa', 'vasopressor'),
               num_lags_trt = 2,
               model_formula_vars_trt = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                          'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser'),
               parametric_model_trt = F,
               nfold_trt = 5,
               tree_depth_trt = seq(1,3,2),
               shrinkage_factor_trt = seq(0.01,0.03,0.02),
               num_trees_trt = seq(100,300,100),
               num_cores_trt = 5,
               parametric_outcome_non_DR = NULL,
               vars_to_lag_outcome = c('HR', 'SBP', 'DBP', 'RR', 
                                       'temp', 'glucose', 'spO2', 'sofa', 'vasopressor'),
               num_lags_outcome = 2,
               model_formula_vars_outcome = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                              'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser'),
               parametric_model_outcome = F,
               nfold_outcome = 5,
               tree_depth_outcome = seq(1,3,2),
               shrinkage_factor_outcome = seq(0.01,0.03,0.02),
               num_trees_outcome = seq(100,300,100),
               num_cores_outcome = 5,
               DR = T,
               nsplits=2,
               dynamic_var = 'sofa',
               dynamic_val = 7,
               dynamic_above = FALSE) 
  })

tt3_save_results_light <- tt3_save_results[2:4,]

save(tt3_save_results_light, file='tt3_start_by_hour_results_MICU_only.Rdata')
save(tt3_save_results, file='tt3_start_by_hour_results_MICU_only_with_data.Rdata')

#############################
# dont intubate until k hours
#############################

tt3_save_results2 <- 
  sapply(seq(8,48,8), function(time) {
    resultsfnc(data = cohort,
               treatment = 'started_invasive',
               strategy_type = 'dont_start_until',
               strategy_time = time,
               max_hour = 720,
               truncate_time = T,
               pooled_time_treatment_model = T,
               vars_to_lag_trt = c('HR', 'SBP', 'DBP', 'RR', 
                                   'temp', 'glucose', 'spO2', 'sofa', 'vasopressor'),
               num_lags_trt = 2,
               model_formula_vars_trt = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                          'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser'),
               parametric_model_trt = F,
               nfold_trt = 5,
               tree_depth_trt = seq(1,3,2),
               shrinkage_factor_trt = seq(0.01,0.03,0.02),
               num_trees_trt = seq(100,300,100),
               num_cores_trt = 5,
               parametric_outcome_non_DR = NULL,
               vars_to_lag_outcome = c('HR', 'SBP', 'DBP', 'RR', 
                                       'temp', 'glucose', 'spO2', 'sofa', 'vasopressor'),
               num_lags_outcome = 2,
               model_formula_vars_outcome = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                              'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser'),
               parametric_model_outcome = F,
               nfold_outcome = 5,
               tree_depth_outcome = seq(1,3,2),
               shrinkage_factor_outcome = seq(0.01,0.03,0.02),
               num_trees_outcome = seq(100,300,100),
               num_cores_outcome = 5,
               DR = T,
               nsplits=2,
               dynamic_var = 'sofa',
               dynamic_val = 6,
               dynamic_above = TRUE) 
  })

tt3_save_results2_light <- tt3_save_results2[2:4,]

save(tt3_save_results2_light, file='tt3_dont_start_until_results_MICU_only.Rdata')
save(tt3_save_results2, file='tt3_dont_start_until_results_MICU_only_with_data.Rdata')


#################################
# Target Trial 4
#################################

load("./cohort_analysis_appropriate_baseline_MICU_only.Rdata")

cohort <- cohort %>% 
  mutate(resp_distress = case_when(spO2 < 90 | noninvasive == 1 | highflow == 1 | RR > 30 ~ 1, 
                                   TRUE ~ 0))

#############################
# intubate by k hours
#############################

tt4_save_results <- 
  sapply(seq(8,48,8), function(time) {
    resultsfnc(data = cohort,
               treatment = 'started_invasive',
               strategy_type = 'start_by_hour',
               strategy_time = time,
               max_hour = 720,
               truncate_time = T,
               pooled_time_treatment_model = T,
               vars_to_lag_trt = c('HR', 'SBP', 'DBP', 'RR', 
                                   'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'resp_distress'),
               num_lags_trt = 2,
               model_formula_vars_trt = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                          'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser', 'resp_distress'),
               parametric_model_trt = F,
               nfold_trt = 5,
               tree_depth_trt = seq(1,3,2),
               shrinkage_factor_trt = seq(0.01,0.03,0.02),
               num_trees_trt = seq(100,300,100),
               num_cores_trt = 5,
               parametric_outcome_non_DR = NULL,
               vars_to_lag_outcome = c('HR', 'SBP', 'DBP', 'RR', 
                                       'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'resp_distress'),
               num_lags_outcome = 2,
               model_formula_vars_outcome = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                              'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser', 'resp_distress'),
               parametric_model_outcome = F,
               nfold_outcome = 5,
               tree_depth_outcome = seq(1,3,2),
               shrinkage_factor_outcome = seq(0.01,0.03,0.02),
               num_trees_outcome = seq(100,300,100),
               num_cores_outcome = 5,
               DR = T,
               nsplits=2,
               dynamic_var = 'resp_distress',
               dynamic_val = 1,
               dynamic_above = FALSE) 
  })

tt4_save_results_light <- tt4_save_results[2:4,]

save(tt4_save_results_light, file='tt4_start_by_hour_results_MICU_only.Rdata')
save(tt4_save_results, file='tt4_start_by_hour_results_MICU_only_with_data.Rdata')

#############################
# dont intubate until k hours
#############################

tt4_save_results2 <- 
  sapply(seq(8,48,8), function(time) {
    resultsfnc(data = cohort,
               treatment = 'started_invasive',
               strategy_type = 'dont_start_until',
               strategy_time = time,
               max_hour = 720,
               truncate_time = T,
               pooled_time_treatment_model = T,
               vars_to_lag_trt = c('HR', 'SBP', 'DBP', 'RR', 
                                   'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'resp_distress'),
               num_lags_trt = 2,
               model_formula_vars_trt = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                          'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser', 'resp_distress'),
               parametric_model_trt = F,
               nfold_trt = 5,
               tree_depth_trt = seq(1,3,2),
               shrinkage_factor_trt = seq(0.01,0.03,0.02),
               num_trees_trt = seq(100,300,100),
               num_cores_trt = 5,
               parametric_outcome_non_DR = NULL,
               vars_to_lag_outcome = c('HR', 'SBP', 'DBP', 'RR', 
                                       'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'resp_distress'),
               num_lags_outcome = 2,
               model_formula_vars_outcome = c('race', 'female', 'medicare_medicaid', 'age', 'HR', 'SBP', 'DBP', 'RR', 
                                              'temp', 'glucose', 'spO2', 'sofa', 'vasopressor', 'elixhauser', 'resp_distress'),
               parametric_model_outcome = F,
               nfold_outcome = 5,
               tree_depth_outcome = seq(1,3,2),
               shrinkage_factor_outcome = seq(0.01,0.03,0.02),
               num_trees_outcome = seq(100,300,100),
               num_cores_outcome = 5,
               DR = T,
               nsplits=2,
               dynamic_var = 'resp_distress',
               dynamic_val = 0,
               dynamic_above = TRUE) 
  })

tt4_save_results2_light <- tt4_save_results2[2:4,]

save(tt4_save_results2_light, file='tt4_dont_start_until_results_MICU_only.Rdata')
save(tt4_save_results2, file='tt4_dont_start_until_results_MICU_only_with_data.Rdata')
