
create_covariates_df <- function(covariates_df, num_lags, vars_to_lag) {
  
  if (num_lags == 0 | is.null(vars_to_lag)) {return(covariates_df)}
  
  covariates_df <- covariates_df %>% group_by(id)
  
  for (v in 1:length(vars_to_lag)) {
    
    for (i in 1:num_lags) {
      
      varname = paste0(vars_to_lag[v], '_', i)
      
      covariates_df <- covariates_df %>% mutate(., !!varname := lag(!!as.symbol(vars_to_lag[v]), i))
      
    }
  }
  
  return(covariates_df %>% ungroup)
  
}

################################################################################
################################################################################
################################################################################

calc_logloss <- function(true, pred){
  
  pred <- pmax(pmin(1-1e-15, pred), 1e-15)
  
  logloss <- -sum(true*log(pred) + (1-true)*log(1-pred)) / length(pred)
  
  return(logloss)
}

################################################################################
################################################################################
################################################################################

calc_mse <- function(true, pred){
  
  mse <- sum((true-pred)^2) / length(pred)
  
  return(mse)
}

################################################################################
################################################################################
################################################################################

get_CV_ids <- function(data, k){
  
  folds_ids <- sample(cut(1:nrow(data), breaks = k, labels = FALSE),
                      size = nrow(data),
                      replace = FALSE)
  
  return(folds_ids)
  
}

################################################################################
################################################################################
################################################################################

find_params_boosted_tree_model <- function(covariates_df, label_vector, nfold, tree_depth, 
                                           shrinkage_factor, bag_fraction, num_trees, num_cores, family='bernoulli'){
  
  params <- expand.grid(n.trees = num_trees, interaction.depth = tree_depth, 
                        shrinkage = shrinkage_factor, bag_fraction = bag_fraction)
  
  cv_test_logloss <- vector(length=nrow(params))
  
  test_preds <- vector(length=nrow(covariates_df))
  
  cl <- makeCluster(num_cores)
  
  clusterEvalQ(cl, library(gbm))
  
  clusterExport(cl, c('params', 'nfold', 'label_vector', 
                      'covariates_df', 'get_CV_ids', 'calc_logloss', 'calc_mse', 'test_preds'),
                envir=environment())
  
  gbm_out <- 
    pbsapply(1:nrow(params), function(iter){
      
      cv_ids <- get_CV_ids(covariates_df, k=nfold)
      
      for (k in 1:nfold) {
        
        train_indices <- which(cv_ids != k)
        
        test_indices <- which(cv_ids == k)
        
        model <- 
          gbm(formula = Y~., data = data.frame(Y=label_vector[train_indices], covariates_df[train_indices, ,drop=FALSE]), 
              bag.fraction = params$bag_fraction, 
              n.trees = params$n.trees[iter], interaction.depth = params$interaction.depth[iter], shrinkage = params$shrinkage[iter],
              distribution = family)
        
        test_preds[which(cv_ids == k)] <- predict(model, type='response', n.trees = model$n.trees,
                                                  newdata = covariates_df[test_indices, ,drop=FALSE])
        
      }
      
      if(family=='bernoulli'){calc_logloss(label_vector, test_preds)}else{calc_mse(label_vector, test_preds)}
      
    }, cl=cl)
  
  stopCluster(cl)
  
  best_params <- as.list(c(params[resample(which(gbm_out == min(gbm_out)), 1), ]))
  best_params$attempted_params <- params
  best_params$logloss <- gbm_out
  
  return(best_params)
  
}

################################################################################
################################################################################
################################################################################

estimate_prob_boosted_tree_model <- function(covariates_df, label_vector,
                                             params, predict_data, boosted_tree_family = 'bernoulli'){
  
  boosted_tree_model <- 
    gbm(
      formula = Y~., 
      data = data.frame(Y=label_vector, covariates_df),
      bag.fraction = params$bag_fraction,
      interaction.depth = params$interaction.depth,
      shrinkage = params$shrinkage,
      n.trees = params$n.trees,
      distribution = boosted_tree_family
    )
  
  out <- predict(boosted_tree_model, newdata=predict_data, type="response", n.trees=boosted_tree_model$n.trees)
  
  
  return(out)
  
}

################################################################################
################################################################################
################################################################################

create_following_strategy_var <- function(treatment, 
                                          strategy_type = 'start_by_hour',
                                          strategy_time, 
                                          dynamic_var = NULL,
                                          dynamic_val = NULL,
                                          dynamic_above = NULL,
                                          data) {
  
  if (strategy_type == 'start_by_hour') {
    
    data <- data %>% group_by(id)
    
    if (is.null(dynamic_var) | is.null(dynamic_val) | is.null(dynamic_above)) {
      
      data <- data %>%
        mutate(following_strategy_k = case_when(hour < strategy_time ~ 1,
                                                hour == strategy_time &
                                                  !!sym(treatment) == 1  ~ 1,
                                                hour == strategy_time & 
                                                  !!sym(treatment) == 0 & 
                                                  left_icu == 0 ~ 0))
      
    } else {
      
      data <- data %>%
        mutate(following_strategy_k = case_when(hour < strategy_time ~ 1,
                                                hour == strategy_time &
                                                  !!sym(treatment) == 1 ~ 1,
                                                hour == strategy_time & 
                                                  !!sym(treatment) == 0 &
                                                  left_icu == 0 &
                                                  (
                                                    (dynamic_above & (!!sym(dynamic_var) <= dynamic_val)) |
                                                      (!dynamic_above & (!!sym(dynamic_var) >= dynamic_val)) 
                                                  ) ~ 0,
                                                hour == strategy_time & 
                                                  !!sym(treatment) == 0 &
                                                  left_icu == 0 &
                                                  (
                                                    (dynamic_above & (!!sym(dynamic_var) > dynamic_val)) |
                                                      (!dynamic_above & (!!sym(dynamic_var) < dynamic_val)) 
                                                  ) ~ 1))
      
    }
    
    data <- data %>%
      fill(following_strategy_k) %>%
      ungroup()
    
  }
  
  if (strategy_type == 'dont_start_until') {
    
    data <- data %>% group_by(id)
    
    if (is.null(dynamic_var) | is.null(dynamic_val) | is.null(dynamic_above)) {
      
      data <- data %>%
        mutate(following_strategy_k = case_when(hour < strategy_time &
                                                  !!sym(treatment) == 1 ~ 0,
                                                TRUE ~ 1))
      
    } else {
      
      data <- data %>%
        mutate(exempt = case_when(hour < strategy_time &
                                    cumsum(!!sym(treatment)) == 1 & 
                                    (
                                      (dynamic_above & (!!sym(dynamic_var) > dynamic_val)) |
                                        (!dynamic_above & (!!sym(dynamic_var) < dynamic_val)) 
                                    ) ~ 1,
                                  TRUE ~ NA_real_)) %>% 
        fill(exempt, .direction = 'updown') %>%
        mutate(following_strategy_k = case_when(hour < strategy_time &
                                                  !!sym(treatment) == 1 &
                                                  is.na(exempt) ~ 0,
                                                TRUE ~ 1)) %>%
        dplyr::select(-exempt)
      
    }
    
    data <- data %>%
      mutate(following_strategy_k = cumprod(following_strategy_k)) %>%
      ungroup()
    
  }
  
  return(data)
  
}

################################################################################
################################################################################
################################################################################

weightsfnc_start_by_hour <- function(treatment,
                                     dynamic_var = NULL,
                                     dynamic_val = NULL,
                                     dynamic_above = NULL,
                                     start_by_hour,
                                     min_hour,
                                     max_hour,
                                     truncate_time,
                                     pooled_time_treatment_model = F,
                                     model_formula_vars,
                                     vars_to_lag,
                                     num_lags,
                                     parametric_model,
                                     data,
                                     nfold,
                                     tree_depth,
                                     shrinkage_factor,
                                     num_trees,
                                     bag_fraction,
                                     num_cores){
  
  data <- data %>% 
    dplyr::select(c('id','hour','following_strategy_k',
                    'outcome','left_icu','split_ids',
                    all_of(model_formula_vars),
                    all_of(treatment))) %>%
    mutate(prob_trt = 1)
  
  if (pooled_time_treatment_model) {
    
    if (is.null(dynamic_var) | is.null(dynamic_val) | is.null(dynamic_above)) {
      
      subset_data_k <- data %>% dplyr::select(-prob_trt) %>% 
        group_by(id) %>%
        filter(left_icu == 0 & lag(!!sym(treatment),1,default=0)==0) %>%
        ungroup() 
      
    } else {
      
      subset_data_k <- data %>% dplyr::select(-prob_trt) %>% 
        group_by(id) %>%
        filter(left_icu == 0 & lag(!!sym(treatment),1,default=0)==0 & 
                 ((dynamic_above & (!!sym(dynamic_var) <= dynamic_val)) |
                    (!dynamic_above & (!!sym(dynamic_var) >= dynamic_val)))) %>%
        ungroup() 
      
    }
    
  } else {
    
    data_k <- create_covariates_df(covariates_df = data %>% dplyr::select(-prob_trt), 
                                   num_lags = min(c(num_lags,start_by_hour-min_hour)), 
                                   vars_to_lag = vars_to_lag)
    
    if (is.null(dynamic_var) | is.null(dynamic_val) | is.null(dynamic_above)) {
      
      subset_data_k <- data_k %>% 
        group_by(id) %>%
        filter(hour == start_by_hour & left_icu == 0 & lag(!!sym(treatment),1,default=0)==0) %>%
        ungroup() 
      
    } else {
      
      subset_data_k <- data_k %>% 
        group_by(id) %>%
        filter(hour == start_by_hour & left_icu == 0 & lag(!!sym(treatment),1,default=0)==0 & 
                 ((dynamic_above & (!!sym(dynamic_var) <= dynamic_val)) |
                    (!dynamic_above & (!!sym(dynamic_var) >= dynamic_val)))) %>%
        ungroup() 
      
    }
    
  }
  
  if (pooled_time_treatment_model) {
    
    model_vars_to_remove <- c('id','following_strategy_k', treatment,
                              'outcome','left_icu','split_ids')
    
  } else {
    
    model_vars_to_remove <- c('id','hour','following_strategy_k', treatment,
                              'outcome','left_icu','split_ids')
    
  }
  
  for (split in 1:length(unique(data$split_ids))) {
    
    if (parametric_model) {
      
      model_formula_RHS <- paste(
        names(subset_data_k)[which(!names(subset_data_k) %in% model_vars_to_remove)], 
        collapse=" + "
      )
      
      if (length(unique(data$split_ids)) == 1) {
        
        wt_model <- 
          glm(formula = as.formula(paste(treatment, model_formula_RHS, sep=" ~ ")),
              data = subset_data_k,
              family = binomial())
        
      } else {
        
        wt_model <- 
          glm(formula = as.formula(paste(treatment, model_formula_RHS, sep=" ~ ")),
              data = subset_data_k %>% filter(split_ids != split),
              family = binomial())
        
      }
      
      prob_trt_k <- predict(wt_model, newdata = subset_data_k %>% filter(hour == start_by_hour & split_ids == split), type='response')
      
    } else {
      
      params_trt <- find_params_boosted_tree_model(covariates_df = subset_data_k %>% 
                                                     filter(split_ids != split) %>% 
                                                     dplyr::select(-all_of(model_vars_to_remove)), 
                                                   label_vector = subset_data_k %>% 
                                                     filter(split_ids != split) %>% 
                                                     dplyr::select(all_of(treatment)) %>% {.[[1]]},
                                                   nfold = nfold,
                                                   tree_depth = tree_depth,
                                                   shrinkage_factor = shrinkage_factor,
                                                   num_trees = num_trees,
                                                   bag_fraction = bag_fraction,
                                                   family = 'bernoulli',
                                                   num_cores = num_cores)
      
      prob_trt_k <- estimate_prob_boosted_tree_model(covariates_df = subset_data_k %>% 
                                                       filter(split_ids != split) %>% 
                                                       dplyr::select(-all_of(model_vars_to_remove)), 
                                                     label_vector = subset_data_k %>% 
                                                       filter(split_ids != split) %>% 
                                                       dplyr::select(all_of(treatment)) %>% {.[[1]]},
                                                     params = params_trt,
                                                     boosted_tree_family = 'bernoulli',
                                                     predict_data = subset_data_k %>% 
                                                       filter(hour == start_by_hour & split_ids == split) %>% 
                                                       dplyr::select(-all_of(model_vars_to_remove)))
      
    }
    
    if (is.null(dynamic_var) | is.null(dynamic_val) | is.null(dynamic_above)) {
      
      data <- data %>% 
        group_by(id) %>%
        mutate(model_prob = case_when(hour==start_by_hour & split_ids == split & left_icu == 0 & lag(!!sym(treatment),1,default=0)==0 ~ 1,
                                      TRUE ~ 0)) %>%
        ungroup()
      
      data[which(data$model_prob == 1), ]$prob_trt <- prob_trt_k
      
      data <- data %>% 
        dplyr::select(-model_prob)
      
    } else {
      
      data <- data %>% 
        group_by(id) %>%
        mutate(model_prob = case_when(hour==start_by_hour & split_ids == split & left_icu == 0 & lag(!!sym(treatment),1,default=0)==0 &
                                        ((dynamic_above & (!!sym(dynamic_var) <= dynamic_val)) |
                                        (!dynamic_above & (!!sym(dynamic_var) >= dynamic_val))) ~ 1,
                                      TRUE ~ 0)) %>%
        ungroup() 
      
      data[which(data$model_prob == 1), ]$prob_trt <- prob_trt_k
      
      data <- data %>% 
        dplyr::select(-model_prob)
      
    }
    
  }
  
  data <- data %>% 
    mutate(
      weight = case_when(hour < start_by_hour | 
                           (hour >= start_by_hour & following_strategy_k == 1) ~ 1 / prob_trt,
                         hour >= start_by_hour & following_strategy_k == 0 ~ 0)
    )
  
  return(data)
  
}

################################################################################
################################################################################
################################################################################

weightsfnc_dont_start_until <- function(treatment,
                                        dynamic_var = NULL,
                                        dynamic_val = NULL,
                                        dynamic_above = NULL,
                                        dont_start_until,
                                        min_hour,
                                        max_hour,
                                        truncate_time,
                                        pooled_time_treatment_model = F,
                                        model_formula_vars,
                                        vars_to_lag,
                                        num_lags,
                                        parametric_model,
                                        data,
                                        nfold,
                                        tree_depth,
                                        shrinkage_factor,
                                        num_trees,
                                        bag_fraction,
                                        num_cores){
  
  data <- data %>% 
    dplyr::select(c('id','hour', 'following_strategy_k',
                    'outcome','left_icu','split_ids',
                    all_of(treatment),
                    all_of(model_formula_vars))) %>%
    mutate(prob_trt = 1)
  
  if (pooled_time_treatment_model) {
    
    if (is.null(dynamic_var) | is.null(dynamic_val) | is.null(dynamic_above)) {
      
      data_k <- data %>% dplyr::select(-prob_trt) %>% 
        group_by(id) %>%
        filter(left_icu == 0 & cumsum(lag(!!sym(treatment),1,default=0)) == 0) %>%
        ungroup()
      
    } else {
      
      data_k <- data %>% dplyr::select(-prob_trt) %>% 
        group_by(id) %>%
        filter(left_icu == 0 & cumsum(lag(!!sym(treatment),1,default=0)) == 0 & 
                 ((dynamic_above & (!!sym(dynamic_var) <= dynamic_val)) |
                    (!dynamic_above & (!!sym(dynamic_var) >= dynamic_val)))) %>%
        ungroup()
      
    }
    
    for (split in 1:length(unique(data$split_ids))) {
      
      if(parametric_model) {
        
        model_formula_RHS <- paste(
          names(data_k)[which(!names(data_k) %in% c('id', 'following_strategy_k', treatment,
                                                'outcome','left_icu','split_ids'))], 
          collapse=" + "
        )
        
        if (length(unique(data$split_ids)) == 1) {
          
          model <- 
            glm(formula = as.formula(paste(treatment, model_formula_RHS, sep=" ~ ")),
                data = data_k,
                family = quasibinomial())
          
        } else {
          
          model <- 
            glm(formula = as.formula(paste(treatment, model_formula_RHS, sep=" ~ ")),
                data = data_k %>% filter(split_ids != split),
                family = quasibinomial())
          
        }
        
        prob_trt_k <- 1-predict(model, newdata = data_k %>% filter(hour < dont_start_until & split_ids == split), type='response')
        
      } else {
        
        params_trt <- find_params_boosted_tree_model(covariates_df = data_k %>% 
                                                       filter(split_ids != split) %>% 
                                                       dplyr::select(-c('id','following_strategy_k', all_of(treatment),
                                                                        'outcome','left_icu','split_ids')), 
                                                     label_vector = data_k %>% 
                                                       filter(split_ids != split) %>% 
                                                       dplyr::select(all_of(treatment)) %>% {.[[1]]},
                                                     nfold = nfold,
                                                     tree_depth = tree_depth,
                                                     shrinkage_factor = shrinkage_factor,
                                                     num_trees = num_trees,
                                                     bag_fraction = bag_fraction,
                                                     family = 'bernoulli',
                                                     num_cores = num_cores)
        
        prob_trt_k <- 1-estimate_prob_boosted_tree_model(covariates_df = data_k %>% 
                                                           filter(split_ids != split) %>% 
                                                           dplyr::select(-c('id','following_strategy_k', all_of(treatment),
                                                                            'outcome','left_icu','split_ids')), 
                                                         label_vector = data_k %>% 
                                                           filter(split_ids != split) %>% 
                                                           dplyr::select(all_of(treatment)) %>% {.[[1]]},
                                                         params = params_trt,
                                                         boosted_tree_family = 'bernoulli',
                                                         predict_data = data_k %>% 
                                                           filter(hour < dont_start_until & split_ids == split) %>% 
                                                           dplyr::select(-c('id','following_strategy_k', all_of(treatment),
                                                                            'outcome','left_icu','split_ids')))
        
      }
      
      if (is.null(dynamic_var) | is.null(dynamic_val) | is.null(dynamic_above)) {
        
        data <- data %>% 
          group_by(id) %>%
          mutate(model_prob = case_when(hour < dont_start_until & split_ids == split & left_icu == 0 & cumsum(lag(!!sym(treatment),1,default=0)) == 0 ~ 1,
                                        TRUE ~ 0)) %>%
          ungroup()
        
        data[which(data$model_prob == 1), ]$prob_trt <- prob_trt_k
        
        data <- data %>% 
          dplyr::select(-model_prob)
        
      } else {
        
        data <- data %>% 
          group_by(id) %>%
          mutate(model_prob = case_when(hour < dont_start_until & split_ids == split & left_icu == 0 & cumsum(lag(!!sym(treatment),1,default=0)) == 0 &
                                          ((dynamic_above & (!!sym(dynamic_var) <= dynamic_val)) |
                                             (!dynamic_above & (!!sym(dynamic_var) >= dynamic_val))) ~ 1,
                                        TRUE ~ 0)) %>%
          ungroup()
        
        data[which(data$model_prob == 1), ]$prob_trt <- prob_trt_k
        
        data <- data %>% 
          dplyr::select(-model_prob)
        
      }
      
    }
    
  } else {
    
    for (k in min_hour:(dont_start_until-1)) {
      
      data_k <- create_covariates_df(covariates_df = data %>% dplyr::select(-prob_trt), 
                                     num_lags = min(c(num_lags,k-min_hour)), 
                                     vars_to_lag = vars_to_lag)
      
      if (is.null(dynamic_var) | is.null(dynamic_val) | is.null(dynamic_above)) {
        
        subset_data_k <- data_k %>% 
          group_by(id) %>%
          filter(hour == k & left_icu == 0 & cumsum(lag(!!sym(treatment),1,default=0)) == 0) %>%
          ungroup()
        
      } else {
        
        subset_data_k <- data_k %>% 
          group_by(id) %>%
          filter(hour == k & left_icu == 0 & cumsum(lag(!!sym(treatment),1,default=0)) == 0 & 
                   ((dynamic_above & (!!sym(dynamic_var) <= dynamic_val)) |
                      (!dynamic_above & (!!sym(dynamic_var) >= dynamic_val)))) %>%
          ungroup()
        
      }
      
      for (split in 1:length(unique(data$split_ids))) {
        
        if(parametric_model) {
          
          model_formula_RHS <- paste(
            names(subset_data_k)[which(!names(subset_data_k) %in% c('id','hour','following_strategy_k', treatment,
                                                                    'outcome','left_icu','split_ids'))], 
            collapse=" + "
          )
          
          if (length(unique(data$split_ids)) == 1) {
            
            model_k <- 
              glm(formula = as.formula(paste(treatment, model_formula_RHS, sep=" ~ ")),
                  data = subset_data_k,
                  family = quasibinomial())
            
          } else {
            
            model_k <- 
              glm(formula = as.formula(paste(treatment, model_formula_RHS, sep=" ~ ")),
                  data = subset_data_k %>% filter(split_ids != split),
                  family = quasibinomial())
            
          }
          
          prob_trt_k <- 1-predict(model_k, newdata = subset_data_k %>% filter(split_ids == split), type='response')
          
        } else {
          
          params_trt_k <- find_params_boosted_tree_model(covariates_df = subset_data_k %>% 
                                                           filter(split_ids != split) %>% 
                                                           dplyr::select(-c('id','hour','following_strategy_k', all_of(treatment),
                                                                            'outcome','left_icu','split_ids')), 
                                                         label_vector = subset_data_k %>% 
                                                           filter(split_ids != split) %>% 
                                                           dplyr::select(all_of(treatment)) %>% {.[[1]]},
                                                         nfold = nfold,
                                                         tree_depth = tree_depth,
                                                         shrinkage_factor = shrinkage_factor,
                                                         num_trees = num_trees,
                                                         bag_fraction = bag_fraction,
                                                         family = 'bernoulli',
                                                         num_cores = num_cores)
          
          prob_trt_k <- 1-estimate_prob_boosted_tree_model(covariates_df = subset_data_k %>% 
                                                             filter(split_ids != split) %>% 
                                                             dplyr::select(-c('id','hour','following_strategy_k', all_of(treatment),
                                                                              'outcome','left_icu','split_ids')), 
                                                           label_vector = subset_data_k %>% 
                                                             filter(split_ids != split) %>% 
                                                             dplyr::select(all_of(treatment)) %>% {.[[1]]},
                                                           params = params_trt_k,
                                                           boosted_tree_family = 'bernoulli',
                                                           predict_data = subset_data_k %>% 
                                                             filter(split_ids == split) %>% 
                                                             dplyr::select(-c('id','hour','following_strategy_k', all_of(treatment),
                                                                              'outcome','left_icu','split_ids')))
          
        }
        
        if (is.null(dynamic_var) | is.null(dynamic_val) | is.null(dynamic_above)) {
          
          data <- data %>% 
            group_by(id) %>%
            mutate(model_prob = case_when(hour==k & split_ids == split & left_icu == 0 & cumsum(lag(!!sym(treatment),1,default=0)) == 0 ~ 1,
                                          TRUE ~ 0)) %>%
            ungroup()
          
          data[which(data$model_prob == 1), ]$prob_trt <- prob_trt_k
          
          data <- data %>% 
            dplyr::select(-model_prob)
          
        } else {
          
          data <- data %>% 
            group_by(id) %>%
            mutate(model_prob = case_when(hour==k & split_ids == split & left_icu == 0 & cumsum(lag(!!sym(treatment),1,default=0)) == 0 &
                                            ((dynamic_above & (!!sym(dynamic_var) <= dynamic_val)) |
                                               (!dynamic_above & (!!sym(dynamic_var) >= dynamic_val))) ~ 1,
                                          TRUE ~ 0)) %>%
            ungroup()
          
          data[which(data$model_prob == 1), ]$prob_trt <- prob_trt_k
          
          data <- data %>% 
            dplyr::select(-model_prob)
          
        }
        
      }
      
    }
    
  }
  
  data <- data %>% 
    mutate(
      weight = case_when(following_strategy_k == 1 ~ 1 / prob_trt,
                         following_strategy_k == 0 ~ 0)
    )
  
  return(data)
  
}

################################################################################
################################################################################
################################################################################

iterative_outcomefnc <- function(max_hour,
                                 min_hour,
                                 strategy_time,
                                 truncate_time,
                                 model_formula_vars,
                                 vars_to_lag,
                                 num_lags,
                                 parametric_model,
                                 data,
                                 nfold,
                                 tree_depth,
                                 shrinkage_factor,
                                 num_trees,
                                 bag_fraction,
                                 num_cores) {
  
  data <- data %>% 
    dplyr::select(c('id','hour','following_strategy_k',
                    'outcome','left_icu','split_ids',
                    all_of(model_formula_vars))) %>%
    mutate(h_k_final = 0,
           outcome_resid = 0)
  
  if (truncate_time & max_hour > strategy_time) {
    
    data <- data %>% 
      mutate(outcome_hour = case_when(outcome == 1 ~ as.numeric(hour),
                                      TRUE ~ 1)) %>% 
      group_by(id) %>%
      mutate(outcome_hour = prod(outcome_hour)) %>%
      ungroup() %>% 
      mutate(outcome = case_when(hour == strategy_time & outcome_hour >= hour ~ 1,
                                 TRUE ~ outcome)) %>%
      filter(hour <= strategy_time) %>%
      dplyr::select(-outcome_hour)
    
    max_hour = strategy_time
    
  }
  
  data <- data %>%
    group_by(id) %>% 
    mutate(individual_max_hour = max(hour)) %>% 
    ungroup()
  
  for (split in 1:length(unique(data$split_ids))) {
    
    data <- data %>% 
      mutate(h_k_1 = outcome,
             h_k = 0)
  
    for (k in max_hour:min_hour) {
      
      data_k <- create_covariates_df(covariates_df = data %>% dplyr::select(-c(h_k, h_k_final, outcome, outcome_resid, individual_max_hour)), 
                                     num_lags = min(c(num_lags,k-min_hour)), 
                                     vars_to_lag = vars_to_lag)
      
      subset_data_k <- filter(data_k, hour == k & (lag(following_strategy_k, 1) == 1 | hour == min_hour))
      subset_data_k_following_strategy <- filter(subset_data_k, following_strategy_k == 1)
      
      if(parametric_model) {
        
        model_formula_RHS <- paste(
          names(subset_data_k)[which(!names(subset_data_k) %in% c('id','hour','following_strategy_k',
                                                                  'h_k_1','left_icu','split_ids'))], 
          collapse=" + "
        )
        
        model_k <- 
          glm(formula = as.formula(paste("h_k_1", model_formula_RHS, sep=" ~ ")),
              data = subset_data_k_following_strategy %>% filter(split_ids != split),
              family = quasibinomial())
        
        prob_outcome_k <- predict(model_k, newdata = subset_data_k, type='response')
        
      } else {
        
        params_outcome_k <- find_params_boosted_tree_model(covariates_df = subset_data_k_following_strategy %>% 
                                                             filter(split_ids != split) %>% 
                                                             dplyr::select(-c('id','hour','following_strategy_k',
                                                                              'h_k_1','left_icu','split_ids')), 
                                                           label_vector = subset_data_k_following_strategy %>% 
                                                             filter(split_ids != split) %>% 
                                                             dplyr::select(c(h_k_1)) %>% {.[[1]]},
                                                           nfold = nfold,
                                                           tree_depth = tree_depth,
                                                           shrinkage_factor = shrinkage_factor,
                                                           num_trees = num_trees,
                                                           bag_fraction = bag_fraction,
                                                           family = 'gaussian',
                                                           num_cores = num_cores)
        
        prob_outcome_k <- estimate_prob_boosted_tree_model(covariates_df = subset_data_k_following_strategy %>% 
                                                             filter(split_ids != split) %>% 
                                                             dplyr::select(-c('id','hour','following_strategy_k',
                                                                              'h_k_1','left_icu','split_ids')), 
                                                           label_vector = subset_data_k_following_strategy %>% 
                                                             filter(split_ids != split) %>% 
                                                             dplyr::select(c(h_k_1)) %>% {.[[1]]},
                                                           params = params_outcome_k,
                                                           boosted_tree_family = 'gaussian',
                                                           predict_data = subset_data_k %>%
                                                             dplyr::select(-c('id','hour','following_strategy_k',
                                                                              'h_k_1','left_icu','split_ids')))
        
      }
      
      data[which(data$hour == k & (lag(data$following_strategy_k, 1) == 1 | data$hour == min_hour)), ]$h_k <- prob_outcome_k
      
      if (k != min_hour) {
        data <- data %>% 
          mutate(
            h_k_1 = case_when(hour == k-1 & h_k_1 == 0 & hour != individual_max_hour ~ lead(h_k,1),
                              TRUE ~ as.numeric(h_k_1))
          )
      }
      
    }
    
    data[which(data$split_ids == split), ]$outcome_resid <- 
      data[which(data$split_ids == split), ]$h_k_1 - data[which(data$split_ids == split), ]$h_k
    
    data[which(data$split_ids == split), ]$h_k_final <- data[which(data$split_ids == split), ]$h_k
    
  }
  
  return(data %>% dplyr::select(-c(individual_max_hour, h_k, h_k_1)) %>% rename(h_k = h_k_final))
  
}

################################################################################
################################################################################
################################################################################

resultsfnc <- function(data, 
                       treatment, 
                       strategy_type = 'start_by_hour',
                       strategy_time,
                       truncate_time = T,
                       pooled_time_treatment_model = F,
                       max_hour = NULL, 
                       vars_to_lag_trt = NULL,
                       num_lags_trt = NULL,
                       model_formula_vars_trt,
                       parametric_model_trt = F,
                       nfold_trt = NULL,
                       tree_depth_trt = NULL,
                       shrinkage_factor_trt = NULL,
                       num_trees_trt = NULL,
                       num_cores_trt = NULL,
                       bag_fraction_trt = c(0.5),
                       parametric_outcome_non_DR = NULL,
                       trim_weights_quantile = 1,
                       vars_to_lag_outcome = NULL,
                       num_lags_outcome = NULL,
                       model_formula_vars_outcome = NULL,
                       parametric_model_outcome = NULL,
                       nfold_outcome = NULL,
                       tree_depth_outcome = NULL,
                       shrinkage_factor_outcome = NULL,
                       num_trees_outcome = NULL,
                       bag_fraction_outcome = c(0.5),
                       num_cores_outcome = NULL,
                       nsplits = NULL,
                       DR = T,
                       dynamic_var = NULL,
                       dynamic_val = NULL,
                       dynamic_above = NULL){

  min_hour <- min(data$hour)
  
  if (is.null(max_hour)) {max_hour <- max(data$hour)} else {
    data <- data %>% filter(hour <= max_hour)
  }
  
  data <- data %>%
    create_following_strategy_var(treatment = treatment, 
                                  strategy_type = strategy_type,
                                  strategy_time = strategy_time,
                                  dynamic_var = dynamic_var,
                                  dynamic_val = dynamic_val,
                                  dynamic_above = dynamic_above,
                                  data=.)
  
  if (!is.null(nsplits)) {
    split_ids <- sample(rep(1:nsplits,ceiling(length(unique(data$id))/nsplits))[1:length(unique(data$id))])
  } else {
    split_ids <- rep(1,length(unique(data$id)))
  }
  
  data <- left_join(data, data.frame(id = unique(data$id), split_ids))
  
  if (!DR) {truncate_time = F}
  
  if (strategy_type == 'start_by_hour') {
    
    data_wts <- data %>%
      weightsfnc_start_by_hour(treatment = treatment,
                               dynamic_var = dynamic_var,
                               dynamic_val = dynamic_val,
                               dynamic_above = dynamic_above,
                               model_formula_vars = model_formula_vars_trt,
                               vars_to_lag = vars_to_lag_trt,
                               num_lags = num_lags_trt,
                               start_by_hour = strategy_time,
                               min_hour = min_hour,
                               max_hour = max_hour,
                               truncate_time = truncate_time,
                               pooled_time_treatment_model = pooled_time_treatment_model,
                               parametric_model = parametric_model_trt,
                               data = .,
                               nfold = nfold_trt,
                               tree_depth = tree_depth_trt,
                               shrinkage_factor = shrinkage_factor_trt,
                               num_trees = num_trees_trt,
                               bag_fraction = bag_fraction_trt,
                               num_cores = num_cores_trt)
    
  }
  
  if (strategy_type == 'dont_start_until') {
    
    data_wts <- data %>%
      weightsfnc_dont_start_until(treatment = treatment,
                                  dynamic_var = dynamic_var,
                                  dynamic_val = dynamic_val,
                                  dynamic_above = dynamic_above,
                                  model_formula_vars = model_formula_vars_trt,
                                  vars_to_lag = vars_to_lag_trt,
                                  num_lags = num_lags_trt,
                                  dont_start_until = strategy_time,
                                  min_hour = min_hour,
                                  max_hour = max_hour,
                                  truncate_time = truncate_time,
                                  pooled_time_treatment_model = pooled_time_treatment_model,
                                  parametric_model = parametric_model_trt,
                                  data = .,
                                  nfold = nfold_trt,
                                  tree_depth = tree_depth_trt,
                                  shrinkage_factor = shrinkage_factor_trt,
                                  num_trees = num_trees_trt,
                                  bag_fraction = bag_fraction_trt,
                                  num_cores = num_cores_trt)
    
  }
  
  data_wts <- data_wts %>%
    group_by(id) %>%
    mutate(weight = cumprod(weight)) %>%
    ungroup() %>%
    mutate(weight = case_when(weight > quantile(weight, trim_weights_quantile) ~ quantile(weight, trim_weights_quantile), TRUE ~ weight))
  
  if (truncate_time & max_hour > strategy_time) {
    
    data_wts <- data_wts %>% 
      filter(hour <= strategy_time)
    
  }
  
  if (!DR){
    
    if (parametric_outcome_non_DR) {
      
      standard_data <- data.frame(hour = min_hour:max_hour)
      
      outcome_model <- glm(outcome ~ 
                             ns(hour, knots = seq(min_hour+1,max_hour-1,2), Boundary.knots = c(min_hour,max_hour)), 
                           data = data_wts,
                           family = binomial(),
                           weights = weight
      )
      
      predicted <- standard_data %>%
        mutate(cond_surv = 1-predict(outcome_model, ., type="response")) %>% mutate(CI = 1-cumprod(cond_surv)) 
      
    } else {
      
      predicted <- data_wts %>% 
        group_by(hour) %>% 
        summarise(cond_surv = 1- (sum(outcome*weight) / sum(weight))) %>%
        {data.frame(hour = min_hour:max_hour, CI = 1-cumprod(.$cond_surv))}
      
    }
    
    return(list(data = data_wts,
                outcome = data.frame(CI=c(0,predicted[["CI"]])) %>% mutate(hour = min_hour:(max_hour+1))))
    
  } else {
    
    data_outcome <- data %>% 
      iterative_outcomefnc(max_hour = max_hour,
                           min_hour = min_hour,
                           strategy_time = strategy_time,
                           truncate_time = truncate_time,
                           model_formula_vars = model_formula_vars_outcome,
                           vars_to_lag = vars_to_lag_outcome,
                           num_lags = num_lags_outcome,
                           parametric_model = parametric_model_outcome,
                           data = .,
                           nfold = nfold_outcome,
                           tree_depth = tree_depth_outcome,
                           shrinkage_factor = shrinkage_factor_outcome,
                           num_trees = num_trees_outcome,
                           bag_fraction = bag_fraction_outcome,
                           num_cores = num_cores_outcome)
    
    data <- data_outcome %>% mutate(prob_trt = data_wts$prob_trt,
                                    weight = data_wts$weight)
    
    data <- data %>% 
      mutate(D_k = (following_strategy_k*weight)*(outcome_resid))
    
    data <- data %>% 
      group_by(id) %>% 
      mutate(cumsum_Dk = cumsum(D_k)) %>% 
      ungroup()
    
    EIF_data <- data.frame(
      EIF = (data %>% group_by(id) %>% filter(hour == min(max_hour, max(hour))))$cumsum_Dk +
        (data %>% group_by(id) %>% filter(hour == min_hour))$h_k,
      split_ids = data %>% filter(hour==min_hour) %>% {.$split_ids}
    )
    
    DR_outcome <- 
      sapply(1:nsplits, function(split) {
        
        split_data <- EIF_data %>% filter(split_ids == split)
        
        mean(split_data$EIF)
        
      })
    
    DR_var <- 
      sapply(1:nsplits, function(split) {
        
        split_data <- EIF_data %>% filter(split_ids == split)
        
        sum((split_data$EIF - mean(split_data$EIF))^2) / (length(split_data$EIF)^2)
        
      })
    
    return(list(data = data,
                EIF_data = EIF_data,
                DR_outcome = DR_outcome,
                DR_var = DR_var))
    
  }
}

################################################################################
################################################################################
################################################################################

combine_DR_splits <- function(results, CI_level) {
  
  outcome = mean(results$DR_outcome)
  
  se = sqrt((1/(length(results$DR_var)^2))*sum(results$DR_var))
  
  CI = c(outcome + abs(qnorm((1-CI_level)/2))*se, 
         outcome - abs(qnorm((1-CI_level)/2))*se)
  
  return(list(outcome = outcome,
              se = se,
              CI = CI))
  
}

################################################################################
################################################################################
################################################################################

compute_risk_difference <- function(results1, results2, CI_level) {
  
  outcome <- mean(results1$EIF_data$EIF - results2$EIF_data$EIF)
  
  se <- sqrt(
    sum(((results1$EIF_data$EIF - results2$EIF_data$EIF) - outcome)^2) / (length(results1$EIF_data$EIF)^2)
  )
  
  CI = c(outcome + abs(qnorm((1-CI_level)/2))*se, 
         outcome - abs(qnorm((1-CI_level)/2))*se)
  
  return(list(outcome = outcome,
              se = se,
              CI = CI))
  
}

