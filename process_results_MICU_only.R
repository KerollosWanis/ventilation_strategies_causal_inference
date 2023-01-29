source("./src/dependencies.R")
source("./src/functions.R")

#################################
# Target Trial 1
#################################

# point estimates and confidence intervals for start by hour strategies

load("tt1_start_by_hour_results_MICU_only_with_data.Rdata")

tt1_start_by_hour_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results[,x], CI_level=0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results[,x], CI_level=0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results[,x], CI_level=0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to start intubation') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.075,0.20))
  }


# point estimates and confidence intervals for dont start until strategies

load("tt1_dont_start_until_results_MICU_only_with_data.Rdata")

tt1_dont_start_until_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results2[,x], CI_level=0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results2[,x], CI_level=0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results2[,x], CI_level=0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to delay intubation') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.075,0.20))
  }

# combine graphs for target trial 1

target_trial_1 <- 
  ggarrange(tt1_start_by_hour_graph, tt1_dont_start_until_graph,
            ncol=1, nrow=2, common.legend = TRUE, legend="bottom",
            font.label=list(size=20))
annotate_figure(target_trial_1, 
                left = text_grob('30-day cumulative incidence of death', 
                                 color = "black", rot = 90))

ggsave("Target_trial_1_graph_MICU_only.png", dpi=600)


#################################
# Target Trial 2
#################################

# point estimates and confidence intervals for start by hour strategies

load("tt2_start_by_hour_results_MICU_only_with_data.Rdata")

tt2_start_by_hour_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results[,x], CI_level=0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results[,x], CI_level=0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results[,x], CI_level=0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to start intubation') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.075,0.20))
  }


# point estimates and confidence intervals for dont start until strategies

load("tt2_dont_start_until_results_MICU_only_with_data.Rdata")

tt2_dont_start_until_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results2[,x], CI_level=0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results2[,x], CI_level=0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results2[,x], CI_level=0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to delay intubation') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.075,0.20))
  }

# combine graphs for target trial 2

target_trial_2 <- 
  ggarrange(tt2_start_by_hour_graph, tt2_dont_start_until_graph,
            ncol=1, nrow=2, common.legend = TRUE, legend="bottom",
            font.label=list(size=20))
annotate_figure(target_trial_2, 
                left = text_grob('30-day cumulative incidence of death', 
                                 color = "black", rot = 90))

ggsave("Target_trial_2_graph_MICU_only.png", dpi=600)


#################################
# Target Trial 3
#################################

# point estimates and confidence intervals for start by hour strategies

load("tt3_start_by_hour_results_MICU_only_with_data.Rdata")

tt3_start_by_hour_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results[,x], CI_level=0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results[,x], CI_level=0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results[,x], CI_level=0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to start intubation') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.075,0.20))
  }


# point estimates and confidence intervals for dont start until strategies

load("tt3_dont_start_until_results_MICU_only_with_data.Rdata")

tt3_dont_start_until_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results2[,x], CI_level=0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results2[,x], CI_level=0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results2[,x], CI_level=0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to delay intubation') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.075,0.20))
  }

# combine graphs for target trial 3

target_trial_3 <- 
  ggarrange(tt3_start_by_hour_graph, tt3_dont_start_until_graph,
            ncol=1, nrow=2, common.legend = TRUE, legend="bottom",
            font.label=list(size=20))
annotate_figure(target_trial_3, 
                left = text_grob('30-day cumulative incidence of death', 
                                 color = "black", rot = 90))

ggsave("Target_trial_3_graph_MICU_only.png", dpi=600)


#################################
# Target Trial 4
#################################

# point estimates and confidence intervals for start by hour strategies

load("tt4_start_by_hour_results_MICU_only_with_data.Rdata")

tt4_start_by_hour_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results[,x], CI_level=0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results[,x], CI_level=0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results[,x], CI_level=0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>%
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to start intubation') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.075,0.20))
  }


# point estimates and confidence intervals for dont start until strategies

load("tt4_dont_start_until_results_MICU_only_with_data.Rdata")

tt4_dont_start_until_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results2[,x], CI_level=0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results2[,x], CI_level=0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results2[,x], CI_level=0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to delay intubation') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.075,0.20))
  }

# combine graphs for target trial 4

target_trial_4 <- 
  ggarrange(tt4_start_by_hour_graph, tt4_dont_start_until_graph,
            ncol=1, nrow=2, common.legend = TRUE, legend="bottom",
            font.label=list(size=20))
annotate_figure(target_trial_4, 
                left = text_grob('30-day cumulative incidence of death', 
                                 color = "black", rot = 90))

ggsave("Target_trial_4_graph_MICU_only.png", dpi=600)

############################################
# Combine trial results for main comparison
############################################

load("tt1_start_by_hour_results_MICU_only_with_data.Rdata")
load("tt1_dont_start_until_results_MICU_only_with_data.Rdata")
load("tt2_start_by_hour_results_MICU_only_with_data.Rdata")
load("tt2_dont_start_until_results_MICU_only_with_data.Rdata")
load("tt3_start_by_hour_results_MICU_only_with_data.Rdata")
load("tt3_dont_start_until_results_MICU_only_with_data.Rdata")
load("tt4_start_by_hour_results_MICU_only_with_data.Rdata")
load("tt4_dont_start_until_results_MICU_only_with_data.Rdata")

early <- 
  data.frame(
    protocol = c("Target Trial 1",
                 "Target Trial 2",
                 "Target Trial 3",
                 "Target Trial 4"),
    estimate = c(
      trim_weights_compute_risk(0.995, tt1_save_results[,1], CI_level=0.95)$outcome,
      trim_weights_compute_risk(0.995, tt2_save_results[,1], CI_level=0.95)$outcome,
      trim_weights_compute_risk(0.995, tt3_save_results[,1], CI_level=0.95)$outcome,
      trim_weights_compute_risk(0.995, tt4_save_results[,1], CI_level=0.95)$outcome
    ),
    se = c(
      trim_weights_compute_risk(0.995, tt1_save_results[,1], CI_level=0.95)$se,
      trim_weights_compute_risk(0.995, tt2_save_results[,1], CI_level=0.95)$se,
      trim_weights_compute_risk(0.995, tt3_save_results[,1], CI_level=0.95)$se,
      trim_weights_compute_risk(0.995, tt4_save_results[,1], CI_level=0.95)$se
    )
  ) %>% mutate(lower = estimate-1.96*se,
               upper = estimate+1.96*se) %>%
  mutate(non_null = case_when(sign(lower) == sign(upper) ~ TRUE,
                              TRUE ~ FALSE)) %>%
  {
    ggplot(data=., aes(y=estimate,x=protocol)) + 
      geom_point(size=2, color='brown1') + 
      geom_segment(aes(x=protocol, xend=protocol, y=lower, yend=upper), color='brown1', size=0.75) +
      xlab('') +
      ylab('') +
      ggtitle('Early intubation') +
      theme_classic() +
      theme(text = element_text(size=18), legend.position="none") +
      ylim(c(0.075,0.20))
  }

delay <- 
  data.frame(
    protocol = c("Target Trial 1",
                 "Target Trial 2",
                 "Target Trial 3",
                 "Target Trial 4"),
    estimate = c(
      trim_weights_compute_risk(0.995, tt1_save_results2[,6], CI_level=0.95)$outcome,
      trim_weights_compute_risk(0.995, tt2_save_results2[,6], CI_level=0.95)$outcome,
      trim_weights_compute_risk(0.995, tt3_save_results2[,6], CI_level=0.95)$outcome,
      trim_weights_compute_risk(0.995, tt4_save_results2[,6], CI_level=0.95)$outcome
    ),
    se = c(
      trim_weights_compute_risk(0.995, tt1_save_results2[,6], CI_level=0.95)$se,
      trim_weights_compute_risk(0.995, tt2_save_results2[,6], CI_level=0.95)$se,
      trim_weights_compute_risk(0.995, tt3_save_results2[,6], CI_level=0.95)$se,
      trim_weights_compute_risk(0.995, tt4_save_results2[,6], CI_level=0.95)$se
    )
  ) %>% mutate(lower = estimate-1.96*se,
               upper = estimate+1.96*se) %>%
  mutate(non_null = case_when(sign(lower) == sign(upper) ~ TRUE,
                              TRUE ~ FALSE)) %>%
  {
    ggplot(data=., aes(y=estimate,x=protocol)) + 
      geom_point(size=2, color='brown4') + 
      geom_segment(aes(x=protocol, xend=protocol, y=lower, yend=upper), color='brown4', size=0.75) +
      xlab('') +
      ylab('') +
      ggtitle('Delay intubation') +
      theme_classic() +
      theme(text = element_text(size=18), legend.position="none") +
      ylim(c(0.075,0.20))
  }

# combine graphs

combined_main <- 
  ggarrange(early, delay,
            ncol=1, nrow=2, common.legend = TRUE, legend="bottom",
            font.label=list(size=20))
annotate_figure(combined_main, 
                left = text_grob('30-day cumulative incidence of death', size=18, 
                                 color = "black", rot = 90))

ggsave("combined_main_MICU_only.png", dpi=600)


#################################
# Target Trial risk differences
#################################

load("tt1_start_by_hour_results_MICU_only_with_data.Rdata")
load("tt1_dont_start_until_results_MICU_only_with_data.Rdata")
load("tt2_start_by_hour_results_MICU_only_with_data.Rdata")
load("tt2_dont_start_until_results_MICU_only_with_data.Rdata")
load("tt3_start_by_hour_results_MICU_only_with_data.Rdata")
load("tt3_dont_start_until_results_MICU_only_with_data.Rdata")
load("tt4_start_by_hour_results_MICU_only_with_data.Rdata")
load("tt4_dont_start_until_results_MICU_only_with_data.Rdata")

data.frame(
  protocol = c("Target Trial 1",
               "Target Trial 2",
               "Target Trial 3",
               "Target Trial 4"),
  estimate = c(
    trim_weights_compute_risk_difference(0.995, tt1_save_results[,1], tt1_save_results2[,6], 0.95)$outcome,
    trim_weights_compute_risk_difference(0.995, tt2_save_results[,1], tt2_save_results2[,6], 0.95)$outcome,
    trim_weights_compute_risk_difference(0.995, tt3_save_results[,1], tt3_save_results2[,6], 0.95)$outcome,
    trim_weights_compute_risk_difference(0.995, tt4_save_results[,1], tt4_save_results2[,6], 0.95)$outcome
  ),
  se = c(
    trim_weights_compute_risk_difference(0.995, tt1_save_results[,1], tt1_save_results2[,6], 0.95)$se,
    trim_weights_compute_risk_difference(0.995, tt2_save_results[,1], tt2_save_results2[,6], 0.95)$se,
    trim_weights_compute_risk_difference(0.995, tt3_save_results[,1], tt3_save_results2[,6], 0.95)$se,
    trim_weights_compute_risk_difference(0.995, tt4_save_results[,1], tt4_save_results2[,6], 0.95)$se
  )
) %>% mutate(lower = estimate-1.96*se,
             upper = estimate+1.96*se) %>%
  mutate(non_null = case_when(sign(lower) == sign(upper) ~ TRUE,
                              TRUE ~ FALSE))

data.frame(
  x=c('Target trial 1', 'Target trial 2', 'Target trial 3', 'Target trial 4'),
  y=c(
    trim_weights_compute_risk_difference(0.995, tt1_save_results[,1], tt1_save_results2[,6], 0.95)$outcome,
    trim_weights_compute_risk_difference(0.995, tt2_save_results[,1], tt2_save_results2[,6], 0.95)$outcome,
    trim_weights_compute_risk_difference(0.995, tt3_save_results[,1], tt3_save_results2[,6], 0.95)$outcome,
    trim_weights_compute_risk_difference(0.995, tt4_save_results[,1], tt4_save_results2[,6], 0.95)$outcome
  ),
  y_max = c(
    trim_weights_compute_risk_difference(0.995, tt1_save_results[,1], tt1_save_results2[,6], 0.95)$outcome,
    trim_weights_compute_risk_difference(0.995, tt2_save_results[,1], tt2_save_results2[,6], 0.95)$outcome,
    trim_weights_compute_risk_difference(0.995, tt3_save_results[,1], tt3_save_results2[,6], 0.95)$outcome,
    trim_weights_compute_risk_difference(0.995, tt4_save_results[,1], tt4_save_results2[,6], 0.95)$outcome
  ) + 1.96*c(
    trim_weights_compute_risk_difference(0.995, tt1_save_results[,1], tt1_save_results2[,6], 0.95)$se,
    trim_weights_compute_risk_difference(0.995, tt2_save_results[,1], tt2_save_results2[,6], 0.95)$se,
    trim_weights_compute_risk_difference(0.995, tt3_save_results[,1], tt3_save_results2[,6], 0.95)$se,
    trim_weights_compute_risk_difference(0.995, tt4_save_results[,1], tt4_save_results2[,6], 0.95)$se
  ),
  y_min = c(
    trim_weights_compute_risk_difference(0.995, tt1_save_results[,1], tt1_save_results2[,6], 0.95)$outcome,
    trim_weights_compute_risk_difference(0.995, tt2_save_results[,1], tt2_save_results2[,6], 0.95)$outcome,
    trim_weights_compute_risk_difference(0.995, tt3_save_results[,1], tt3_save_results2[,6], 0.95)$outcome,
    trim_weights_compute_risk_difference(0.995, tt4_save_results[,1], tt4_save_results2[,6], 0.95)$outcome
  ) - 1.96*c(
    trim_weights_compute_risk_difference(0.995, tt1_save_results[,1], tt1_save_results2[,6], 0.95)$se,
    trim_weights_compute_risk_difference(0.995, tt2_save_results[,1], tt2_save_results2[,6], 0.95)$se,
    trim_weights_compute_risk_difference(0.995, tt3_save_results[,1], tt3_save_results2[,6], 0.95)$se,
    trim_weights_compute_risk_difference(0.995, tt4_save_results[,1], tt4_save_results2[,6], 0.95)$se
  )) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('') +
      ylab('30-day risk difference') +
      theme_classic() +
      theme(text = element_text(size=18)) +
      geom_hline(yintercept=0, linetype='dotted', size=1, color='violet') +
      ylim(c(-0.05,0.1))
  }

ggsave("Target_trial_risk_differences_MICU_only.png", dpi=600)

#################################
# Target Trial 1 intubation outcome
#################################

# point estimates and confidence intervals for dont start until strategies

load("tt1_dont_start_until_results_intubation_outcome_MICU_only_with_data.Rdata")

tt1_dont_start_until_intubation_outcome_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt1_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to delay intubation') +
      ylab('30-day cumulative incidence of death or intubation') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.0,0.40))
  }

ggsave("Target_trial_1_intubation_outcome_graph_MICU_only.png", dpi=600)

#################################
# Target Trial 2 intubation outcome
#################################

# point estimates and confidence intervals for dont start until strategies

load("tt2_dont_start_until_results_intubation_outcome_MICU_only_with_data.Rdata")

tt2_dont_start_until_intubation_outcome_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt2_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to delay intubation') +
      ylab('30-day cumulative incidence of death or intubation') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.0,0.40))
  }

ggsave("Target_trial_2_intubation_outcome_graph_MICU_only.png", dpi=600)

#################################
# Target Trial 3 intubation outcome
#################################

# point estimates and confidence intervals for dont start until strategies

load("tt3_dont_start_until_results_intubation_outcome_MICU_only_with_data.Rdata")

tt3_dont_start_until_intubation_outcome_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt3_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to delay intubation') +
      ylab('30-day cumulative incidence of death or intubation') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.15,0.30))
  }

tt3_dont_start_until_intubation_outcome_graph

ggsave("Target_trial_3_intubation_outcome_graph_MICU_only.png", dpi=600)

#################################
# Target Trial 4 intubation outcome
#################################

# point estimates and confidence intervals for dont start until strategies

load("tt4_dont_start_until_results_intubation_outcome_MICU_only_with_data.Rdata")

tt4_dont_start_until_intubation_outcome_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    trim_weights_compute_risk(0.995, tt4_save_results_intubation_outcome2[,x], CI_level=0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to delay intubation') +
      ylab('30-day cumulative incidence of death or intubation') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.15,0.30))
  }

tt4_dont_start_until_intubation_outcome_graph

ggsave("Target_trial_4_intubation_outcome_graph_MICU_only.png", dpi=600)
