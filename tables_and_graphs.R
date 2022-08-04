source("./src/dependencies.R")
source("./src/functions.R")

#################################
# Descriptives
#################################

load("./cohort_analysis.Rdata")

intubated_data <- 
  cohort %>% 
    group_by(id) %>%
    mutate(intubated = case_when(hour > 4 & lag(started_invasive)==0 & started_invasive==1 ~ 1, 
                                 hour == 4 & started_invasive==1 ~ 1,
                                 TRUE ~ 0)) %>%
    ungroup() %>% group_by(hour) %>%
    summarize(mean(intubated)) %>% 
    mutate(intubated = cumsum(`mean(intubated)`)) 

intubated_data %>%
  {ggplot(data=., aes(y=intubated, x=hour)) + 
      geom_line(size=1) +
      geom_bar(aes(y=`mean(intubated)`, x=hour), stat='identity') +
      theme_classic() +
      scale_x_continuous(breaks=seq(4,48,4), limits=c(4,48)) +
      ylab('Proportion intubated') +
      xlab('Hour')}

ggsave("Intubated_descriptive.png", dpi=600)

noninvasive_highflow_data <- 
  cohort %>% 
  group_by(hour) %>%
  summarize(mean(noninvasive), mean(highflow)) %>%
  gather(., 'variable', 'value', -hour)

noninvasive_highflow_data %>%
  {ggplot(data=., aes(y=value, x=hour, col=variable)) + 
      geom_line(size=1) +
      #geom_line(aes(y=begin_highflow, x=hour), size=1) +
      #geom_line(aes(y=begin_noninvasive, x=hour), size=1) + 
      #geom_line(aes(y=`mean(highflow)`, x=hour), stat='identity', color='red', size=1) +
      #geom_line(aes(y=`mean(noninvasive)`, x=hour), stat='identity', color='blue', size=1) +
      theme_classic() +
      scale_x_continuous(breaks=seq(4,48,4), limits=c(4,48)) +
      ylab('Proportion receiving non-invasive respiratory support') +
      xlab('Hour') +
      scale_color_manual(values = c('red', 'blue'),
                         labels = c("High flow nasal cannula",
                                    "Non-invasive positive pressure ventilation")) +
      theme(legend.title=element_blank(),
            axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5),
            legend.position=c(.4,.15),
            legend.key.size = unit(2,"line")) + guides(linetype = guide_legend(override.aes = list(size=1.2))) +
      scale_y_continuous(breaks=c(0,0.003, 0.006, 0.009, 0.012))}

ggsave("noninvasive_descriptive.png", dpi=600)

##############################################################

load("./cohort_analysis_appropriate_baseline.Rdata")

intubated_data <- 
  cohort %>% 
  group_by(id) %>%
  mutate(intubated = case_when(hour > 4 & lag(started_invasive)==0 & started_invasive==1 ~ 1, 
                               hour == 4 & started_invasive==1 ~ 1,
                               TRUE ~ 0)) %>%
  ungroup() %>% group_by(hour) %>%
  summarize(mean(intubated)) %>% 
  mutate(intubated = cumsum(`mean(intubated)`)) 

intubated_data %>%
  {ggplot(data=., aes(y=intubated, x=hour)) + 
      geom_line(size=1) +
      geom_bar(aes(y=`mean(intubated)`, x=hour), stat='identity') +
      theme_classic() +
      scale_x_continuous(breaks=seq(4,48,4), limits=c(4,48)) +
      ylab('Proportion intubated') +
      xlab('Hour')}

ggsave("Intubated_descriptive3.png", dpi=600)

noninvasive_highflow_data <- 
  cohort %>% 
  group_by(hour) %>%
  summarize(mean(noninvasive), mean(highflow)) %>%
  gather(., 'variable', 'value', -hour)

noninvasive_highflow_data %>%
  {ggplot(data=., aes(y=value, x=hour, col=variable)) + 
      geom_line(size=1) +
      #geom_line(aes(y=begin_highflow, x=hour), size=1) +
      #geom_line(aes(y=begin_noninvasive, x=hour), size=1) + 
      #geom_line(aes(y=`mean(highflow)`, x=hour), stat='identity', color='red', size=1) +
      #geom_line(aes(y=`mean(noninvasive)`, x=hour), stat='identity', color='blue', size=1) +
      theme_classic() +
      scale_x_continuous(breaks=seq(4,48,4), limits=c(4,48)) +
      ylab('Proportion receiving non-invasive respiratory support') +
      xlab('Hour') +
      scale_color_manual(values = c('red', 'blue'),
                         labels = c("High flow nasal cannula",
                                    "Non-invasive positive pressure ventilation")) +
      theme(legend.title=element_blank(),
            axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5),
            legend.position=c(.4,.15),
            legend.key.size = unit(2,"line")) + guides(linetype = guide_legend(override.aes = list(size=1.2)))}

ggsave("noninvasive_descriptive3.png", dpi=600)  

###### combine intubated descriptive 1 and 3 ######

load("./cohort_analysis.Rdata")

intubated_data1 <- 
  cohort %>% 
  group_by(id) %>%
  mutate(intubated = case_when(hour > 4 & lag(started_invasive)==0 & started_invasive==1 ~ 1, 
                               hour == 4 & started_invasive==1 ~ 1,
                               TRUE ~ 0)) %>%
  ungroup() %>% group_by(hour) %>%
  summarize(mean(intubated)) %>% 
  mutate(intubated = cumsum(`mean(intubated)`),
         tt = 'all') 

load("./cohort_analysis_appropriate_baseline.Rdata")

intubated_data3 <- 
  cohort %>% 
  group_by(id) %>%
  mutate(intubated = case_when(hour > 4 & lag(started_invasive)==0 & started_invasive==1 ~ 1, 
                               hour == 4 & started_invasive==1 ~ 1,
                               TRUE ~ 0)) %>%
  ungroup() %>% group_by(hour) %>%
  summarize(mean(intubated)) %>% 
  mutate(intubated = cumsum(`mean(intubated)`),
         tt = 'appropriate baseline') 

bind_rows(intubated_data1, 
          intubated_data3) %>%
  {ggplot(data=., aes(y=intubated, x=hour, col=tt, fill=tt)) + 
      geom_line(size=1.25) +
      geom_bar(aes(y=`mean(intubated)`), stat='identity', col='black') +
      scale_color_manual(values=c('blue', 'red'), labels=c('target trial 3 eligible',
                                                           'target trial 1 and 2 eligible')) +
      scale_fill_manual(values=c('red', 'blue'), labels=c('target trial 3 eligible',
                                                          'target trial 1 and 2 eligible')) + 
      theme_classic() +
      scale_x_continuous(breaks=seq(4,48,4), limits=c(4,48)) +
      scale_y_continuous(limits=c(0,0.15)) +
      guides(fill=guide_legend(title=""),
             col = guide_legend(title="")) +
      ylab('Proportion intubated') +
      xlab('Hour')}

ggsave("Intubated_descriptive_combined.png", dpi=600)

#################################
# Target Trial 1
#################################

# point estimates and confidence intervals for start by hour strategies

load("tt1_start_by_hour_results.Rdata")

tt1_start_by_hour_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results_light[,x], 0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results_light[,x], 0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results_light[,x], 0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results_light[,x], 0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results_light[,x], 0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to start intubation') +
      #ylab('30-day cumulative incidence of death') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0,0.4))
  }


# point estimates and confidence intervals for dont start until strategies

load("tt1_dont_start_until_results.Rdata")

tt1_dont_start_until_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results2_light[,x], 0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results2_light[,x], 0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results2_light[,x], 0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results2_light[,x], 0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results2_light[,x], 0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to delay intubation') +
      #ylab('30-day cumulative incidence of death') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.0,0.40))
  }

# combine graphs for target trial 1

library(ggpubr)
target_trial_1 <- 
  ggarrange(tt1_start_by_hour_graph, tt1_dont_start_until_graph,
            ncol=1, nrow=2, common.legend = TRUE, legend="bottom",
            font.label=list(size=20))
annotate_figure(target_trial_1, 
                left = text_grob('30-day cumulative incidence of death', 
                                 color = "black", rot = 90))

ggsave("Target_trial_1_graph.png", dpi=600)


#################################
# Target Trial 2
#################################

# point estimates and confidence intervals for start by hour strategies

load("tt2_start_by_hour_results.Rdata")

tt2_start_by_hour_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results_light[,x], 0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results_light[,x], 0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results_light[,x], 0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results_light[,x], 0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results_light[,x], 0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to start intubation') +
      #ylab('30-day cumulative incidence of death') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0,0.40))
  }


# point estimates and confidence intervals for dont start until strategies

load("tt2_dont_start_until_results.Rdata")

tt2_dont_start_until_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results2_light[,x], 0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results2_light[,x], 0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results2_light[,x], 0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results2_light[,x], 0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results2_light[,x], 0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to delay intubation') +
      #ylab('30-day cumulative incidence of death') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.0,0.40))
  }

# combine graphs for target trial 2

library(ggpubr)
target_trial_2 <- 
  ggarrange(tt2_start_by_hour_graph, tt2_dont_start_until_graph,
            ncol=1, nrow=2, common.legend = TRUE, legend="bottom",
            font.label=list(size=20))
annotate_figure(target_trial_2, 
                left = text_grob('30-day cumulative incidence of death', 
                                 color = "black", rot = 90))

ggsave("Target_trial_2_graph.png", dpi=600)


#################################
# Target Trial 3
#################################

# point estimates and confidence intervals for start by hour strategies

load("tt3_start_by_hour_results.Rdata")

tt3_start_by_hour_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results_light[,x], 0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results_light[,x], 0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results_light[,x], 0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results_light[,x], 0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results_light[,x], 0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  mutate(y_max_l = ifelse(y_max > 0.4, 0.4, NA)) %>%
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max_l), arrow = arrow(length=unit(0.15, "cm"), ends='last', type='closed')) +
      xlab('Hour by which to start intubation') +
      #ylab('30-day cumulative incidence of death') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.0,0.40))
  }


# point estimates and confidence intervals for dont start until strategies

load("tt3_dont_start_until_results.Rdata")

tt3_dont_start_until_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results2_light[,x], 0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results2_light[,x], 0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results2_light[,x], 0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results2_light[,x], 0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results2_light[,x], 0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to delay intubation') +
      #ylab('30-day cumulative incidence of death') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.0,0.40))
  }

# combine graphs for target trial 3

library(ggpubr)
target_trial_3 <- 
  ggarrange(tt3_start_by_hour_graph, tt3_dont_start_until_graph,
            ncol=1, nrow=2, common.legend = TRUE, legend="bottom",
            font.label=list(size=20))
annotate_figure(target_trial_3, 
                left = text_grob('30-day cumulative incidence of death', 
                                 color = "black", rot = 90))

ggsave("Target_trial_3_graph.png", dpi=600)


#################################
# Target Trial 4
#################################

# point estimates and confidence intervals for start by hour strategies

load("tt4_start_by_hour_results.Rdata")

tt4_start_by_hour_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results_light[,x], 0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results_light[,x], 0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results_light[,x], 0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results_light[,x], 0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results_light[,x], 0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>%
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to start intubation') +
      #ylab('30-day cumulative incidence of death') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.0,0.40))
  }


# point estimates and confidence intervals for dont start until strategies

load("tt4_dont_start_until_results.Rdata")

tt4_dont_start_until_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results2_light[,x], 0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results2_light[,x], 0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results2_light[,x], 0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results2_light[,x], 0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results2_light[,x], 0.95)
  })[2,] %>% unlist()),
  x= seq(8,48,8)) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('Hour by which to delay intubation') +
      #ylab('30-day cumulative incidence of death') +
      ylab('') +
      theme_classic() +
      scale_x_continuous(breaks = seq(8,48,8)) +
      ylim(c(0.0,0.40))
  }

# combine graphs for target trial 4

library(ggpubr)
target_trial_4 <- 
  ggarrange(tt4_start_by_hour_graph, tt4_dont_start_until_graph,
            ncol=1, nrow=2, common.legend = TRUE, legend="bottom",
            font.label=list(size=20))
annotate_figure(target_trial_4, 
                left = text_grob('30-day cumulative incidence of death', 
                                 color = "black", rot = 90))

ggsave("Target_trial_4_graph.png", dpi=600)

#################################
# Target Trial risk differences
#################################

data.frame(
  protocol = c("Target Trial 1",
               "Target Trial 2",
               "Target Trial 3",
               "Target Trial 4"),
  estimate = c(
    compute_risk_difference(tt1_save_results_light[,1], tt1_save_results2_light[,1], 0.95)$outcome,
    compute_risk_difference(tt2_save_results_light[,1], tt2_save_results2_light[,1], 0.95)$outcome,
    compute_risk_difference(tt3_save_results_light[,1], tt3_save_results2_light[,1], 0.95)$outcome,
    compute_risk_difference(tt4_save_results_light[,1], tt4_save_results2_light[,1], 0.95)$outcome
  ),
  se = c(
    compute_risk_difference(tt1_save_results_light[,1], tt1_save_results2_light[,1], 0.95)$se,
    compute_risk_difference(tt2_save_results_light[,1], tt2_save_results2_light[,1], 0.95)$se,
    compute_risk_difference(tt3_save_results_light[,1], tt3_save_results2_light[,1], 0.95)$se,
    compute_risk_difference(tt4_save_results_light[,1], tt4_save_results2_light[,1], 0.95)$se
  )
) %>% mutate(lower = estimate-1.96*se,
             upper = estimate+1.96*se) %>%
  mutate(non_null = case_when(sign(lower) == sign(upper) ~ TRUE,
                              TRUE ~ FALSE))

data.frame(
  x=c('Target trial 1', 'Target trial 2', 'Target trial 3', 'Target trial 4'),
  y=c(
    compute_risk_difference(tt1_save_results_light[,1], tt1_save_results2_light[,1], 0.95)$outcome,
    compute_risk_difference(tt2_save_results_light[,1], tt2_save_results2_light[,1], 0.95)$outcome,
    compute_risk_difference(tt3_save_results_light[,1], tt3_save_results2_light[,1], 0.95)$outcome,
    compute_risk_difference(tt4_save_results_light[,1], tt4_save_results2_light[,1], 0.95)$outcome
  ),
  y_max = c(
    compute_risk_difference(tt1_save_results_light[,1], tt1_save_results2_light[,1], 0.95)$outcome,
    compute_risk_difference(tt2_save_results_light[,1], tt2_save_results2_light[,1], 0.95)$outcome,
    compute_risk_difference(tt3_save_results_light[,1], tt3_save_results2_light[,1], 0.95)$outcome,
    compute_risk_difference(tt4_save_results_light[,1], tt4_save_results2_light[,1], 0.95)$outcome
  ) + 1.96*c(
    compute_risk_difference(tt1_save_results_light[,1], tt1_save_results2_light[,1], 0.95)$se,
    compute_risk_difference(tt2_save_results_light[,1], tt2_save_results2_light[,1], 0.95)$se,
    compute_risk_difference(tt3_save_results_light[,1], tt3_save_results2_light[,1], 0.95)$se,
    compute_risk_difference(tt4_save_results_light[,1], tt4_save_results2_light[,1], 0.95)$se
  ),
  y_min = c(
    compute_risk_difference(tt1_save_results_light[,1], tt1_save_results2_light[,1], 0.95)$outcome,
    compute_risk_difference(tt2_save_results_light[,1], tt2_save_results2_light[,1], 0.95)$outcome,
    compute_risk_difference(tt3_save_results_light[,1], tt3_save_results2_light[,1], 0.95)$outcome,
    compute_risk_difference(tt4_save_results_light[,1], tt4_save_results2_light[,1], 0.95)$outcome
  ) - 1.96*c(
    compute_risk_difference(tt1_save_results_light[,1], tt1_save_results2_light[,1], 0.95)$se,
    compute_risk_difference(tt2_save_results_light[,1], tt2_save_results2_light[,1], 0.95)$se,
    compute_risk_difference(tt3_save_results_light[,1], tt3_save_results2_light[,1], 0.95)$se,
    compute_risk_difference(tt4_save_results_light[,1], tt4_save_results2_light[,1], 0.95)$se
  )) %>% 
  {
    ggplot(data=., aes(y=y,x=x)) + 
      geom_point() + 
      geom_segment(aes(x=x, xend=x, y=y_min, yend=y_max)) +
      xlab('') +
      ylab('30-day risk difference') +
      theme_classic() +
      ylim(c(-0.10,0.3))
  }

ggsave("Target_trial_risk_differences.png", dpi=600)

#################################
# Target Trial 1 intubation outcome
#################################

# point estimates and confidence intervals for dont start until strategies

load("tt1_dont_start_until_results_intubation_outcome.Rdata")

tt1_dont_start_until_intubation_outcome_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results_intubation_outcome2_light[,x], 0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results_intubation_outcome2_light[,x], 0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results_intubation_outcome2_light[,x], 0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results_intubation_outcome2_light[,x], 0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt1_save_results_intubation_outcome2_light[,x], 0.95)
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

ggsave("Target_trial_1_intubation_outcome_graph.png", dpi=600)

#################################
# Target Trial 2 intubation outcome
#################################

# point estimates and confidence intervals for dont start until strategies

load("tt2_dont_start_until_results_intubation_outcome.Rdata")

tt2_dont_start_until_intubation_outcome_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results_intubation_outcome2_light[,x], 0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results_intubation_outcome2_light[,x], 0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results_intubation_outcome2_light[,x], 0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results_intubation_outcome2_light[,x], 0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt2_save_results_intubation_outcome2_light[,x], 0.95)
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

ggsave("Target_trial_2_intubation_outcome_graph.png", dpi=600)

#################################
# Target Trial 3 intubation outcome
#################################

# point estimates and confidence intervals for dont start until strategies

load("tt3_dont_start_until_results_intubation_outcome.Rdata")

tt3_dont_start_until_intubation_outcome_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results_intubation_outcome2_light[,x], 0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results_intubation_outcome2_light[,x], 0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results_intubation_outcome2_light[,x], 0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results_intubation_outcome2_light[,x], 0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt3_save_results_intubation_outcome2_light[,x], 0.95)
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
      ylim(c(0.15,0.35))
  }

tt3_dont_start_until_intubation_outcome_graph

ggsave("Target_trial_3_intubation_outcome_graph.png", dpi=600)

#################################
# Target Trial 4 intubation outcome
#################################

# point estimates and confidence intervals for dont start until strategies

load("tt4_dont_start_until_results_intubation_outcome.Rdata")

tt4_dont_start_until_intubation_outcome_graph <- 
  data.frame(y=(sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results_intubation_outcome2_light[,x], 0.95)
  })[1,] %>% unlist()),
  y_max = (sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results_intubation_outcome2_light[,x], 0.95)
  })[1,] %>% unlist()) + 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results_intubation_outcome2_light[,x], 0.95)
  })[2,] %>% unlist()),
  y_min = (sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results_intubation_outcome2_light[,x], 0.95)
  })[1,] %>% unlist()) - 1.96*(sapply(1:6, function(x){
    combine_DR_splits(tt4_save_results_intubation_outcome2_light[,x], 0.95)
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
      ylim(c(0.15,0.35))
  }

tt4_dont_start_until_intubation_outcome_graph

ggsave("Target_trial_4_intubation_outcome_graph.png", dpi=600)
