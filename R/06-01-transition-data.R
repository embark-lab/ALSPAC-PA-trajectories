load('data/ed_data.RData')
library(cgwtools)
#Do just a little bit of post processing to get datasets ready for Multi-state Transition Modeling in Girls and Boys

transition_data_girls <- demo_ed.lf.girls %>% 
  mutate(ti_mean_14_std = standardise(ti_mean_14)) %>% 
  mutate(body_sat_mean_14_std = standardise(body_sat_mean_14)) %>% 
  mutate(exercise_group = exercise_group + 1) %>% 
  mutate(age = as.numeric(age)) %>% 
  # count number of exercise observations and select only rows with available exercise data and only individuals who have completed at least two exercise assessments 
  mutate(ex_complete = ifelse(is.na(driven_exercise) == TRUE, NA, 1)) %>%
  group_by(id, ex_complete) %>% 
  mutate(nobs = n()) %>% 
  ungroup() %>%   
  filter(nobs > 1) 

levels(transition_data_girls$exercise_group) <- c('No Exercise for Weight Loss', 'Exercise for Weight Loss', 'Maladaptive Exercise')

transition_data_boys <- demo_ed.lf.boys %>% 
  mutate(ti_mean_14_std = standardise(ti_mean_14)) %>% 
  mutate(body_sat_mean_14_std = standardise(body_sat_mean_14)) %>% 
  mutate(exercise_group = exercise_group + 1) %>% 
  mutate(age = as.numeric(age)) %>% 
  # count number of exercise observations and select only rows with available exercise data and only individuals who have completed at least two exercise assessments 
  mutate(ex_complete = ifelse(is.na(driven_exercise) == TRUE, NA, 1)) %>%
  group_by(id, ex_complete) %>% 
  mutate(nobs = n()) %>% 
  ungroup() %>%   
  filter(!is.na(ex_complete) == TRUE) %>% 
  filter(nobs > 1) 

levels(transition_data_boys$exercise_group) <- c('No Exercise for Weight Loss', 'Exercise for Weight Loss', 'Maladaptive Exercise')


save(transition_data_girls, file = 'data/transition_data.RData')
resave(transition_data_boys, file = 'data/transition_data.RData')

rm(list = ls())
