
library(mice)
library(dplyr)
library(miceadds)
library(datawizard)
library(haven)
load('data/ed_data.RData')

#Create Imputation Dataset - Girls

glm_girls_data <- demo_ed.l.girls %>%
  mutate(ti_mean_14_std = standardise(ti_mean_14)) %>%
  mutate(body_sat_mean_14_std = standardise(body_sat_mean_14)) %>%
  mutate(ex_complete = ifelse(is.na(driven_exercise) == TRUE, NA, 1)) %>%
  mutate(age_adjust = as.numeric(age) - 14) |>
  group_by(id) %>%
  mutate(nobs = sum(!is.na(ex_complete))) %>%
  ungroup() %>%
  filter(nobs > 0) %>%
  mutate(id = as.numeric(id)) %>%
  mutate (bmi_z_bestavail_1 = if_else(is.na(bmi_z_bestavail.13) == TRUE, bmi_z_clinic_2006.12, bmi_z_bestavail.13)) %>%
  mutate (bmi_z_bestavail = if_else(is.na(bmi_z_bestavail_1) == TRUE, bmi_z_pr_2006_14, bmi_z_bestavail_1)) %>%
  select(c(id, age_adjust, driven_exercise, exercise_wtloss, bmi_z_bestavail, ti_mean_14_std, body_sat_mean_14_std, fear_wtgain_14, parent_highest_occupation, education_mum, education_partner))



#MICE data - this preps up an imputation dataset with predictor variables.
pred <- make.predictorMatrix(glm_girls_data)
pred[,'education_mum'] <- 0
pred['education_mum',] <- 0
pred['parent_highest_occupation', 'education_mum'] <-1

pred[,'education_partner'] <- 0
pred['education_partner',] <- 0
pred['parent_highest_occupation', 'education_partner'] <-1

pred[,'id'] <- -2
pred[,'age_adjust'] <- 0

meth <- list()
meth[1:2] <- ""
meth[3] <- "2l.bin"
meth[4] <- "2l.pmm"
meth[5:11] <- "2lonly.pmm"

glm_imp_girls <- mice(glm_girls_data, meth = meth, pred = pred, m = 20, maxit = 10, seed = 9845)

#check to make sure that things are working right

table(glm_girls_data$driven_exercise, useNA = "always")
table(mice::complete(glm_imp_girls)$driven_exercise, useNA = "always")

summary(glm_girls_data$bmi_z_bestavail, useNA = "always")
summary(mice::complete(glm_imp_girls)$bmi_z_bestavail, useNA = "always")

#write file
write.mice.imputation(glm_imp_girls, name = 'glm_mice_girls', include.varnames=TRUE, long=TRUE, mids2spss=FALSE, dattype='csv')

#Repeats the Process for Boys

glm_boys_data <- demo_ed.l.boys %>%
  mutate(ti_mean_14_std = standardise(ti_mean_14)) %>%
  mutate(body_sat_mean_14_std = standardise(body_sat_mean_14)) %>%
  mutate(ex_complete = ifelse(is.na(driven_exercise) == TRUE, NA, 1)) %>%
  mutate(age_adjust = as.numeric(age) - 14) |>
  group_by(id) %>%
  mutate(nobs = sum(!is.na(ex_complete))) %>%
  ungroup() %>%
  filter(nobs > 0) %>%
  mutate(id = as.numeric(id)) %>%
  mutate (bmi_z_bestavail_1 = if_else(is.na(bmi_z_bestavail.13) == TRUE, bmi_z_clinic_2006.12, bmi_z_bestavail.13)) %>%
  mutate (bmi_z_bestavail = if_else(is.na(bmi_z_bestavail_1) == TRUE, bmi_z_pr_2006_14, bmi_z_bestavail_1)) %>%
  #select only relevant variables - we choose a set of 16 variables  which include other ed cognitions at age 14, bmi at ages 12 and 14, and parent education variables to supplement parent highest occupation as axuillary predictors.
  select(c(id, age_adjust, driven_exercise, exercise_wtloss, bmi_z_bestavail, ti_mean_14_std, body_sat_mean_14_std, fear_wtgain_14, parent_highest_occupation, education_mum, education_partner))

#MICE data - this preps up an imputation dataset with predictor variables
pred <- make.predictorMatrix(glm_boys_data)
pred[,'education_mum'] <- 0
pred['education_mum',] <- 0
pred['parent_highest_occupation', 'education_mum'] <-1

pred[,'education_partner'] <- 0
pred['education_partner',] <- 0
pred['parent_highest_occupation', 'education_partner'] <-1

pred[,'id'] <- -2
pred[,'age_adjust'] <- 0

meth <- list()
meth[1:2] <- ""
meth[3] <- "2l.bin"
meth[4] <- "2l.pmm"
meth[5:11] <- "2lonly.pmm"

glm_imp_boys <- mice(glm_boys_data, meth = meth, pred = pred, m = 20, maxit = 10, seed = 9845)

#check to make sure that things are working right

table(glm_boys_data$driven_exercise, useNA = "always")
table(mice::complete(glm_imp_boys)$driven_exercise, useNA = "always")

summary(glm_boys_data$bmi_z_bestavail, useNA = "always")
summary(mice::complete(glm_imp_boys)$bmi_z_bestavail, useNA = "always")

#write file
write.mice.imputation(glm_imp_boys, name = 'glm_mice_boys', include.varnames=TRUE, long=TRUE, mids2spss=FALSE, dattype='csv')

rm(list = ls())
