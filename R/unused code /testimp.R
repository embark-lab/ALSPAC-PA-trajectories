#Create Imputation Dataset


#start with the wide data for MI on predictors

glm_girls_data <- demo_ed.l.girls %>%
  mutate(ti_mean_14_std = standardise(ti_mean_14)) %>%
  mutate(body_sat_mean_14_std = standardise(body_sat_mean_14)) %>%
  mutate(ex_complete = ifelse(is.na(driven_exercise) == TRUE, NA, 1)) %>%
  group_by(id) %>%
  mutate(nobs = sum(!is.na(ex_complete))) %>%
  ungroup() %>%
  filter(nobs > 0) %>%
  mutate(id = as.numeric(id)) %>%
  mutate (bmi_z_bestavail_1 = if_else(is.na(bmi_z_bestavail.13) == TRUE, bmi_z_clinic_2006.12, bmi_z_bestavail.13)) %>%
  mutate (bmi_z_bestavail = if_else(is.na(bmi_z_bestavail_1) == TRUE, bmi_z_pr_2006_14, bmi_z_bestavail_1)) %>%
  #select only relevant variables - we choose a set of 16 variables  which include other ed cognitions at age 14, bmi at ages 12 and 14, and parent education variables to supplement parent highest occupation as axuillary predictors.
  select(c(id, age, driven_exercise, bmi_z_bestavail, ti_mean_14_std, body_sat_mean_14_std, fear_wtgain_14, parent_highest_occupation, education_mum))



#MICE data - this preps up an imputation dataset with predictor variables. Since we are only imputing fixed (baseline) predictors, we'll impute them in the wide-form dataset and then
pred <- make.predictorMatrix(glm_girls_data)
pred[,'id'] <- -2
pred[,'age'] <- 0
pred[,'education_mum'] <- 0
pred['education_mum',] <- 0
pred['parent_highest_occupation', 'education_mum'] <- 3



meth <- list()
meth[1:2] <- ""
meth[3] <- "2l.bin"
meth[4:8] <- '2lonly.pmm'
meth[9] <- ""


glm_imp_girls <- mice(glm_girls_data, meth = meth, pred = pred, m = 10, maxit = 20, seed = 9845)

table(glm_girls_data$driven_exercise, useNA = "always")
table(mice::complete(glm_imp_girls)$driven_exercise, useNA = "always")

summary(glm_girls_data$bmi_z_bestavail, useNA = "always")
summary(mice::complete(glm_imp_girls)$bmi_z_bestavail, useNA = "always")

summary(glm_girls_data$ti_mean_14_std, useNA = "always")
summary(mice::complete(glm_imp_girls)$ti_mean_14_std, useNA = "always")

summary(glm_girls_data$body_sat_mean_14_std, useNA = "always")
summary(mice::complete(glm_imp_girls)$body_sat_mean_14_std, useNA = "always")

table(glm_girls_data$fear_wtgain_14, useNA = "always")
table(mice::complete(glm_imp_girls)$fear_wtgain_14, useNA = "always")

table(glm_girls_data$parent_highest_occupation, useNA = "always")
table(mice::complete(glm_imp_girls)$parent_highest_occupation, useNA = "always")
