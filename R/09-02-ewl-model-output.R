library(dplyr)
library(cgwtools)

load('models/Girl_Models_EWL')

age_1_EWL_girls_pooled <- pool(age_1_ewl_girls)
age_1_EWL_girls_ORs <- summary(age_1_EWL_girls_pooled, conf.int = TRUE, exponentiate = FALSE) |> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81) |> 
  mutate (OR = exp(estimate)) |> 
  mutate (`OR LCI` = exp(`0.25 %`)) |> 
  mutate (`OR HCI` = exp(`99.75 %`))

covs_2_EWL_girls_pooled <- pool(covs_2_ewl_girls)
covs_2_EWL_girls_ORs <- summary(covs_2_EWL_girls_pooled, conf.int = TRUE, exponentiate = FALSE) |> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81) |> 
  mutate (OR = exp(estimate)) |> 
  mutate (`OR LCI` = exp(`0.25 %`)) |> 
  mutate (`OR HCI` = exp(`99.75 %`))

covs_3_EWL_girls_pooled <- pool(covs_3_ewl_girls)
covs_3_EWL_girls_ORs <- summary(covs_3_EWL_girls_pooled, conf.int = TRUE, exponentiate = FALSE) |> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81) |> 
  mutate (OR = exp(estimate)) |> 
  mutate (`OR LCI` = exp(`0.25 %`)) |> 
  mutate (`OR HCI` = exp(`99.75 %`))

covs_4_EWL_girls_pooled <- pool(covs_4_ewl_girls)
covs_4_EWL_girls_ORs <- summary(covs_4_EWL_girls_pooled, conf.int = TRUE, exponentiate = FALSE) |> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81) |> 
  mutate (OR = exp(estimate)) |> 
  mutate (`OR LCI` = exp(`0.25 %`)) |> 
  mutate (`OR HCI` = exp(`99.75 %`))

save(age_1_EWL_girls_pooled, file = 'models/Girl_Models_EWL_Output')
resave(age_1_EWL_girls_ORs, file = 'models/Girl_Models_EWL_Output')

resave(covs_2_EWL_girls_pooled, file = 'models/Girl_Models_EWL_Output')
resave(covs_2_EWL_girls_ORs, file = 'models/Girl_Models_EWL_Output')

resave(covs_3_EWL_girls_pooled, file = 'models/Girl_Models_EWL_Output')
resave(covs_3_EWL_girls_ORs, file = 'models/Girl_Models_EWL_Output')

resave(covs_4_EWL_girls_pooled, file = 'models/Girl_Models_EWL_Output')
resave(covs_4_EWL_girls_ORs, file = 'models/Girl_Models_EWL_Output')


load('models/Boy_Models_EWL')

age_1_EWL_boys_pooled <- pool(age_1_ewl_boys)
age_1_EWL_boys_ORs <- summary(age_1_EWL_boys_pooled, conf.int = TRUE, exponentiate = FALSE) |> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81) |> 
  mutate (OR = exp(estimate)) |> 
  mutate (`OR LCI` = exp(`0.25 %`)) |> 
  mutate (`OR HCI` = exp(`99.75 %`))

covs_2_EWL_boys_pooled <- pool(covs_2_ewl_boys)
covs_2_EWL_boys_ORs <- summary(covs_2_EWL_boys_pooled, conf.int = TRUE, exponentiate = FALSE) |> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81) |> 
  mutate (OR = exp(estimate)) |> 
  mutate (`OR LCI` = exp(`0.25 %`)) |> 
  mutate (`OR HCI` = exp(`99.75 %`))

covs_3_EWL_boys_pooled <- pool(covs_3_ewl_boys)
covs_3_EWL_boys_ORs <- summary(covs_3_EWL_boys_pooled, conf.int = TRUE, exponentiate = FALSE) |> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81) |> 
  mutate (OR = exp(estimate)) |> 
  mutate (`OR LCI` = exp(`0.25 %`)) |> 
  mutate (`OR HCI` = exp(`99.75 %`))

covs_4_EWL_boys_pooled <- pool(covs_4_ewl_boys)
covs_4_EWL_boys_ORs <- summary(covs_4_EWL_boys_pooled, conf.int = TRUE, exponentiate = FALSE) |> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81) |> 
  mutate (OR = exp(estimate)) |> 
  mutate (`OR LCI` = exp(`0.25 %`)) |> 
  mutate (`OR HCI` = exp(`99.75 %`))

save(age_1_EWL_boys_pooled, file = 'models/Boy_models_EWL_Output')
resave(age_1_EWL_boys_ORs, file = 'models/Boy_models_EWL_Output')

resave(covs_2_EWL_boys_pooled, file = 'models/Boy_models_EWL_Output')
resave(covs_2_EWL_boys_ORs, file = 'models/Boy_models_EWL_Output')

resave(covs_3_EWL_boys_pooled, file = 'models/Boy_models_EWL_Output')
resave(covs_3_EWL_boys_ORs, file = 'models/Boy_models_EWL_Output')

resave(covs_4_EWL_boys_pooled, file = 'models/Boy_models_EWL_Output')
resave(covs_4_EWL_boys_ORs, file = 'models/Boy_models_EWL_Output')

rm(list = ls())
