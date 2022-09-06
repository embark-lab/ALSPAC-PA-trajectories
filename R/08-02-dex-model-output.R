library(mice)
library(broom.mixed)
library(cgwtools)

load('models/Boy_Models_DEx')


bl_0_dex_boys_pooled <- pool(bl_0_dex_boys)
bl_0_dex_boys_ORs <- summary(bl_0_dex_boys_pooled, conf.int = TRUE, exponentiate = TRUE)|> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81)

age_1_dex_boys_pooled <- pool(age_1_dex_boys)
age_1_dex_boys_ORs <- summary(age_1_dex_boys_pooled, conf.int = TRUE, exponentiate = TRUE)|> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81)

#use the D1 method to compare 
mcomp_1 <- D1(age_1_dex_boys, bl_0_dex_boys)


save(bl_0_dex_boys_pooled, file = 'models/Boy_models_DEx_Output')
resave(bl_0_dex_boys_ORs, file = 'models/Boy_models_DEx_Output')
resave(age_1_dex_boys_pooled, file = 'models/Boy_models_DEx_Output')
resave(age_1_dex_boys_ORs, file = 'models/Boy_models_DEx_Output')
resave(mcomp_1, file = 'models/Boy_models_DEx_Output')


#Now we include fixed effect covariates of BMI, parent highest occupation, and eating disorder cogitions at age 14: thin-ideal internalization, body dissatisfaction, and fear of weight gain
covs_2_dex_boys_pooled <- pool(covs_2_dex_boys)
covs_2_dex_boys_ORs <- summary(covs_2_dex_boys_pooled, conf.int = TRUE, exponentiate = TRUE)|> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81)

mcomp_2 <- D1(covs_2_dex_boys, age_1_dex_boys)

resave(covs_2_dex_boys_pooled, file = 'models/Boy_models_DEx_Output')
resave(covs_2_dex_boys_ORs, file = 'models/Boy_models_DEx_Output')
resave(mcomp_2, file = 'models/Boy_models_DEx_Output')

covs_3_dex_boys_pooled <- pool(covs_3_dex_boys)
covs_3_dex_boys_ORs <- summary(covs_3_dex_boys_pooled, conf.int = TRUE, exponentiate = TRUE)|> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81)

mcomp_3 <- D1(covs_3_dex_boys, covs_2_dex_boys)

resave(covs_3_dex_boys_pooled, file = 'models/Boy_models_DEx_Output')
resave(covs_3_dex_boys_ORs, file = 'models/Boy_models_DEx_Output')
resave(mcomp_3, file = 'models/Boy_models_DEx_Output')

covs_4_dex_boys_pooled <- pool(covs_4_dex_boys)
covs_4_dex_boys_ORs <- summary(covs_4_dex_boys_pooled, conf.int = TRUE, exponentiate = TRUE)|> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81)

mcomp_4 <- D1(covs_4_dex_boys, covs_3_dex_boys)

resave(covs_4_dex_boys_pooled, file = 'models/Boy_models_DEx_Output')
resave(covs_4_dex_boys_ORs, file = 'models/Boy_models_DEx_Output')
resave(mcomp_4, file = 'models/Boy_models_DEx_Output')

##Girls Below

rm(list = ls())
load('models/Girl_Models_DEx')

bl_0_dex_girls_pooled <- pool(bl_0_dex_girls)
bl_0_dex_girls_ORs <- summary(bl_0_dex_girls_pooled, conf.int = TRUE, exponentiate = TRUE) |>  
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81)

save(bl_0_dex_girls_pooled, file = 'models/Girl_models_DEx_Output')
resave(bl_0_dex_girls_ORs, file = 'models/Girl_models_DEx_Output')

age_1_dex_girls_pooled <- pool(age_1_dex_girls)
age_1_dex_girls_ORs <- summary(age_1_dex_girls_pooled, conf.int = TRUE, exponentiate = TRUE)  |> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81)


resave(age_1_dex_girls_pooled, file = 'models/Girl_models_DEx_Output')
resave(age_1_dex_girls_ORs, file = 'models/Girl_models_DEx_Output')

#use the D1 method to compare 
mcomp_1 <- D1(age_1_dex_girls, bl_0_dex_girls)

resave(mcomp_1, file = 'models/Girl_models_DEx_Output')


#Now we include fixed effect covariates of BMI, parent highest occupation, and eating disorder cogitions at age 14: thin-ideal internalization, body dissatisfaction, and fear of weight gain
covs_2_dex_girls_pooled <- pool(covs_2_dex_girls)
covs_2_dex_girls_ORs <- summary(covs_2_dex_girls_pooled, conf.int = TRUE, exponentiate = TRUE)|> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81)

mcomp_2 <- D1(covs_2_dex_girls, age_1_dex_girls)

resave(covs_2_dex_girls_pooled, file = 'models/Girl_models_DEx_Output')
resave(covs_2_dex_girls_ORs, file = 'models/Girl_models_DEx_Output')
resave(mcomp_2, file = 'models/Girl_models_DEx_Output')


covs_3_dex_girls_pooled <- pool(covs_3_dex_girls)
covs_3_dex_girls_ORs <- summary(covs_3_dex_girls_pooled, conf.int = TRUE, exponentiate = TRUE)|> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81)

mcomp_3 <- D1(covs_3_dex_girls, covs_2_dex_girls)


resave(covs_3_dex_girls_pooled, file = 'models/Girl_models_DEx_Output')
resave(covs_3_dex_girls_ORs, file = 'models/Girl_models_DEx_Output')
resave(mcomp_3, file = 'models/Girl_models_DEx_Output')

covs_4_dex_girls_pooled <- pool(covs_4_dex_girls)
covs_4_dex_girls_ORs <- summary(covs_4_dex_girls_pooled, conf.int = TRUE, exponentiate = TRUE)|> 
  mutate(`0.25 %` = estimate - std.error*2.81) |> 
  mutate(`99.75 %` = estimate + std.error*2.81)

mcomp_4 <- D1(covs_4_dex_girls, covs_3_dex_girls)


resave(covs_4_dex_girls_pooled, file = 'models/Girl_models_DEx_Output')
resave(covs_4_dex_girls_ORs, file = 'models/Girl_models_DEx_Output')
resave(mcomp_4, file = 'models/Girl_models_DEx_Output')

rm(list = ls())

