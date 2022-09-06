#Run Models and Save:

library(cgwtools)
library(broom.mixed)
library(mitml)
library(mice)
library(miceadds)
library(dplyr)
library(haven)

load('data/glm_mice_girls/glm_mice_girls.Rdata')
glm_mice_girls <- mi.res
rm(mi.res)

bl_0_dex_girls <- with(glm_mice_girls, lme4::glmer(formula = driven_exercise ~ (1|id), family = binomial(link = 'logit')), control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))

age_1_dex_girls <- with(glm_mice_girls, lme4::glmer(formula = driven_exercise ~ age_adjust + (1|id), family = binomial(link = 'logit')), control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))

covs_2_dex_girls <- with(glm_mice_girls, lme4::glmer(formula = driven_exercise ~ age_adjust + parent_highest_occupation + bmi_z_bestavail + (1|id), family = binomial(link = 'logit')), control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))

covs_3_dex_girls <- with(glm_mice_girls, lme4::glmer(formula = driven_exercise ~ age_adjust + parent_highest_occupation + bmi_z_bestavail + fear_wtgain_14 + body_sat_mean_14_std + ti_mean_14_std + (1|id), family = binomial(link = 'logit')), control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))

covs_4_dex_girls <- with(glm_mice_girls, lme4::glmer(formula = driven_exercise ~ age_adjust + parent_highest_occupation + bmi_z_bestavail + fear_wtgain_14 + body_sat_mean_14_std + ti_mean_14_std + age_adjust*bmi_z_bestavail + age_adjust*fear_wtgain_14 + age_adjust*body_sat_mean_14_std + age_adjust*ti_mean_14_std + (1|id), family = binomial(link = 'logit')), control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))


save(bl_0_dex_girls, file = 'models/Girl_Models_DEx')
resave(age_1_dex_girls, file = 'models/Girl_Models_DEx')
resave(covs_2_dex_girls, file = 'models/Girl_Models_DEx')
resave(covs_3_dex_girls, file = 'models/Girl_Models_DEx')
resave(covs_4_dex_girls, file = 'models/Girl_Models_DEx')


load('data/glm_mice_boys/glm_mice_boys.Rdata')
glm_mice_boys <- mi.res
rm(mi.res)

bl_0_dex_boys <- with(glm_mice_boys, lme4::glmer(formula = driven_exercise ~ (1|id), family = binomial(link = 'logit')), control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))

age_1_dex_boys <- with(glm_mice_boys, lme4::glmer(formula = driven_exercise ~ age_adjust + (1|id), family = binomial(link = 'logit')), control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))

covs_2_dex_boys <- with(glm_mice_boys, lme4::glmer(formula = driven_exercise ~ age_adjust + parent_highest_occupation + bmi_z_bestavail + (1|id), family = binomial(link = 'logit')), control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))

covs_3_dex_boys <- with(glm_mice_boys, lme4::glmer(formula = driven_exercise ~ age_adjust + parent_highest_occupation + bmi_z_bestavail + fear_wtgain_14 + body_sat_mean_14_std + ti_mean_14_std + (1|id), family = binomial(link = 'logit')), control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))

covs_4_dex_boys <- with(glm_mice_boys, lme4::glmer(formula = driven_exercise ~ age_adjust + parent_highest_occupation + bmi_z_bestavail + fear_wtgain_14 + body_sat_mean_14_std + ti_mean_14_std + age_adjust*bmi_z_bestavail + age_adjust*fear_wtgain_14 + age_adjust*body_sat_mean_14_std + age_adjust*ti_mean_14_std + (1|id), family = binomial(link = 'logit')), control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))


save(bl_0_dex_boys, file = 'models/Boy_Models_DEx')
resave(age_1_dex_boys, file = 'models/Boy_Models_DEx')
resave(covs_2_dex_boys, file = 'models/Boy_Models_DEx')
resave(covs_3_dex_boys, file = 'models/Boy_Models_DEx')
resave(covs_4_dex_boys, file = 'models/Boy_Models_DEx')

rm(list = ls())
