
load('data/transition_data.RData')
library(cgwtools)
library(msm)

#Q-matrix not allowing instantaneous transitions to and from 'driven exercise' and 'no exercise for weight loss'
qmat_1 <- rbind (c(0, 1, 0), 
                 c(1, 0, 1),
                 c(0, 1, 0))

#Q-matrix allowing instantaneous transition from 'driven exercise' to 'no exercise for weight loss', but not from 'exercise for weight loss' to 'driven exercise'
qmat_2 <- rbind (c(0, 1, 0), 
                 c(1, 0, 1),
                 c(1, 1, 0))

#Q-matrix allowing all direct transitions
qmat_3 <- rbind (c(0, 1, 1), 
                 c(1, 0, 1),
                 c(1, 1, 0))

rownames(qmat_1) <- colnames(qmat_1) <- c('No EWL', 'EWL', 'Maladaptive Ex')
rownames(qmat_2) <- colnames(qmat_2) <- c('No EWL', 'EWL', 'Maladaptive Ex')
rownames(qmat_3) <- colnames(qmat_3) <- c('No EWL', 'EWL', 'Maladaptive Ex')


ex_group_girls_1 <- msm(exercise_group ~ age, subject = id, data = transition_data_girls, qmatrix = qmat_1, gen.inits = TRUE, control = list(fnscale = 5000, maxit = 10000))
ex_group_girls_2 <- msm(exercise_group ~ age, subject = id, data = transition_data_girls, qmatrix = qmat_2, gen.inits = TRUE, control = list(fnscale = 5000, maxit = 10000))
ex_group_girls_3 <- msm(exercise_group ~ age, subject = id, data = transition_data_girls, qmatrix = qmat_3, gen.inits = TRUE, control = list(fnscale = 5000, maxit = 10000))

ex_group_girls_covs_1 <- msm(exercise_group ~ age, subject = id, data = transition_data_girls, qmatrix = qmat_1, gen.inits = TRUE, control = list(fnscale =5000, maxit = 100000, reltol = 1e-16), 
                             covariates = ~ ti_mean_14_std + body_sat_mean_14_std + fear_wtgain_14 + bmi_z_bestavail.13 + parent_highest_occupation)

save(ex_group_girls_1, file = 'models/transition_models')
resave(ex_group_girls_2, file = 'models/transition_models')
resave(ex_group_girls_3, file = 'models/transition_models')

resave(ex_group_girls_covs_1, file = 'models/transition_models')


ex_group_boys_1 <- msm(exercise_group ~ age, subject = id, data = transition_data_boys, qmatrix = qmat_1, gen.inits = TRUE, control = list(fnscale = 5000, maxit = 10000))
ex_group_boys_2 <- msm(exercise_group ~ age, subject = id, data = transition_data_boys, qmatrix = qmat_2, gen.inits = TRUE, control = list(fnscale = 5000, maxit = 10000))
ex_group_boys_3 <- msm(exercise_group ~ age, subject = id, data = transition_data_boys, qmatrix = qmat_3, gen.inits = TRUE, control = list(fnscale = 5000, maxit = 10000))

#Markov model with covariates
ex_group_boys_covs_1 <- msm(exercise_group ~ age, subject = id, data = transition_data_boys, qmatrix = qmat_1, gen.inits = TRUE, control = list(fnscale =5000, maxit = 1000000, reltol = 1e-16), covariates = ~ ti_mean_14_std + bmi_z_bestavail.13 + fear_wtgain_14)


resave(ex_group_boys_1, file = 'models/transition_models')
resave(ex_group_boys_2, file = 'models/transition_models')
resave(ex_group_boys_3, file = 'models/transition_models')

resave(ex_group_boys_covs_1, file = 'models/transition_models')


rm(list = ls())
