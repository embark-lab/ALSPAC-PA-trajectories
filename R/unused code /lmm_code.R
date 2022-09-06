#note - must have broom.mixed installed for pooling to work

glm_bin_imp <- function(mi_obj, formula) {

imp_model <-
  mice::complete(mi_obj, 'long') |>
  mutate(age_anchor = as.numeric(age) - 14) |>
  mutate(impute = as.factor(.imp)) |>
  group_by (impute) |>
  do(model = glmer(formula = !!formula, data = ., family = binomial)) |>
  as.list()
return(imp_model)
}

clmm_imp <- function(mi_obj, formula) {

  imp_model <-
    mice::complete(mi_obj, 'long') |>
    mutate(age_anchor = as.numeric(age) - 14) |>
    mutate(impute = as.factor(.imp)) |>
    group_by (impute) |>
    do(model = clmm2(formula = !!formula, data = ., method = 'nlminb', Hess = TRUE)) |>
    as.list()
  return(imp_model)
}
