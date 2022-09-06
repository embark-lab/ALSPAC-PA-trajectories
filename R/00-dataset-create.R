
#Creates Demographic and Eating Disorder Dataset. Note - only needs to be run when Cleaned data is updated

library(dplyr)
library(cgwtools)
library(tidyr)
library(datawizard)
library(stringr)
library(haven)

#create joined dataset
load('data/ALSPAC_Cleaned.RData')
binge_comp <- full_join(binge_eating, compensatory_behaviors)
ed_behaviors <- full_join(binge_comp, driven_exercise)
ed_cognitions <- ed_cognitions %>%
  rename_with(~ str_replace(.x, pattern = '.14', replacement = '_14'))
ed_cog_beh <-full_join(ed_behaviors, ed_cognitions)
ed_cog_beh_bmi <- full_join(ed_cog_beh, bmi)
demo_ed <-full_join(ed_cog_beh_bmi, demographics)

#create dataset with at least one ED timepoint
demo_ed.f <- demo_ed %>%
  filter_at(vars(ends_with(c('.14', '.16', '.18', '.24'))), any_vars(!is.na(.))) |> 
  filter_at(vars(c(exercise_wtloss.14, exercise_wtloss.16, exercise_wtloss.18, exercise_wtloss.24)), any_vars(!is.na(.)))

#create long dataset
demo_ed.l <- demo_ed %>%
  pivot_longer(cols = ends_with(c(".14", ".16", ".18", ".24")),
               names_sep = "\\.",
               names_to = c(".value", "age")
  ) |>
  mutate(age_adjust = as.numeric(age) - 14)


#creates long version of the filtered data
demo_ed.lf <- demo_ed.f %>%
  pivot_longer(cols = ends_with(c(".14", ".16", ".18", ".24")),
               names_sep = "\\.",
               names_to = c(".value", "age")
  )

# separate boys and girls for final dataset for separated analyses

demo_ed_girls <- demo_ed %>%
  filter(sex == 1)
demo_ed_boys <- demo_ed |>
  filter (sex == 0)
demo_ed.l.girls <- demo_ed.l |>
  filter(sex == 1)
demo_ed.l.boys <- demo_ed.l |>
  filter(sex == 0)
demo_ed.lf.girls <- demo_ed.lf %>%
  filter(sex ==1 )
demo_ed.lf.boys <- demo_ed.lf %>%
  filter (sex == 0)

save(demo_ed, file = 'data/ed_data.RData')
resave(demo_ed.l, file = 'data/ed_data.RData' )
resave(demo_ed.f, file = 'data/ed_data.RData' )
resave(demo_ed.lf, file = 'data/ed_data.RData' )
resave(demo_ed.lf.boys, file = 'data/ed_data.RData' )
resave(demo_ed.lf.girls, file = 'data/ed_data.RData' )
resave(demo_ed.l.girls, file = 'data/ed_data.RData' )
resave(demo_ed.l.boys, file = 'data/ed_data.RData' )
resave(demo_ed_boys, file = 'data/ed_data.RData' )
resave(demo_ed_girls, file = 'data/ed_data.RData' )

rm(list = ls())

