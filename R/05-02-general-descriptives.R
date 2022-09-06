load('data/ed_data.RData')

demo_ed.f_girls <- demo_ed.f |> 
  filter(sex == 1)   
demo_ed.f_boys <- demo_ed.f |> 
  filter(sex == 0)  



calculate_n <- function (df, variable) {
  x <-  df |> filter(!is.na({{variable}})) |> summarise(n = n())
  return(x[[1]])
}

girl_pses <- count(demo_ed.f_girls, parent_highest_occupation) |> 
  mutate(percent = round(n/sum(n)*100, 2))

boy_pses <- count(demo_ed.f_boys,parent_highest_occupation) |> 
  mutate(percent = round(n/sum(n)*100, 2))



girl_bmi <- c(mean(demo_ed.f_girls$bmi_bestavail.13, na.rm = TRUE) |> round(2), sd(demo_ed.f_girls$bmi_bestavail.13, na.rm = TRUE) |> round(2), median(demo_ed.f_girls$bmi_bestavail.13, na.rm = TRUE) |> round(2), calculate_n(demo_ed.f_girls, bmi_bestavail.13), nrow(demo_ed.f_girls) - calculate_n(demo_ed.f_girls, bmi_bestavail.13))

girl_bmiz <- c(mean(demo_ed.f_girls$bmi_z_bestavail.13, na.rm = TRUE) |> round(2), sd(demo_ed.f_girls$bmi_z_bestavail.13, na.rm = TRUE) |> round(2), median(demo_ed.f_girls$bmi_z_bestavail.13, na.rm = TRUE) |> round(2), calculate_n(demo_ed.f_girls, bmi_bestavail.13), nrow(demo_ed.f_girls) - calculate_n(demo_ed.f_girls, bmi_z_bestavail.13))

boy_bmi <- c(mean(demo_ed.f_boys$bmi_bestavail.13, na.rm = TRUE) |> round(2), sd(demo_ed.f_boys$bmi_bestavail.13, na.rm = TRUE) |> round(2), median(demo_ed.f_boys$bmi_bestavail.13, na.rm = TRUE) |> round(2), calculate_n(demo_ed.f_boys, bmi_bestavail.13), nrow(demo_ed.f_boys) - calculate_n(demo_ed.f_boys, bmi_bestavail.13))

boy_bmiz <- c(mean(demo_ed.f_boys$bmi_z_bestavail.13, na.rm = TRUE) |> round(2), sd(demo_ed.f_boys$bmi_z_bestavail.13, na.rm = TRUE) |> round(2), median(demo_ed.f_boys$bmi_z_bestavail.13, na.rm = TRUE) |> round(2), calculate_n(demo_ed.f_boys, bmi_z_bestavail.13), nrow(demo_ed.f_boys) - calculate_n(demo_ed.f_boys, bmi_z_bestavail.13))

girl_ti <- c(mean(demo_ed.f_girls$ti_mean_14, na.rm = TRUE) |> round(2),
             sd(demo_ed.f_girls$ti_mean_14, na.rm = TRUE) |> round(2), median(demo_ed.f_girls$ti_mean_14, na.rm = TRUE) |> round(2), calculate_n(demo_ed.f_girls, ti_mean_14), nrow(demo_ed.f_girls) - calculate_n(demo_ed.f_girls, ti_mean_14))

boy_ti <- c(mean(demo_ed.f_boys$ti_mean_14, na.rm = TRUE) |> round(2),
            sd(demo_ed.f_boys$ti_mean_14, na.rm = TRUE) |> round(2), median(demo_ed.f_boys$ti_mean_14, na.rm = TRUE) |> round(2), calculate_n(demo_ed.f_boys, ti_mean_14), nrow(demo_ed.f_boys) - calculate_n(demo_ed.f_boys, ti_mean_14))

girl_fearwt <- count(demo_ed.f_girls, fear_wtgain_14) |> 
  mutate(percent = round(n/sum(n)*100, 2))

boy_fearwt <- count(demo_ed.f_boys, fear_wtgain_14) |> 
  mutate(percent = round(n/sum(n)*100, 2))


girl_bd <- c(mean(demo_ed.f_girls$body_sat_mean_14, na.rm = TRUE) |> round(2), sd(demo_ed.f_girls$body_sat_mean_14, na.rm = TRUE) |> round(2), median(demo_ed.f_girls$body_sat_mean_14, na.rm = TRUE) |> round(2), calculate_n(demo_ed.f_girls, body_sat_mean_14), nrow(demo_ed.f_girls) - calculate_n(demo_ed.f_girls, body_sat_mean_14))

boy_bd <- c(mean(demo_ed.f_boys$body_sat_mean_14, na.rm = TRUE) |> round(2),
            sd(demo_ed.f_boys$body_sat_mean_14, na.rm = TRUE) |> round(2), median(demo_ed.f_boys$body_sat_mean_14, na.rm = TRUE) |> round(2), calculate_n(demo_ed.f_boys, body_sat_mean_14), nrow(demo_ed.f_boys) - calculate_n(demo_ed.f_boys, body_sat_mean_14))

df_girls <- tibble(girl_bmi, girl_bmiz, girl_ti, girl_bd)

girl_continuous <- as_tibble(cbind(Variable = names(df_girls), t(df_girls))) |> 
  rename('Mean' = V2) |> 
  rename('SD' = V3) |> 
  rename('Median' = V4) |> 
  rename('N' = V5) |> 
  rename('Missing N' = V6)

girl_continuous[1,1] = 'BMI at Age 13 - Girls'
girl_continuous[2,1] = 'BMI Z-score at age 13 - Girls'
girl_continuous[3,1] = 'Thin-Ideal Internalization at age 14 - Girls'
girl_continuous[4,1] = 'Body Dissatisfaction at age 14 - Girls'

df_boys <- tibble(boy_bmi, boy_bmiz, boy_ti, boy_bd)

boy_continuous <- as_tibble(cbind(Variable = names(df_boys), t(df_boys))) |> 
  rename('Mean' = V2) |> 
  rename('SD' = V3) |> 
  rename('Median' = V4) |> 
  rename('N' = V5) |> 
  rename('Missing N' = V6)

boy_continuous[1,1] = 'BMI at Age 13 - Boys'
boy_continuous[2,1] = 'BMI Z-score at age 13 - Boys'
boy_continuous[3,1] = 'Thin-Ideal Internalization at age 14 - Boys'
boy_continuous[4,1] = 'Body Dissatisfaction at age 14 - Boys'

cont_vars <- rbind(girl_continuous, boy_continuous) 

fear_wt_table <- cbind(girl_fearwt, boy_fearwt) 

rownames(fear_wt_table) <- c('Not at All', 'A Little', 'A Lot', 'All the Time', 'Missing')
colnames(fear_wt_table) <- c('Response - Girls', 'N - Girls', 'Percent - Girls', 'Response - Boys', 'N- Boys', 'Percent - Boys')
fear_wt_table <- fear_wt_table |> select (-4)

pses_table <- cbind(girl_pses, boy_pses) 
rownames(pses_table) <- c('Unskilled', 'Partially Skilled', 'Skilled Manual', 'Skilled Non-Manual', 'Manegerial and Technical', 'Professional', 'Missing' )
colnames(pses_table) <- c('Response - Girls', 'N - Girls', 'Percent- Girls', 'Response - Boys', 'N- Boys' , 'Percent - Boys')
pses_table <- pses_table |> select (-4)

library(cgwtools)
# setwd("/Volumes/kschaumberg/ALSPAC/Data Cleaning and Analysis/analysis/alspac-dex-1-bookdown") - my personal working directory for the project. Just make sure you are in your project directory.

save(pses_table, file = 'data/demo_tables.Rdata')
resave(fear_wt_table, file = 'data/demo_tables.Rdata')
resave(cont_vars, file = 'data/demo_tables.Rdata')

rm(list = ls())
           