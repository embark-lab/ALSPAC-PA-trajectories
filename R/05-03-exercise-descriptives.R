library(ggplot2)
library(dplyr)
library(wesanderson)
#Exercise for Weight Loss Plot Across Age

#Plot 1 - Exercise for Weight Loss Categories - '<1x/mo = No; 1-3x/mo = Yes, Sometimes; 1x/wk or more = Yes, Frequently' at age 24.

load('data/ed_data.RData')
source('R/05-01-histogram-functions.R')

png('figs/ewl_plot.png')
ewl_plot <- hist_by_age_sex(demo_ed.lf, exercise_wtloss, '')
ewl_plot
dev.off()

png('figs/age24_raw_ewl_plot.png')
age_24_raw_ewl_plot <- hist_by_sex(demo_ed.f, exercise_wtloss_raw.24, 'Frequency of Exercise For Weight Loss Reported as Raw Data - Age 24')
age_24_raw_ewl_plot
dev.off()


png('figs/ex-interfere.png')
exercise_interfere_gf_df  <- pivot_longer(demo_ed, cols = c('exercise_interfere_present.14', 'exercise_interfere_present.16', 'exercise_interfere_present.18', 'exercise_interfere_present.24'), names_to = 'age', names_prefix = 'exercise_interfere_present.', values_to = 'exercise_interfere') 

exercise_interfere_data <- frq_table_by_age_sex(exercise_interfere_gf_df, exercise_interfere)

exercise_interfere_plot_1 <- ggplot(data = exercise_interfere_data, 
                                    aes(x = sex, y = pct, fill = exercise_interfere, label = sprintf('%0.1f%%', pct*100))) + #set the aesthetic features, including the labels at one decimal place
  geom_col(position = position_stack(reverse = TRUE)) + #add column graph, with male and female separated
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1))+ #set the lower and upper limits of the y axis ( 0-100 percent); 
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), text = element_text(size = 15))+ #make legend text a bit bigger
  labs(x = element_blank(), y = 'Percent (within gender)', title = 'Exercise Interference with Work, School, or Daily Activities Across Age') +
  geom_text(aes(label = sprintf('%0.1f%%', pct*100)), position = position_stack(reverse = TRUE, vjust = 0.5)) + 
  theme_classic()+
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), text = element_text(size = 15)) + #make legend text a bit bigger 
  scale_fill_manual(values = wes_palette(name = 'GrandBudapest1', n = 2)) +
  facet_wrap(~age) 

exercise_interfere_plot_1
dev.off()