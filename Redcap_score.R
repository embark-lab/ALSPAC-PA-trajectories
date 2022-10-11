# 0. Load packages

library(readxl)
library(haven)
library(cgwtools)
library(scorekeeper)
library(purrr)
library(tibble)
library(dplyr)


# 1. Download Data from API

# Load Raw data; name cleaned data file for output
data <- read.csv('data/BAMH_redcap_raw.csv') |> 
  filter(redcap_event_name != 'consent_arm_1')


# 1.5 Clean known data issues

# 2. Scorekeep 



# Load Individual Scoresheets
filenames <- list.files('scoresheets_clean/') # MAKE SURE THIS IS THE CORRECT DIRECTORY NAME
filenames <- paste('scoresheets_clean', filenames, sep = '/' )
ldf <- lapply(filenames, read_xlsx)

# List of Measure Names from the Scoresheets
measures <- list.files('scoresheets_clean/') 
measures <- gsub('.xlsx*', '', measures)

# Names the scoresheets
names(ldf) <- measures

# Cleans and saves cleaned data for each measure 
x <- vector(mode = 'list', length = (length(measures)))
names(x) <- measures

tibble_func_1 <- function(x) {
  y = as_tibble(x)
  return (y) }

cleaned_data <- purrr::map(x, tibble_func_1)

for (i in 1:length(measures)) {
  cleaned <- scorekeep(data, ldf[[i]])
  cleaned_last <- cleaned[[max(ldf[[i]]$step)]]
  cleaned_data[[i]] <-cleaned_last
}


# 3. Post-Scorekeep Data Processing

## 3.1. Weight Suppression Variables

## 3.2. TMFS zero center

# 4. Long Dataset


long_data <- full_join(cleaned_data[[1]], cleaned_data[[2]])

j = 3
while (j <= length(ldf)) {
  long_data <- full_join(long_data, cleaned_data[[j]])
  j = j+1
}

long_data <- long_data %>% 
  mutate(timepoint = recode(timepoint, questionnaires_arm_2 = 'BL', questionnaires_arm_3 = 'post', questionnaires_arm_4 = '8wk'))


# 5. Wide Dataset


cols <- colnames(long_data)
cols <- cols[-c(1,2)]

wide_data <- long_data %>% 
  tidyr::pivot_wider(names_from = timepoint, 
                     values_from = cols)


# 6. Save cleaned data 

# Dataset with dataframes separated by measure
BAMH_redcap <- cleaned_data 
save(BAMH_redcap, file = 'data/BAMH_redcap.RData') 

# Long Dataframe
BAMH_redcap_long <- long_data
save(BAMH_redcap_long, file = 'data/BAMH_redcap_long.RData') 

# Wide Dataframe
BAMH_redcap_wide <- wide_data
save(BAMH_redcap_wide, file = 'data/BAMH_redcap_wide.RData') 
```

# 7. Clear environment and load data for checking

rm(list = ls())
load('data/BAMH_redcap.RData')
load('data/BAMH_redcap_long.RData')
load('data/BAMH_redcap_wide.RData')

```

