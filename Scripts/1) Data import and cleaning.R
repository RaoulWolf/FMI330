## FMI330 2019

## Loading the necessary packages

library(multcomp)  # post hoc tests
library(tidyverse) # data wrangling
library(readxl)    # reading .xls(x) files
library(drc)       # dose-response modelling (drm)
library(lmtest)    # drm parameter analysis
library(sandwich)  # dynamic variance-covariance refinement

## Importing the data

data_raw <- read_xlsx(path = "Data/Diuron_DRC_Data_RevLee.xlsx", # relative path to file
                      sheet = "Summary")                         # sheet in file

data <- data_raw %>% 
  select(-SampleName, -Replicate) %>% 
  rename(Concentration = concentration, 
         Fronds_number_inhibition = FN_Normalization_PERCENT,
         Frond_size_inhibition = FS_Normalization_PERCENT,
         Photosystem_II_inhibition = PSII_Normalization_PERCENT,
         ROS_formation = `ROS_Formation_Fold increase`,
         Chlorophyll_A_inhibition = `Chlorophyll a_inhibition_PERCENT`,
         Chlorophyll_B_inhibition = `Chlorophyll b_inhibition_PERCENT`, 
         Carotenoids_inhibition = Carotenoids_inhibition_PERCENT) %>% 
  filter(!str_detect(Note, "CT")) %>% 
  select(-Note)

rm(data_raw)
