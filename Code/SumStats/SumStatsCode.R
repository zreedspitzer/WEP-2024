## Creating Official Summary Stats for variables used in model
# Created: 11/30/23
# Updated: 2/14/24
# Purpose: WEP Revisions
# Author: Zoey Reed-Spitzer :)

# ---- Setup ----
setwd("C:/Users/zoeys/Documents/Thesis")

#loading packages
library(readxl)
library(zoo)
library(dplyr)
library(tidyr)
library(sandwich)
library(writexl)

#loading data
df <- read_xlsx("Df_AllSB_upto211Update.xlsx", sheet = "Sheet1")

#changing temp from F to C
df<- df %>% 
  mutate(Temp.C = (Mean.Temp-32)*5/9)

# ---- Summary Stats for Each Subbasin over study period ----
sumstats <- df %>%
  group_by(Subbasin) %>%
  summarise(
    Mean_DTW = mean(DTW),
    Median_DTW = median(DTW),
    Min_DTW = min(DTW),
    Max_DTW = max(DTW),
    SD_DTW = sd(DTW),
    
    Mean_Temp.C = mean(Temp.C),
    Median_Temp.C = median(Temp.C),
    Min_Temp.C = min(Temp.C),
    Max_Temp.C = max(Temp.C),
    SD_Temp.C = sd(Temp.C),
    
    Mean_Precip = mean(Precip),
    Median_Precip = median(Precip),
    Min_Precip = min(Precip),
    Max_Precip = max(Precip),
    SD_Precip = sd(Precip),
    
    Mean_Irrigated.Acres = mean(Irrigated.Acres),
    Median_Irrigated.Acres = median(Irrigated.Acres),
    Min_Irrigated.Acres = min(Irrigated.Acres),
    Max_Irrigated.Acres = max(Irrigated.Acres),
    SD_Irrigated.Acres = sd(Irrigated.Acres),
    
    Mean_wa_Per_Capita_Income = mean(wa_Per_Capita_Income),
    Median_wa_Per_Capita_Income = median(wa_Per_Capita_Income),
    Min_wa_Per_Capita_Income = min(wa_Per_Capita_Income),
    Max_wa_Per_Capita_Income = max(wa_Per_Capita_Income),
    SD_wa_Per_Capita_Income = sd(wa_Per_Capita_Income),
    
    Mean_avg_Housing_Units = mean(avg_Housing_Units),
    Median_avg_Housing_Units = median(avg_Housing_Units),
    Min_avg_Housing_Units = min(avg_Housing_Units),
    Max_avg_Housing_Units = max(avg_Housing_Units),
    SD_avg_Housing_Units = sd(avg_Housing_Units)
  )

# Code for old setup
#long_format <- sumstats %>%
#  pivot_longer(
#    cols = -Subbasin,
#    names_to = c("Variable", ".value"),
#    names_pattern = "([A-Za-z]+)_(.+)"
#  )

# Organizing columns by each variable in order I want (min,mean,med,max,sd)
sumstats <- sumstats %>% 
  select(Subbasin,Min_DTW,Mean_DTW,Median_DTW,Max_DTW,SD_DTW,
         Min_Temp.C, Mean_Temp.C,Median_Temp.C,Max_Temp.C,SD_Temp.C,
         Min_Precip,Mean_Precip,Median_Precip,Max_Precip,SD_Precip,
         Min_Irrigated.Acres,Mean_Irrigated.Acres,Median_Irrigated.Acres,Max_Irrigated.Acres,SD_Irrigated.Acres,
         Min_wa_Per_Capita_Income,Mean_wa_Per_Capita_Income,Median_wa_Per_Capita_Income,Max_wa_Per_Capita_Income,SD_wa_Per_Capita_Income,
         Min_avg_Housing_Units,Mean_avg_Housing_Units,Median_avg_Housing_Units,Max_avg_Housing_Units,SD_avg_Housing_Units)


# ---- Exporting sum stats to excel ----
#write_xlsx(long_format, "Sum_Stats_11-30.xlsx")
write_xlsx(sumstats, "Sum_Stats_2-14.xlsx")
