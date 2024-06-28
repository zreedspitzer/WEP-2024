# Purpose: To average PRISM Monthly temp & precip data to subbasin level
# Author: Zoey Reed-Spitzer
# Date Created: 1/23
# Date Modified: 6/27
# Note: This is for the added subbasins BMR. Same code used for 1-13 subbasins

# ---- Setup ----
setwd("C:/Users/zoeys/Documents/WEP-2024")

# Load packages
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
library(lubridate)
library(zoo)

# Import data
precip <- read_xlsx("Data/Weather/Intermediate/precipt_monthly_BMR_2-11.xlsx")
temp <- read_xlsx("Data/Weather/Intermediate/tempr_monthly_BMR_2-11.xlsx")

# ---- Data cleaning / setup (New SBs 113) ----
# filter out unneeded subbasins, create year variable
precip <- precip %>% 
  filter(Subbasin %in% c("AvraValley","Eloy","MaricopaStanfield","RainbowValley")) %>% 
  mutate(Year = year(allDates))
temp <- temp %>% 
  filter(Subbasin %in% c("AvraValley","Eloy","MaricopaStanfield","RainbowValley")) %>% 
  mutate(Year = year(allDates))


# ---- Data cleaning / setup (New SBs 130 & BMR 2/11) ----
# creating Year variable
precip <- precip %>% 
  mutate(Year = year(allDates)) %>% 
  rename(Subbasin = area)
temp <- temp %>% 
  mutate(Year = year(allDates)) %>% 
  rename(Subbasin = area)

# ---- Averaging to subbasin level ----
# grouping by year and subbasin then averaging up
precip_avg <- precip %>% 
  group_by(Subbasin,Year) %>% 
  summarize(avg_precip = mean(precip))
temp_avg <- temp %>% 
  group_by(Subbasin,Year) %>%
  summarize(avg_temp = mean(temp))

# ---- Exporting DFs ----
write_xlsx(precip_avg,"precip_avg_BMR_2-11.xlsx")
write_xlsx(temp_avg,"temp_avg_BMR_2-11.xlsx")
