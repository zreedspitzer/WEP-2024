## This is code to compare aggregate crop data | updated to include crop type aggregates
# Created: 11/9/23
# Updated: 2/14/24
# Purpose: WEP Revisions
# Author: Zoey Reed-Spitzer

# ---- Setup ----
setwd("C:/Users/zoeys/Documents/Research/Cropscape_Processing_StudyArea")

#loading packages
library(readxl)
library(zoo)
library(dplyr)
library(tidyr)
library(sandwich)
library(ggplot2)
library(stargazer)
library(openxlsx)

#loading data
df_2010 <- read.csv("state=AZ_year=2010_crop=all.csv")
df_2011 <- read.csv("state=AZ_year=2011_crop=all.csv")
df_2012 <- read.csv("state=AZ_year=2012_crop=all.csv")
df_2013 <- read.csv("state=AZ_year=2013_crop=all.csv")
df_2014 <- read.csv("state=AZ_year=2014_crop=all.csv")
df_2015 <- read.csv("state=AZ_year=2015_crop=all.csv")
df_2016 <- read.csv("state=AZ_year=2016_crop=all.csv")
df_2017 <- read.csv("state=AZ_year=2017_crop=all.csv")
df_2018 <- read.csv("state=AZ_year=2018_crop=all.csv")
df_2019 <- read.csv("state=AZ_year=2019_crop=all.csv")
df_2020 <- read.csv("state=AZ_year=2020_crop=all.csv")
df_2021 <- read.csv("state=AZ_year=2021_crop=all.csv")

# ---- Data Cleaning ----
#adding year variables to each individual year data frame
df_2010 <- df_2010 %>% 
  mutate(Year = 2010) %>% 
  rename(Subbasin = SUBBASIN_N) %>% 
  select(Subbasin, Year, everything())

df_2011 <- df_2011 %>% 
  mutate(Year = 2011) %>% 
  rename(Subbasin = SUBBASIN_N) %>% 
  select(Subbasin, Year, everything())

df_2012 <- df_2012 %>% 
  mutate(Year = 2012) %>% 
  rename(Subbasin = SUBBASIN_N) %>% 
  select(Subbasin, Year, everything())

df_2013 <- df_2013 %>% 
  mutate(Year = 2013) %>% 
  rename(Subbasin = SUBBASIN_N) %>% 
  select(Subbasin, Year, everything())

df_2014 <- df_2014 %>% 
  mutate(Year = 2014) %>% 
  rename(Subbasin = SUBBASIN_N) %>% 
  select(Subbasin, Year, everything())

df_2015 <- df_2015 %>% 
  mutate(Year = 2015) %>% 
  rename(Subbasin = SUBBASIN_N) %>% 
  select(Subbasin, Year, everything())

df_2016 <- df_2016 %>% 
  mutate(Year = 2016) %>% 
  rename(Subbasin = SUBBASIN_N) %>% 
  select(Subbasin, Year, everything())

df_2017 <- df_2017 %>% 
  mutate(Year = 2017) %>% 
  rename(Subbasin = SUBBASIN_N) %>% 
  select(Subbasin, Year, everything())

df_2018 <- df_2018 %>% 
  mutate(Year = 2018) %>% 
  rename(Subbasin = SUBBASIN_N) %>% 
  select(Subbasin, Year, everything())

df_2019 <- df_2019 %>% 
  mutate(Year = 2019) %>% 
  rename(Subbasin = SUBBASIN_N) %>% 
  select(Subbasin, Year, everything())

df_2020 <- df_2020 %>% 
  mutate(Year = 2020) %>% 
  rename(Subbasin = SUBBASIN_N) %>% 
  select(Subbasin, Year, everything())

df_2021 <- df_2021 %>% 
  mutate(Year = 2021) %>% 
  rename(Subbasin = SUBBASIN_N) %>% 
  select(Subbasin, Year, everything())

#merging to create one data frame
df <- bind_rows(df_2010,df_2011)
df <- bind_rows(df,df_2012)
df <- bind_rows(df,df_2013)
df <- bind_rows(df,df_2014)
df <- bind_rows(df,df_2015)
df <- bind_rows(df,df_2016)
df <- bind_rows(df,df_2017)
df <- bind_rows(df,df_2018)
df <- bind_rows(df,df_2019)
df <- bind_rows(df,df_2020)
df <- bind_rows(df,df_2021)

write.xlsx(df,"individual_crops_all_years.xlsx")

# ---- Aggregating ----
df <- df %>% 
  group_by(Subbasin,Year) %>% 
  mutate(irrigated_ag_px = sum(crop2,crop1,crop4,crop21,crop22,crop23,crop24,
                               crop236,crop237,crop36,crop37,crop68,crop71,
                               crop72,crop74,crop77,crop204,crop212,crop6,
                               crop12,crop14,crop27,crop28,crop29,crop41,crop42,
                               crop43,crop44,crop47,crop48,crop49,crop51,crop53,
                               crop54,crop57,crop67,crop69,crop205,crop206,
                               crop207,crop208,crop209,crop211,crop213,crop214,
                               crop215,crop216,crop219,crop225,crop226,crop227,
                               crop228,crop229,crop231,crop232,crop233,crop238,
                               crop243,crop244,crop245,crop246,crop249),
         developed_acres_px = sum(crop121,crop122,crop123,crop124),
         cotton_px = crop2,
         grains_px = sum(crop1,crop4,crop21,crop22,crop23,crop24,crop236,crop237),
         alfalfa_px = sum(crop36,crop37),
         trees_px = sum(crop68,crop71,crop72,crop74,crop77,crop204,crop212),
         other_crops_px = sum(crop6,crop12,crop14,crop27,crop28,crop29,crop41,
                           crop42,crop43,crop44,crop47,crop48,crop49,crop51,
                           crop53,crop54,crop57,crop67,crop69,crop205,crop206,
                           crop207,crop208,crop209,crop211,crop213,crop214,
                           crop215,crop216,crop219,crop225,crop226,crop227,
                           crop228,crop229,crop231,crop232,crop233,crop238,
                           crop243,crop244,crop245,crop246,crop249),
         fallow_px = crop61) %>% 
  select(Subbasin,Year,irrigated_ag_px, developed_acres_px, fallow_px, cotton_px, 
         grains_px, alfalfa_px, trees_px, other_crops_px, everything())

#note: only one double crop included in grain category, rest are in other
#because I they were a mix of two different categories.

# ---- Converting to Acres ----
df <- df %>% 
  group_by(Subbasin,Year) %>% 
  mutate(irrigated_ag_acres = irrigated_ag_px * 0.222394,
         developed_acres = developed_acres_px * 0.222394,
         cotton_acres = cotton_px * 0.222394,
         grains_acres = grains_px * 0.222394,
         alfalfa_acres = alfalfa_px * 0.222394,
         trees_acres = trees_px * 0.222394,
         other_crops_acres = other_crops_px * 0.222394,
         fallow_acres = fallow_px * 0.222394) %>% 
  select(Subbasin,Year,irrigated_ag_acres,developed_acres,fallow_acres,
         cotton_acres,grains_acres,alfalfa_acres,trees_acres,other_crops_acres,
         irrigated_ag_px, developed_acres_px, fallow_px, cotton_px, 
         grains_px, alfalfa_px, trees_px, other_crops_px, everything())

# ---- Creating proportion of Ag to Human Activity ----
df <- df %>% 
  group_by(Subbasin,Year) %>% 
  mutate(human_activity_acres = irrigated_ag_acres + developed_acres,
         prop_ag_to_human = irrigated_ag_acres / human_activity_acres) %>% 
  select(Subbasin,Year,human_activity_acres,prop_ag_to_human,irrigated_ag_acres,
         developed_acres,fallow_acres,cotton_acres,grains_acres,alfalfa_acres,
         trees_acres,other_crops_acres,irrigated_ag_px, developed_acres_px, 
         fallow_px, cotton_px,grains_px, alfalfa_px, trees_px, other_crops_px, 
         everything())


# ---- Making table of crop types Acres in 2021 for new SBs ----
# Taking subbasins of interest for only year 2021
df_crop21 <- df %>% 
  group_by(Subbasin) %>% 
  filter(Year == '2021') %>% 
  ungroup() %>% 
  select(Subbasin,Year,human_activity_acres,prop_ag_to_human,irrigated_ag_acres,
         developed_acres,fallow_acres,cotton_acres,grains_acres,alfalfa_acres,
         trees_acres,other_crops_acres) %>% 
  filter(Subbasin %in% c("AVRA VALLEY","BUTLER VALLEY", "ELOY", "MARICOPA-STANFIELD",
                         "MCMULLEN VALLEY", "RAINBOW VALLEY", "RANEGRAS PLAIN",
                         "SAN SIMON VALLEY"))
  
# Combining Butler Valley, MMU, and RAN
BMR_df <- df_crop21 %>%
  filter(Subbasin %in% c("BUTLER VALLEY", "MCMULLEN VALLEY", "RANEGRAS PLAIN")) %>%
  group_by(Year) %>%
  summarise(
    human_activity_acres = sum(human_activity_acres, na.rm = TRUE),
    prop_ag_to_human = sum(prop_ag_to_human, na.rm = TRUE),
    irrigated_ag_acres = sum(irrigated_ag_acres, na.rm = TRUE),
    developed_acres = sum(developed_acres, na.rm = TRUE),
    fallow_acres = sum(fallow_acres, na.rm = TRUE),
    cotton_acres = sum(cotton_acres, na.rm = TRUE),
    grains_acres = sum(grains_acres, na.rm = TRUE),
    alfalfa_acres = sum(alfalfa_acres, na.rm = TRUE),
    trees_acres = sum(trees_acres, na.rm = TRUE),
    other_crops_acres = sum(other_crops_acres, na.rm = TRUE)
  )

#Creating subbasin row name for combining later
BMR_df$Subbasin <- 'BMR'

# Bind the combined subbasin DataFrame with the original DataFrame, excluding the subbasins that were combined
df_21_croptype <- bind_rows(df_crop21, BMR_df)

# ---- Exporting data ----
write.xlsx(df,"crops_aggregated_all.xlsx")
write.xlsx(df_21_croptype,"NewSB_upto211_CropTypes2021.xlsx")
