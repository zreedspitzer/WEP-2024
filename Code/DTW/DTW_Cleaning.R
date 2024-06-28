#Purpose: Cleaning Raw DTW Level Data
#Author: Zoey Reed-Spitzer
#Date Created: 2/6/2024
#Date Updated: 6/27/2024

#most recent run without winter month restriction but yes to two year interpolation

# ---- Setup ----
setwd("C:/Users/zoeys/Documents/WEP-2024")

#load packages
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
library(lubridate)
library(zoo)

#import data
df <- read_xlsx("Data/DTW/Raw/GWSI_WW_LEVELS.xlsx", sheet = "DTW_AllSBs_IndWells_asof130")

#keeping only columns of interest
df <- df %>%
  select(SITE_WELL_SITE_ID, SITE_ADWBAS_CODE_ENTRY, WLWA_MEASUREMENT_DATE,
         WLWA_DEPTH_TO_WATER)

#reformatting date column
df <- df %>% 
  mutate(MEASUREMENT_DATE = as.Date(WLWA_MEASUREMENT_DATE))
# 116294 Observations

# ---- Filtering DF (2010 to 2021) ----
#Filtering unneeded years and months in each year

# Filter the dataframe to keep only observations within the specified date range
df <- df %>%
  filter(MEASUREMENT_DATE >= "2009-01-01", MEASUREMENT_DATE <= "2022-12-31")
# 23849 Observations

# Filter the dataframe to keep only observations with specified months
#df <- df %>%
#  filter(month(MEASUREMENT_DATE) %in% c(11,12,1,2,3)) #for keeping only measurements taken Nov-Mar instead of Oct-Apr
# 19708 Observations

# Filter the dataframe to keep only observations with specified months
df <- df %>%
  filter(month(MEASUREMENT_DATE) %in% c(10,11,12,1,2,3,4)) #for keeping measurements taken Oct-Apr
# 21640 Observations

#creating new column to only indicate year
df <- df %>% 
  mutate(YEAR = year(MEASUREMENT_DATE))

#creating average DTW for each year
df_avg <- df %>% 
  group_by(SITE_WELL_SITE_ID, YEAR) %>% 
  mutate(AVE_DTW = mean(WLWA_DEPTH_TO_WATER))
# 21640 observations (should be same as df)

#getting rid of duplicate average values in the same year
#first, removing unneeded columns
df_avg <- df_avg %>% 
  select(SITE_WELL_SITE_ID,SITE_ADWBAS_CODE_ENTRY,YEAR,AVE_DTW) %>% 
  distinct(SITE_WELL_SITE_ID,AVE_DTW, .keep_all = TRUE) %>% 
  mutate(SITE_WELL_SITE_ID = format(SITE_WELL_SITE_ID, scientific = FALSE)) #converting well id to number instead of exponential
# 18991 observations


# ---- Interpolating Part 1: One Year of Missing Data ----
#splitting df_avg into one for each subbasin, then interpolating
df_dou <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY %in% c("DOU","DIN"))
df_gil <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "GIL")
df_harqhass <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY %in% c("HAR","HAS"))
df_sca <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "SCA") #scama north and south combined
df_sev <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "SEV") #sierra vista and benson combined
df_wil <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "WIL")
df_den <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "DEN")
df_elo <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "ELO")
df_avr <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "AVR")
df_sro <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "SRO")
df_agv <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "AGV")
df_ssw <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "SSW")
df_mst <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "MST")
df_wat <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "WAT")
df_ara <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "ARA")
df_but <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "BUT")
df_mmuran <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY %in% c("RAN","MMU"))
df_sbv <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "SBV")
df_ssi <- df_avg %>% 
  filter(SITE_ADWBAS_CODE_ENTRY == "SSI")
# all subbasins have entries within study period except santa rosa and san simon wash

# converting IDs in exponential form to character
#df_ara[] <- lapply(df_ara, function(x) format(x, scientific = FALSE))
#df_but[] <- lapply(df_but, function(x) format(x, scientific = FALSE))
#df_mmu[] <- lapply(df_mmu, function(x) format(x, scientific = FALSE))
#df_ran[] <- lapply(df_ran, function(x) format(x, scientific = FALSE))
#df_sbv[] <- lapply(df_sbv, function(x) format(x, scientific = FALSE))
#df_ssi[] <- lapply(df_ssi, function(x) format(x, scientific = FALSE))

# Function to fill in missing years with the average of the previous and subsequent years for existing NA's
#filling in IDs as well as average dtw interpolation
#douglas
fill_missing_years <- function(df_dou) {
  df_dou %>%
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_dou <- df_dou %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))
#gila bend
fill_missing_years <- function(df_gil) {
  df_gil %>%
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_gil <- df_gil %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

#harq/hass
fill_missing_years <- function(df_harqhass) {
  df_harqhass %>%
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_harqhass <- df_harqhass %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

#scama north and south
fill_missing_years <- function(df_sca) {
  df_sca %>%
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_sca <- df_sca %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

#sierra vista and benson
fill_missing_years <- function(df_sev) {
  df_sev %>%
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_sev <- df_sev %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

#willcox
fill_missing_years <- function(df_wil) {
  df_wil %>%
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_wil <- df_wil %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

#eloy
fill_missing_years <- function(df_elo) {
  df_elo %>%
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_elo <- df_elo %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

#DEN
fill_missing_years <- function(df_den) {
  df_den %>%
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_den <- df_den %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

#AVR
fill_missing_years <- function(df_avr) {
  df_avr %>%
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_avr <- df_avr %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

#AGV
fill_missing_years <- function(df_agv) {
  df_agv %>%
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_agv <- df_agv %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

#MST
fill_missing_years <- function(df_mst) {
  df_mst %>%
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_mst <- df_mst %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

#WAT
fill_missing_years <- function(df_wat) {
  df_wat %>%
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_wat <- df_wat %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

#aravaipa valley
fill_missing_years <- function(df_ara) {
  df_ara %>%
    mutate(YEAR = as.numeric(YEAR),
           AVE_DTW = as.numeric(AVE_DTW)) %>%  # Convert YEAR column to numeric
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_ara <- df_ara %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

#butler valley
fill_missing_years <- function(df_but) {
  df_but %>%
    mutate(YEAR = as.numeric(YEAR),
           AVE_DTW = as.numeric(AVE_DTW)) %>%  # Convert YEAR column to numeric
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_but <- df_but %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

# mcmullen valley and ranegras plain
fill_missing_years <- function(df_mmuran) {
  df_mmuran %>%
    mutate(YEAR = as.numeric(YEAR),
           AVE_DTW = as.numeric(AVE_DTW)) %>%  # Convert YEAR column to numeric
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_mmuran <- df_mmuran %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

# san bernadino valley
fill_missing_years <- function(df_sbv) {
  df_sbv %>%
    mutate(YEAR = as.numeric(YEAR),
           AVE_DTW = as.numeric(AVE_DTW)) %>%  # Convert YEAR column to numeric
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_sbv <- df_sbv %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

# san simon valley
fill_missing_years <- function(df_ssi) {
  df_ssi %>%
    mutate(YEAR = as.numeric(YEAR),
           AVE_DTW = as.numeric(AVE_DTW)) %>%  # Convert YEAR column to numeric
    complete(YEAR = full_seq(YEAR, 1)) %>%
    arrange(YEAR) %>%
    mutate(AVE_DTW = ifelse(is.na(AVE_DTW),
                            (lag(AVE_DTW) + lead(AVE_DTW)) / 2,
                            AVE_DTW),
           SITE_WELL_SITE_ID = zoo::na.locf(SITE_WELL_SITE_ID),
           SITE_ADWBAS_CODE_ENTRY = zoo::na.locf(SITE_ADWBAS_CODE_ENTRY))
}

# Apply the function to each unique set of ID columns
df_fill_ssi <- df_ssi %>%
  group_by(SITE_WELL_SITE_ID) %>%
  do(fill_missing_years(.))

# ---- Interpolating Part 2: Two Years of Missing Data ----
# Loosening restriction from one year gap to two. 
# Function to estimate missing values based on linear interpolation
linear_interpolation <- function(x) {
  na_index <- is.na(x$AVE_DTW)
  n <- sum(na_index)
  
  if (n >= 2) {
    available_values <- x[!na_index, ]
    
    if (length(available_values$YEAR) < 2) {
      return(x)
    }
    
    for (i in 1:(length(available_values$YEAR) - 1)) {
      current_year <- available_values$YEAR[i]
      next_year <- available_values$YEAR[i + 1]
      
      if (next_year - current_year == 3) {
        x$AVE_DTW[x$YEAR > current_year & x$YEAR < next_year & is.na(x$AVE_DTW)] <- 
          available_values$AVE_DTW[i] + ((x$YEAR[x$YEAR > current_year & x$YEAR < next_year] - current_year) / 3) * (available_values$AVE_DTW[i + 1] - available_values$AVE_DTW[i])
      }
    }
  }
  
  return(x)
}

# Apply the function by group (yes! this fills in two missing years with linear trend)
df_interp_dou <- df_fill_dou %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_gil <- df_fill_gil %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_harqhass <- df_fill_harqhass %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_sca <- df_fill_sca %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_sev <- df_fill_sev %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_wil <- df_fill_wil %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_elo <- df_fill_elo %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_mst <- df_fill_mst %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_wat <- df_fill_wat %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_agv <- df_fill_agv %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_avr <- df_fill_avr %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_den <- df_fill_den %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_ara <- df_fill_ara %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_but <- df_fill_but %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_mmuran <- df_fill_mmuran %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_sbv <- df_fill_sbv %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

df_interp_ssi <- df_fill_ssi %>% 
  group_by(SITE_WELL_SITE_ID) %>%
  do(linear_interpolation(.)) %>%
  ungroup()

# ---- Cleaning DF's to only have qualifying wells ----
#eloy test
#df_elo_test <- df_fill_elo %>% 
#  group_by(SITE_WELL_SITE_ID) %>% 
#  filter(all(c(2010:2022)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
#  ungroup()
# good, this code gets rid of all wells in Eloy that have missing information between study period
# but leaves IDs with missing values in 2022. 

#douglas
df_dou_wells <- df_interp_dou %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()

#gila bend
df_gil_wells <- df_interp_gil %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()

#harqhass
df_harqhass_wells <- df_interp_harqhass %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()

#scama north and south
df_sca_wells <- df_interp_sca %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()
#not useable

#sierra vista and benson
df_sev_wells <- df_interp_sev %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()

#willcox
df_wil_wells <- df_interp_wil %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()

#eloy
df_elo_wells <- df_interp_elo %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()

#dendora
df_den_wells <- df_interp_den %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()
#not useable

#avra valley
df_avr_wells <- df_interp_avr %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()

#aguirre valley
df_agv_wells <- df_interp_agv %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()
#not useable

#maricopa stanfeild
df_mst_wells <- df_interp_mst %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()

#rainbow valley
df_wat_wells <- df_interp_wat %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()

#aravaipa valley
df_ara_wells <- df_interp_ara %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()

#butler valley
df_but_wells <- df_interp_but %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()
#not useable

#mcmullen valley and ranegras plain
df_mmuran_wells <- df_interp_mmuran %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()

#san bernadino valley
df_sbv_wells <- df_interp_sbv %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup()
#only one well here

#san simon valley
df_ssi_wells <- df_interp_ssi %>% 
  group_by(SITE_WELL_SITE_ID) %>% 
  filter(all(c(2010:2021)%in% YEAR) & !any(is.na(AVE_DTW)& YEAR >= 2010 & YEAR <= 2021)) %>% 
  ungroup() 

# ---- Pivoting DF's and combining together ----
wells_dou <- df_dou_wells %>%
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_gil <- df_gil_wells %>% 
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_harqhass <- df_harqhass_wells %>% 
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_sca <- df_sca_wells %>% 
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_sev <- df_sev_wells %>%
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_wil <- df_wil_wells %>% 
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_elo <- df_elo_wells %>%
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_avr <- df_avr_wells %>% 
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_den <- df_den_wells %>% 
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_mst <- df_mst_wells %>% 
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_wat <- df_wat_wells %>% 
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_ara <- df_ara_wells %>%
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_but <- df_but_wells %>% 
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_mmuran <- df_mmuran_wells %>% 
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )

wells_sbv <- df_sbv_wells %>% 
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )


wells_ssi <- df_ssi_wells %>% 
  pivot_wider(
    id_cols = SITE_WELL_SITE_ID,
    names_from = YEAR,
    values_from = AVE_DTW
  )


#rearranging tables to get years in right order & adding name column
wells_dou <- wells_dou %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "Douglas")

wells_gil <- wells_gil %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "GilaBend")

wells_harqhass <- wells_harqhass %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "HarqHass")

wells_sca <- wells_sca %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "SCAMA")

wells_sev <- wells_sev %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "SierraVistaBenson")

wells_wil <- wells_wil %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "Willcox")

wells_elo <- wells_elo %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "Eloy")

wells_avr <- wells_avr %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "AvraValley")

wells_mst <- wells_mst %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "MaricopaStanfield")

wells_wat <- wells_wat %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "RainbowValley")

wells_ara <- wells_ara %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "AravaipaCanyon")

wells_but <- wells_but %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "ButlerValley")

wells_mmuran <- wells_mmuran %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "McMullenRanegras")

wells_sbv <- wells_sbv %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "SanBernadinoValley")

wells_ssi <- wells_ssi %>% 
  select(SITE_WELL_SITE_ID, '2009', '2010', '2011', '2012', '2013', '2014', 
         '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022') %>% 
  mutate(Subbasin = "SanSimonValley")


#stacking the data
df_wells <- rbind(wells_ara,wells_but)
df_wells <- rbind(df_wells,wells_mmuran)
df_wells <- rbind(df_wells,wells_sbv)
df_wells <- rbind(df_wells,wells_ssi)
df_wells <- rbind(df_wells,wells_dou)
df_wells <- rbind(df_wells,wells_gil)
df_wells <- rbind(df_wells,wells_harqhass)
df_wells <- rbind(df_wells,wells_sca)
df_wells <- rbind(df_wells,wells_sev)
df_wells <- rbind(df_wells,wells_wil)
df_wells <- rbind(df_wells,wells_elo)
df_wells <- rbind(df_wells,wells_mst)
df_wells <- rbind(df_wells,wells_avr)
df_wells <- rbind(df_wells,wells_wat)

# ---- Exporting DF ----
#for study period 2010 to 2021
write_xlsx(df_wells, "Data/DTW/Intermediate/AllSB_Qualified_Ind_Wells.xlsx")

