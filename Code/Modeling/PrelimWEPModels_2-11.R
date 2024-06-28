# Purpose: Running preliminary models for WEP w/ added SBs: RAN, MMU, SSI
# Author: Zoey 
# Date Created: 1/15
# Date Updated: 2/15

# Updated to include scaled irrigated ag & per capita income & new weather data
# DTW variable : used .5 ratio exlusion version that allows 2 yrs missing data
# Subbasins included: WEP, 113, 130 with combined MMU, RAN, and BUT (211)

#Updated 4-8 to compare

# ---- Setup ----
setwd("C:/Users/zoeys/Documents/Thesis")

#loading packages
library(readxl)
library(plm)
library(lme4)
library(lmtest)
library(zoo)
library(dplyr)
library(tidyr)
library(sandwich)
library(ggplot2)
library(stargazer)
library(broom)
library(writexl)

#loading data
df <- read_xlsx("Df_AllSB_upto211Update.xlsx", sheet = "Sheet1")

# ---- Data Cleaning / Setup ----
# scaling irrigated agriculture by 100 acre units and per cap inc by $1000
df <- df %>% 
  mutate(scaled.Irrigated.Acres = Irrigated.Acres/100,         #scaled by 100
         scaled.Per.Capita.Income = wa_Per_Capita_Income/1000) #scaled by 1000

# creating unregulated dummy for clearer interpretation
df <- df %>% 
  mutate(Dummy_Unregulated = ifelse(Regulation == "Unregulated",1,0))

#changing temp from F to C
df<- df %>% 
  mutate(Temp.C = (Mean.Temp-32)*5/9)

# Calculate change in variables for each subbasin from the starting observation in 2010
df <- df %>%
  group_by(Subbasin) %>%
  mutate(Change_DTW = DTW - first(DTW),
         Change_Temp = Temp.C - first(Temp.C),
         Change_Precip = Precip - first(Precip),
         Change_IrrAcres = Irrigated.Acres - first(Irrigated.Acres),
         Change_Housing = avg_Housing_Units - first(avg_Housing_Units),
         Change_Income = wa_Per_Capita_Income - first(wa_Per_Capita_Income))


# ---- Fixed Effects Models ----
# One-Way FE Model, No Time Effects
fe_within <- plm(DTW ~ Mean.Temp + Precip + scaled.Irrigated.Acres 
                 + scaled.Per.Capita.Income 
                 + avg_Housing_Units + Dummy_Recharge, data = df, model = "within")
summary(fe_within)

bptest(fe_within)
fe_within_se <- coeftest(fe_within, vcov = vcovHC(fe_within, type = "HC3"))
coeftest(fe_within, vcov = vcovHC(fe_within, type = "HC3")) 

# Two-Way FE Model, No Time Effects
fe_2ways <- plm(DTW ~ Mean.Temp + Precip + scaled.Irrigated.Acres 
                 + scaled.Per.Capita.Income 
                 + avg_Housing_Units + Dummy_Recharge + factor(Year), data = df, model = "within")
summary(fe_2ways)

bptest(fe_2ways)
fe_2ways_se <- coeftest(fe_2ways, vcov = vcovHC(fe_2ways, type = "HC3"))
coeftest(fe_2ways, vcov = vcovHC(fe_2ways, type = "HC3")) 

#fe with interaction of irrigated acres and 
fe_2waysint <- plm(DTW ~ Temp.C + Precip + scaled.Irrigated.Acres 
                + scaled.Per.Capita.Income 
                + avg_Housing_Units + Dummy_Recharge + scaled.Irrigated.Acres*Dummy_Unregulated 
                + Trend_Benson + Trend_Douglas + Trend_GilaBend + Trend_HarqHass 
                + Trend_SCAMANorth + Trend_SCAMASouth + Trend_SierraVista 
                + Trend_Willcox + Trend_AvraValley + Trend_Eloy 
                + Trend_MaricopaStanfield + Trend_RaindbowValley + Trend_BMR 
                + Trend_SanSimonValley + factor(Year), data = df, model = "within")
summary(fe_2waysint)

bptest(fe_2waysint)
fe_2waysint_se <- coeftest(fe_2waysint, vcov = vcovHC(fe_2waysint, type = "HC3"))
coeftest(fe_2waysint, vcov = vcovHC(fe_2waysint, type = "HC3")) 


# ---- Pooled OLS (time effects, with and without interaction ----
#pooled OLS model, controlling for time (with interactions)
ols_time_int <- lm(DTW ~ Mean.Temp + Precip 
                   + scaled.Irrigated.Acres*Dummy_Unregulated + scaled.Per.Capita.Income 
                   + avg_Housing_Units 
                   + Dummy_Recharge + factor(Year), 
                   data = df)
summary(ols_time_int) #took out housing unit interaction

bptest(ols_time_int)
ols_time_int_se <- coeftest(ols_time_int, vcov = vcovHC(ols_time_int, type = "HC3"))
coeftest(ols_time_int, vcov = vcovHC(ols_time_int, type = "HC3"))

#pooled OLS model, controlling for time (controls for space through reg dummy)
ols_time <- lm(DTW ~ + Mean.Temp + Precip 
               + scaled.Irrigated.Acres + scaled.Per.Capita.Income 
               + avg_Housing_Units + Dummy_Unregulated 
               + Dummy_Recharge + factor(Year), 
               data = df)
summary(ols_time)

bptest(ols_time)
ols_time_se <- coeftest(ols_time, vcov = vcovHC(ols_time, type = "HC3"))
coeftest(ols_time, vcov = vcovHC(ols_time, type = "HC3"))

# 4-11 Models: No effects
ols_noef <- lm(DTW ~ scaled.Irrigated.Acres*Dummy_Unregulated + Dummy_Recharge 
               + scaled.Per.Capita.Income 
               + avg_Housing_Units 
               + Mean.Temp + Precip, 
               data = df)
summary(ols_noef)

bptest(ols_noef)
ols_noef_se <- coeftest(ols_noef, vcov = vcovHC(ols_noef, type = "HC3"))
coeftest(ols_noef, vcov = vcovHC(ols_noef, type = "HC3"))

# 4-11 Models: Year Effects
ols_year <- lm(DTW ~ scaled.Irrigated.Acres*Dummy_Unregulated + Dummy_Recharge 
               + scaled.Per.Capita.Income 
               + avg_Housing_Units 
               + Mean.Temp + Precip + factor(Year), 
               data = df)
summary(ols_year) 

bptest(ols_year)
ols_year_se <- coeftest(ols_year, vcov = vcovHC(ols_year, type = "HC3"))
coeftest(ols_year, vcov = vcovHC(ols_year, type = "HC3"))

# ---- Correlated Random Effects Model ----
# creating time trend dummy
df <- df %>% # creating dummy for each subbasin
  mutate(Dummy_AvraValley = ifelse(Subbasin == "AvraValley",1,0),
         Dummy_Eloy = ifelse(Subbasin == "Eloy",1,0),
         Dummy_MaricopaStanfield = ifelse(Subbasin == "MaricopaStanfield",1,0),
         Dummy_RainbowValley = ifelse(Subbasin == "RainbowValley",1,0),
         Dummy_BMR = ifelse(Subbasin == "BMR",1,0),
         Dummy_SanSimonValley = ifelse(Subbasin == "SanSimonValley",1,0),
         Dummy_Benson = ifelse(Subbasin == "Benson",1,0),
         Dummy_Douglas = ifelse(Subbasin == "Douglas",1,0),
         Dummy_GilaBend = ifelse(Subbasin == "GilaBend",1,0),
         Dummy_HarqHass = ifelse(Subbasin == "HarqHass",1,0),
         Dummy_SCAMANorth = ifelse(Subbasin == "SCAMANorth",1,0),
         Dummy_SCAMASouth = ifelse(Subbasin == "SCAMASouth",1,0),
         Dummy_SierraVista = ifelse(Subbasin == "SierraVista",1,0),
         Dummy_Willcox = ifelse(Subbasin == "Willcox",1,0),)%>% 
  group_by(Subbasin) %>% # creating numbers for each year and multiplying by dummies 
  mutate(Year_Counts = row_number()) %>% 
  ungroup() %>% 
  mutate(Trend_Benson = Dummy_Benson*Year_Counts, #combining columns to make trend dummies
         Trend_Douglas = Dummy_Douglas*Year_Counts,
         Trend_GilaBend = Dummy_GilaBend*Year_Counts,
         Trend_HarqHass = Dummy_HarqHass*Year_Counts,
         Trend_SCAMANorth = Dummy_SCAMANorth*Year_Counts,
         Trend_SCAMASouth = Dummy_SCAMASouth*Year_Counts,
         Trend_SierraVista = Dummy_SierraVista*Year_Counts,
         Trend_Willcox = Dummy_Willcox*Year_Counts,
         Trend_AvraValley = Dummy_AvraValley*Year_Counts,
         Trend_Eloy = Dummy_Eloy*Year_Counts,
         Trend_MaricopaStanfield = Dummy_MaricopaStanfield*Year_Counts,
         Trend_RaindbowValley = Dummy_RainbowValley*Year_Counts,
         Trend_BMR = Dummy_BMR*Year_Counts,
         Trend_SanSimonValley = Dummy_SanSimonValley*Year_Counts)

# creating unit specific time averages
df <- df %>% 
  group_by(Subbasin) %>% 
  mutate(Avg_Irrigated_Acres = mean(scaled.Irrigated.Acres),
         Avg_Per_Capita_Income = mean(scaled.Per.Capita.Income),
         Avg_Housing_Units = mean(avg_Housing_Units),
         Avg_Temp = mean(Mean.Temp),
         Avg_Precip = mean(Precip)) %>% 
  ungroup()

# CRM (Trend, USTA, Year dummies) with interaction between regulation and irrigated acres and housing units
crm_int <- lm(DTW ~ Mean.Temp + Precip
              + scaled.Irrigated.Acres*Dummy_Unregulated 
              + scaled.Per.Capita.Income 
              + avg_Housing_Units + Dummy_Recharge
              + Avg_Irrigated_Acres + Avg_Per_Capita_Income 
              + Avg_Housing_Units + Avg_Temp + Avg_Precip 
              + Trend_Benson + Trend_Douglas + Trend_GilaBend + Trend_HarqHass 
              + Trend_SCAMANorth + Trend_SCAMASouth + Trend_SierraVista 
              + Trend_Willcox + Trend_AvraValley + Trend_Eloy 
              + Trend_MaricopaStanfield + Trend_RaindbowValley + Trend_BMR 
              + Trend_SanSimonValley 
              + factor(Year), 
              data = df)
summary(crm_int)

bptest(crm_int)
crm_int_se <- coeftest(crm_int, vcov = vcovHC(crm_int, type = "HC3"))
coeftest(crm_int, vcov = vcovHC(crm_int, type = "HC3"))

# CRM without Time Trend
crm_sin_time <- lm(DTW ~ Mean.Temp + Precip
              + scaled.Irrigated.Acres*Dummy_Unregulated 
              + scaled.Per.Capita.Income 
              + avg_Housing_Units + Dummy_Recharge
              + Avg_Irrigated_Acres + Avg_Per_Capita_Income 
              + Avg_Housing_Units + Avg_Temp + Avg_Precip 
              + factor(Year), 
              data = df)
summary(crm_sin_time)

bptest(crm_sin_time)
crm_sin_time_se <- coeftest(crm_sin_time, vcov = vcovHC(crm_sin_time, type = "HC3"))
coeftest(crm_sin_time, vcov = vcovHC(crm_sin_time, type = "HC3"))

# CRM without USTA
crm_sin_usta <- lm(DTW ~ Mean.Temp + Precip
              + scaled.Irrigated.Acres*Dummy_Unregulated 
              + scaled.Per.Capita.Income 
              + avg_Housing_Units + Dummy_Recharge
              + Trend_Benson + Trend_Douglas + Trend_GilaBend + Trend_HarqHass 
              + Trend_SCAMANorth + Trend_SCAMASouth + Trend_SierraVista 
              + Trend_Willcox + Trend_AvraValley + Trend_Eloy 
              + Trend_MaricopaStanfield + Trend_RaindbowValley + Trend_BMR 
              + Trend_SanSimonValley 
              + factor(Year), 
              data = df)
summary(crm_sin_usta)

bptest(crm_sin_usta)
crm_sin_usta_se <- coeftest(crm_sin_usta, vcov = vcovHC(crm_sin_usta, type = "HC3"))
coeftest(crm_sin_usta, vcov = vcovHC(crm_sin_usta, type = "HC3"))

# CRM without year effects
crm_sin_year <- lm(DTW ~ Mean.Temp + Precip
              + scaled.Irrigated.Acres*Dummy_Unregulated 
              + scaled.Per.Capita.Income 
              + avg_Housing_Units + Dummy_Recharge
              + Avg_Irrigated_Acres + Avg_Per_Capita_Income 
              + Avg_Housing_Units + Avg_Temp + Avg_Precip 
              + Trend_Benson + Trend_Douglas + Trend_GilaBend + Trend_HarqHass 
              + Trend_SCAMANorth + Trend_SCAMASouth + Trend_SierraVista 
              + Trend_Willcox + Trend_AvraValley + Trend_Eloy 
              + Trend_MaricopaStanfield + Trend_RaindbowValley + Trend_BMR 
              + Trend_SanSimonValley, 
              data = df)
summary(crm_sin_year)

bptest(crm_sin_year)
crm_sin_year_se <- coeftest(crm_sin_year, vcov = vcovHC(crm_sin_year, type = "HC3"))
coeftest(crm_sin_year, vcov = vcovHC(crm_sin_year, type = "HC3"))

# 4-11 Models: with Year and USTA
ols_yearusta <- lm(DTW ~ scaled.Irrigated.Acres*Dummy_Unregulated + Dummy_Recharge 
                               + scaled.Per.Capita.Income 
                               + avg_Housing_Units + Mean.Temp + Precip 
                   + Avg_Irrigated_Acres + Avg_Per_Capita_Income 
                   + Avg_Housing_Units + Avg_Temp + Avg_Precip + factor(Year), 
                               data = df)
summary(ols_yearusta) 

bptest(ols_yearusta)
ols_yearusta_se <- coeftest(ols_yearusta, vcov = vcovHC(ols_yearusta, type = "HC3"))
coeftest(ols_yearusta, vcov = vcovHC(ols_yearusta, type = "HC3"))

# 4-11 Models: with Year and USTA and Time Trend
ols_yearustatrend <- lm(DTW ~ scaled.Irrigated.Acres*Dummy_Unregulated + Dummy_Recharge 
                   + scaled.Per.Capita.Income 
                   + avg_Housing_Units + Mean.Temp + Precip 
                   + Avg_Irrigated_Acres + Avg_Per_Capita_Income 
                   + Avg_Housing_Units + Avg_Temp + Avg_Precip 
                   + Trend_Benson + Trend_Douglas + Trend_GilaBend + Trend_HarqHass 
                   + Trend_SCAMANorth + Trend_SCAMASouth + Trend_SierraVista 
                   + Trend_Willcox + Trend_AvraValley + Trend_Eloy 
                   + Trend_MaricopaStanfield + Trend_RaindbowValley + Trend_BMR 
                   + Trend_SanSimonValley + factor(Year), 
                   data = df)
summary(ols_yearustatrend) 

bptest(ols_yearustatrend)
ols_yearustatrend_se <- coeftest(ols_yearustatrend, vcov = vcovHC(ols_yearustatrend, type = "HC3"))
coeftest(ols_yearustatrend, vcov = vcovHC(ols_yearustatrend, type = "HC3"))

# ---- DTW Change - Pooled OLS Model ----
#pooled OLS model, controlling for time (with interactions)
ols_time_int <- lm(Change_DTW ~ Temp.C + Precip 
                   + scaled.Irrigated.Acres*Dummy_Unregulated + scaled.Per.Capita.Income 
                   + avg_Housing_Units 
                   + Dummy_Recharge + factor(Year), 
                   data = df)
summary(ols_time_int) #took out housing unit interaction

bptest(ols_time_int)
ols_time_int_se <- coeftest(ols_time_int, vcov = vcovHC(ols_time_int, type = "HC3"))
coeftest(ols_time_int, vcov = vcovHC(ols_time_int, type = "HC3"))

#pooled OLS model, controlling for time (controls for space through reg dummy)
ols_time <- lm(Change_DTW ~ Temp.C + Precip 
               + scaled.Irrigated.Acres + scaled.Per.Capita.Income 
               + avg_Housing_Units + Dummy_Unregulated 
               + Dummy_Recharge + factor(Year), 
               data = df)
summary(ols_time)

bptest(ols_time)
ols_time_se <- coeftest(ols_time, vcov = vcovHC(ols_time, type = "HC3"))
coeftest(ols_time, vcov = vcovHC(ols_time, type = "HC3"))

# ---- DTW Change - CRM Model ----

# CRM (Trend, USTA, Year dummies) with interaction between regulation and irrigated acres and housing units
#taking out USTA because change DTW controls for diff over space (but constant over time)
crm_int <- lm(Change_DTW ~ Temp.C + Precip 
              + Change_IrrAcres*Dummy_Unregulated 
              + Change_Income*Dummy_Unregulated 
              + Change_Housing*Dummy_Unregulated + Dummy_Recharge +
              + Trend_Benson + Trend_Douglas + Trend_GilaBend + Trend_HarqHass 
              + Trend_SCAMANorth + Trend_SCAMASouth + Trend_SierraVista 
              + Trend_Willcox + Trend_AvraValley + Trend_Eloy 
              + Trend_MaricopaStanfield + Trend_RaindbowValley + Trend_BMR 
              + Trend_SanSimonValley 
              + factor(Year), 
              data = df)
summary(crm_int)

bptest(crm_int)
crm_int_se <- coeftest(crm_int, vcov = vcovHC(crm_int, type = "HC3"))
coeftest(crm_int, vcov = vcovHC(crm_int, type = "HC3"))

# CRM (Trend, USTA, Year dummies) 
crm <- lm(Change_DTW ~ Temp.C + Precip 
              + Change_IrrAcres 
              + Change_Income 
              + Change_Housing + Dummy_Recharge + Dummy_Unregulated +
              + Trend_Benson + Trend_Douglas + Trend_GilaBend + Trend_HarqHass 
              + Trend_SCAMANorth + Trend_SCAMASouth + Trend_SierraVista 
              + Trend_Willcox + Trend_AvraValley + Trend_Eloy 
              + Trend_MaricopaStanfield + Trend_RaindbowValley + Trend_BMR 
              + Trend_SanSimonValley 
              + factor(Year), 
              data = df)
summary(crm)

bptest(crm)
crm_se <- coeftest(crm, vcov = vcovHC(crm, type = "HC3"))
coeftest(crm, vcov = vcovHC(crm, type = "HC3"))
# ---- Visualizing Models ----
#tidy_fe_within <- tidy(fe_within_se)
#write_xlsx(tidy_fe_within, path = "fe_within_2-11.xlsx")

#tidy_fe_2ways <- tidy(fe_2ways)
#write_xlsx(tidy_fe_2ways, path = "fe_2ways_2-11.xlsx")

#tidy_ols_time <- tidy(ols_time_se)
#write_xlsx(tidy_ols_time, path = "ols_time_2-11.xlsx")

#tidy_ols_time_int <- tidy(ols_time_int_se)
#write_xlsx(tidy_ols_time_int, path = "ols_time_int_2-11.xlsx")

#tidy_crm_int <- tidy(crm_int_se)
#write_xlsx(tidy_crm_int, path = "crm_int_2-11.xlsx")

#tidy_crm_sin_time <- tidy(crm_sin_time_se)
#write_xlsx(tidy_crm_sin_time, path = "crm_sin_time_4-8.xlsx")

#tidy_crm_sin_usta <- tidy(crm_sin_usta_se)
#write_xlsx(tidy_crm_sin_usta, path = "crm_sin_usta_4-8.xlsx")

#tidy_crm_sin_year <- tidy(crm_sin_year_se)
#write_xlsx(tidy_crm_sin_year, path = "crm_sin_year_4-8.xlsx")

tidy_ols_noef <- tidy(ols_noef_se)
write_xlsx(tidy_ols_noef, path = "ols_noef_4-11.xlsx")

tidy_ols_year <- tidy(ols_year_se)
write_xlsx(tidy_ols_year, path = "ols_year_4-11.xlsx")

tidy_ols_yearusta <- tidy(ols_yearusta_se)
write_xlsx(tidy_ols_yearusta, path = "ols_yearusta_4-11.xlsx")

tidy_ols_yearustatrend <- tidy(ols_yearustatrend_se)
write_xlsx(tidy_ols_yearustatrend, path = "ols_yearustatrend_4-11.xlsx")
