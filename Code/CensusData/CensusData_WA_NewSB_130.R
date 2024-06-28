# Purpose: Calculating Census data Weighted Average for New SB's as of 1/30
# Created: 2/4/24
# Updated: 6/27/24
# Purpose: WEP Revisions
# Author: Zoey Reed-Spitzer

# ---- Setup ---- 
setwd("C:/Users/zoeys/Documents/Thesis/Census_Data")

#loading packages
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(plm)
library(lme4)
library(lmtest)
library(stargazer)
library(sandwich)
library(writexl)

#importing data
cens_2010 <- read_xlsx("CensusData_NewSB_130.xlsx", 
                       sheet = "2010")
cens_2011 <- read_xlsx("CensusData_NewSB_130.xlsx", 
                       sheet = "2011")
cens_2012 <- read_xlsx("CensusData_NewSB_130.xlsx", 
                       sheet = "2012")
cens_2013 <- read_xlsx("CensusData_NewSB_130.xlsx", 
                       sheet = "2013")
cens_2014 <- read_xlsx("CensusData_NewSB_130.xlsx", 
                       sheet = "2014")
cens_2015 <- read_xlsx("CensusData_NewSB_130.xlsx", 
                       sheet = "2015")
cens_2016 <- read_xlsx("CensusData_NewSB_130.xlsx", 
                       sheet = "2016")
cens_2017 <- read_xlsx("CensusData_NewSB_130.xlsx", 
                       sheet = "2017")
cens_2018 <- read_xlsx("CensusData_NewSB_130.xlsx", 
                       sheet = "2018")
cens_2019 <- read_xlsx("CensusData_NewSB_130.xlsx", 
                       sheet = "2019")
cens_2020 <- read_xlsx("CensusData_NewSB_130.xlsx", 
                       sheet = "2020")
cens_2021 <- read_xlsx("CensusData_NewSB_130.xlsx", 
                       sheet = "2021")

#changing variable format for census df 2020 and 2021
#cens_2020$Median.Income.HH <- 
#  as.numeric(cens_2020$Median.Income.HH)
#cens_2021$Median.Income.HH <- 
#  as.numeric(cens_2021$Median.Income.HH)


#changing NA's to 0
#cens_2020$Median.Income.HH <- ifelse(is.na(cens_2020$Median.Income.HH),
#                                     0, cens_2020$Median.Income.HH)
#cens_2021$Median.Income.HH <- ifelse(is.na(cens_2021$Median.Income.HH),
#                                     0, cens_2021$Median.Income.HH)



#fixing willcox in census df's

#merging with census dfs
cens_2010 <- merge(cens_2010,pci_2010,by=c("GeoID","Subbasin"))
cens_2011 <- merge(cens_2011,pci_2011,by=c("GeoID","Subbasin"))
cens_2012 <- merge(cens_2012,pci_2012,by=c("GeoID","Subbasin"))
cens_2013 <- merge(cens_2013,pci_2013,by=c("GeoID","Subbasin"))
cens_2014 <- merge(cens_2014,pci_2014,by=c("GeoID","Subbasin"))
cens_2015 <- merge(cens_2015,pci_2015,by=c("GeoID","Subbasin"))
cens_2016 <- merge(cens_2016,pci_2016,by=c("GeoID","Subbasin"))
cens_2017 <- merge(cens_2017,pci_2017,by=c("GeoID","Subbasin"))
cens_2018 <- merge(cens_2018,pci_2018,by=c("GeoID","Subbasin"))
cens_2019 <- merge(cens_2019,pci_2019,by=c("GeoID","Subbasin"))
cens_2020 <- merge(cens_2020,pci_2020,by=c("GeoID","Subbasin"))
cens_2021 <- merge(cens_2021,pci_2021,by=c("GeoID","Subbasin"))


# ---- Creating Weighted Averages ----
# Calculate the weighted average for each group and create a new data frame
wa_2010 <- cens_2010 %>%
  group_by(Subbasin) %>%
  summarise(wa_Per_Capita_Income = weighted.mean(Per.Capita.Income.HH, 
                                                 w = Population),
            avg_Housing_Units = mean(Housing.Units))


wa_2011 <- cens_2011 %>%
  group_by(Subbasin) %>%
  summarise(wa_Per_Capita_Income = weighted.mean(Per.Capita.Income.HH, 
                                                 w = Population),
            avg_Housing_Units = mean(Housing.Units),
  )

wa_2012 <- cens_2012 %>%
  group_by(Subbasin) %>%
  summarise(wa_Per_Capita_Income = weighted.mean(Per.Capita.Income.HH, 
                                                 w = Population),
            avg_Housing_Units = mean(Housing.Units),
  )

wa_2013 <- cens_2013 %>%
  group_by(Subbasin) %>%
  summarise(wa_Per_Capita_Income = weighted.mean(Per.Capita.Income.HH, 
                                                 w = Population),
            avg_Housing_Units = mean(Housing.Units),
  )

wa_2014 <- cens_2014 %>%
  group_by(Subbasin) %>%
  summarise(wa_Per_Capita_Income = weighted.mean(Per.Capita.Income.HH, 
                                                 w = Population),
            avg_Housing_Units = mean(Housing.Units),
  )

wa_2015 <- cens_2015 %>%
  group_by(Subbasin) %>%
  summarise(wa_Per_Capita_Income = weighted.mean(Per.Capita.Income.HH, 
                                                 w = Population),
            avg_Housing_Units = mean(Housing.Units),
  )

wa_2016 <- cens_2016 %>%
  group_by(Subbasin) %>%
  summarise(wa_Per_Capita_Income = weighted.mean(Per.Capita.Income.HH, 
                                                 w = Population),
            avg_Housing_Units = mean(Housing.Units),
  )

wa_2017 <- cens_2017 %>%
  group_by(Subbasin) %>%
  summarise(wa_Per_Capita_Income = weighted.mean(Per.Capita.Income.HH, 
                                                 w = Population),
            avg_Housing_Units = mean(Housing.Units),
  )

wa_2018 <- cens_2018 %>%
  group_by(Subbasin) %>%
  summarise(wa_Per_Capita_Income = weighted.mean(Per.Capita.Income.HH, 
                                                 w = Population),
            avg_Housing_Units = mean(Housing.Units),
  )

wa_2019 <- cens_2019 %>%
  group_by(Subbasin) %>%
  summarise(wa_Per_Capita_Income = weighted.mean(Per.Capita.Income.HH, 
                                                 w = Population),
            avg_Housing_Units = mean(Housing.Units),
  )

wa_2020 <- cens_2020 %>%
  group_by(Subbasin) %>%
  summarise(wa_Per_Capita_Income = weighted.mean(Per.Capita.Income.HH, 
                                                 w = Population),
            avg_Housing_Units = mean(Housing.Units),
  )

wa_2021 <- cens_2021 %>%
  group_by(Subbasin) %>%
  summarise(wa_Per_Capita_Income = weighted.mean(Per.Capita.Income.HH, 
                                                 w = Population),
            avg_Housing_Units = mean(Housing.Units),
  )

# ---- Merging Data Sets ----
#adding year variable column
wa_2010 <- wa_2010 %>% 
  mutate(Year = "2010") %>% 
  select(Subbasin, Year, everything())

wa_2011 <- wa_2011 %>% 
  mutate(Year = "2011") %>% 
  select(Subbasin, Year, everything())

wa_2012 <- wa_2012 %>% 
  mutate(Year = "2012") %>% 
  select(Subbasin, Year, everything())

wa_2013 <- wa_2013 %>% 
  mutate(Year = "2013") %>% 
  select(Subbasin, Year, everything())

wa_2014 <- wa_2014 %>% 
  mutate(Year = "2014") %>% 
  select(Subbasin, Year, everything())

wa_2015 <- wa_2015 %>% 
  mutate(Year = "2015") %>% 
  select(Subbasin, Year, everything())

wa_2016 <- wa_2016 %>% 
  mutate(Year = "2016") %>% 
  select(Subbasin, Year, everything())

wa_2017 <- wa_2017 %>% 
  mutate(Year = "2017") %>% 
  select(Subbasin, Year, everything())

wa_2018 <- wa_2018 %>% 
  mutate(Year = "2018") %>% 
  select(Subbasin, Year, everything())

wa_2019 <- wa_2019 %>% 
  mutate(Year = "2019") %>% 
  select(Subbasin, Year, everything())

wa_2020 <- wa_2020 %>% 
  mutate(Year = "2020") %>% 
  select(Subbasin, Year, everything())

wa_2021 <- wa_2021 %>% 
  mutate(Year = "2021") %>% 
  select(Subbasin, Year, everything())

#merging weight average df's by subbasin
df_cens <- rbind(wa_2010, wa_2011)
df_cens <- rbind(df_cens, wa_2012)
df_cens <- rbind(df_cens, wa_2013)
df_cens <- rbind(df_cens, wa_2014)
df_cens <- rbind(df_cens, wa_2015)
df_cens <- rbind(df_cens, wa_2016)
df_cens <- rbind(df_cens, wa_2017)
df_cens <- rbind(df_cens, wa_2018)
df_cens <- rbind(df_cens, wa_2019)
df_cens <- rbind(df_cens, wa_2020)
df_cens <- rbind(df_cens, wa_2021)

#exporting df_cens
write_xlsx(df_cens,"AggregateCensusData_NewSB_130.xlsx")

# ---- Models setup ----
#bringing in dataset with other variables
df <- read_excel("WEP_Data.xlsx", sheet = "CensusComplement")

#sorting my census df to alphabetical to match other df
df_cens <- df_cens[order(df_cens$Subbasin), ]

#merging df and df_cens
df <- merge(df,df_cens, by = c("Subbasin","Year"))

# ---- Running FE Models ----
#running original model
model <- plm(DTW.5Ratio ~ Mean.Temp + Precip + avg_Housing_Units + 
               wa_Per_Capita_Income + Irrigated.Pasture, 
             data = df, model = "within")
summary(model)

bptest(model)
model_se <- coeftest(model, vcov = vcovHC(model, type = "HC3"))
coeftest(model, vcov = vcovHC(model, type = "HC3"))               

#looking at between 
model_btw <- plm(DTW.5Ratio ~ Mean.Temp + Precip + avg_Housing_Units + 
                   wa_Per_Capita_Income + Irrigated.Pasture, 
                 data = df, model = "between")
summary(model_btw)


# ---- Creating Dummies ---- 
#creating dummies for regulated and unregulated subbasin
#create a new categorical variable based on "subbasin"
df <- df %>%
  mutate(Regulation = case_when(
    Subbasin %in% c("SCAMASouth","SCAMANorth","Douglas","HarqHass") ~ "Regulated",
    Subbasin %in% c("Benson","GilaBend","SierraVista","Willcox") ~ "Unregulated",
    TRUE ~ "Unknown"  # Handle unexpected values
  ))

#create a dummy variable for "Regulated" (1) and "Unregulated" (0)
df$Dummy_Regulated <- ifelse(df$Subbasin %in% 
                               c("SCAMASouth","SCAMANorth",
                                 "Douglas","HarqHass"), 
                             1, 0)

#creating subbasin dummies
df$Dummy_Benson <- ifelse(df$Subbasin == "Benson", 1, 0)
df$Dummy_Douglas <- ifelse(df$Subbasin == "Douglas", 1, 0)
df$Dummy_GilaBend <- ifelse(df$Subbasin == "GilaBend", 1, 0)
df$Dummy_HarqHass <- ifelse(df$Subbasin == "HarqHass", 1, 0)
df$Dummy_SCAMANorth <- ifelse(df$Subbasin == "SCAMANorth", 1, 0)
df$Dummy_SCAMASouth <- ifelse(df$Subbasin == "SCAMASouth", 1, 0)
df$Dummy_SierraVista <- ifelse(df$Subbasin == "SierraVista", 1, 0)
df$Dummy_Willcox <- ifelse(df$Subbasin == "Willcox", 1, 0)

#exporting new df as an excel
write.csv(df,"df_updated.csv")

# ---- Running OLS Models ----
#loading data
df <- read_xlsx("df_updated.xlsx")

model_ols <- lm(DTW.5Ratio ~ 
                  Dummy_Regulated  
                + Mean.Temp + Precip + Irrigated.Pasture + wa_Per_Capita_Income 
                + avg_Housing_Units, data = df)
summary(model_ols)

bptest(model_ols)
model_ols_se <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC3"))
coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC3"))               

#running with recharge dummy
model_recharge <- lm(DTW.5Ratio ~ 
                       Dummy_Regulated + Dummy_Recharge 
                     + Mean.Temp + Precip + Irrigated.Pasture + wa_Per_Capita_Income 
                     + avg_Housing_Units, data = df)
summary(model_recharge)

bptest(model_recharge)
model_recharge_se <- coeftest(model_recharge, vcov = vcovHC(model_recharge, type = "HC3"))
coeftest(model_recharge, vcov = vcovHC(model_recharge, type = "HC3"))

#now looking at dummy subbasins instead of dummy regulated
model_ols_1 <- lm(DTW.5Ratio ~ 
                    Dummy_Benson +Dummy_Douglas + Dummy_GilaBend + Dummy_HarqHass
                  + Dummy_SCAMANorth + Dummy_SCAMASouth + Dummy_SierraVista
                  + Mean.Temp + Precip + Irrigated.Pasture + wa_Per_Capita_Income 
                  + avg_Housing_Units, data = df)
summary(model_ols_1)

bptest(model_ols_1)
model_ols_1_se <- coeftest(model_ols_1, vcov = vcovHC(model_ols_1, type = "HC3"))
coeftest(model_ols_1, vcov = vcovHC(model_ols_1, type = "HC3"))               


# ---- Running Lagged Model with DTW diff ----
#I computed a change variable (ex. 2011-2010 dtw)
#want to compare change from prev to current year with prev year variables

#loading data
df <- read_xlsx("df_updated.xlsx",sheet = "df_diff")

#running fe model
fe_change <- plm(DTW.Diff ~ Mean.Temp + Precip + avg_Housing_Units + 
                   wa_Per_Capita_Income + Irrigated.Pasture, 
                 data = df, model = "within")
summary(fe_change)

bptest(fe_change)
fe_change_se <- coeftest(fe_change, vcov = vcovHC(fe_change, type = "HC3"))
coeftest(fe_change, vcov = vcovHC(fe_change, type = "HC3"))               

#running ols model
ols_change <- lm(DTW.Diff ~ 
                   Dummy_Regulated + Dummy_Recharge 
                 + Mean.Temp + Precip + Irrigated.Pasture + wa_Per_Capita_Income 
                 + avg_Housing_Units, data = df)
summary(ols_change)

bptest(ols_change)
ols_change_se <- coeftest(ols_change, vcov = vcovHC(ols_change, type = "HC3"))
coeftest(ols_change, vcov = vcovHC(ols_change, type = "HC3"))

#running fe and ols with percent change

#fe
fe_pc <- plm(DTW.Percent.Change ~ Mean.Temp + Precip + avg_Housing_Units + 
               wa_Per_Capita_Income + Irrigated.Pasture, 
             data = df, model = "within")
summary(fe_pc)

bptest(fe_pc)
fe_pc_se <- coeftest(fe_pc, vcov = vcovHC(fe_pc, type = "HC3"))
coeftest(fe_pc, vcov = vcovHC(fe_pc, type = "HC3"))               

#ols
ols_pc <- lm(DTW.Percent.Change ~ 
               Dummy_Regulated + Dummy_Recharge 
             + Mean.Temp + Precip + Irrigated.Pasture + wa_Per_Capita_Income 
             + avg_Housing_Units, data = df)
summary(ols_pc)

bptest(ols_pc)
ols_pc_se <- coeftest(ols_pc, vcov = vcovHC(ols_pc, type = "HC3"))
coeftest(ols_pc, vcov = vcovHC(ols_pc, type = "HC3"))

# ---- Visualizing Models ----
stargazer(model_ols_se,model_recharge_se,model_ols_1_se,model_se,type = "text",
          dep.var.labels = "Depth to Water",
          covariate.labels = c("Regulated (Dummy)", "Recharge (Dummy)", 
                               "Benson (Dummy)","Douglas (Dummy)",
                               "Gila Bend (Dummy)", "HarqHass (Dummy)",
                               "SCAMA North (Dummy)","SCAMA South (Dummy)",
                               "Sierra Vista (Dummy)", "Temperature",
                               "Precipitation", "Irrigated Acreage",
                               "Per Capita Income", "Housing Units"),
          out = "improved_models_Oct15_3.txt")

# ---- Visualizing Variables ----
#creating trends for per capita income, avg housing units, and irrigated acres
ggplot(data = df, aes(x = Year, y = wa_Per_Capita_Income, color = Subbasin)) +
  geom_line() +              # Add data points
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear trend line
  labs(title = "Trend Line of Per Capita Income", x = "Year", 
       y = "Per Capita Income")

ggplot(data = df, aes(x = Year, y = avg_Housing_Units, color = Subbasin)) +
  geom_line() +              # Add data points
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear trend line
  labs(title = "Trend Line of Housing Units", x = "Year", 
       y = "Housing Units")

ggplot(data = df, aes(x = Year, y = Irrigated.Pasture, color = Subbasin)) +
  geom_line() +              # Add data points
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear trend line
  labs(title = "Trend Line of Irrigaed Acres", x = "Year", 
       y = "Irrigated Acres")

#DTW trend
ggplot(data = df, aes(x = Year, y = DTW.5Ratio, color = Subbasin)) +
  geom_line() +              # Add data points
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear trend line
  labs(title = "Trend Line of DTW", x = "Year", 
       y = "Depth to Water")
