## Creating Official Trend Graphs (normalized changes)
# Created: 2/14/24
# Updated: 6/1/24 to updata graphs with percent changes and split btwn reg and unreg
# Purpose: WEP Revisions
# Author: Zoey Reed-Spitzer

# ---- Setup ----
setwd("C:/Users/zoeys/Documents/Thesis")

#loading packages
library(readxl)
library(zoo)
library(dplyr)
library(tidyr)
library(sandwich)
library(writexl)
library(ggplot2)

#loading data
df <- read_xlsx("Df_AllSB_upto211Update.xlsx", sheet = "Sheet1")

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
         Change_Income = wa_Per_Capita_Income - first(wa_Per_Capita_Income),
         pct_change_DTW = Change_DTW/first(DTW)*100,
         pct_change_Temp = Change_Temp/first(Temp.C)*100,
         pct_change_Precip = Change_Precip/first(Precip)*100,
         pct_change_IrrAcres = Change_IrrAcres/first(Irrigated.Acres)*100,
         pct_change_Housing = Change_Housing/first(avg_Housing_Units)*100,
         pct_change_Income = Change_Income/first(wa_Per_Capita_Income)*100)

# ---- DTW Trends ----
ggplot(df, aes(x = Year, y = Change_DTW, color = Subbasin, linetype = Regulation)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("Regulated"="solid", "Unregulated"="dashed")) +
  labs(x = "Year", y = "Change in DTW", title = "Change in DTW in each Subbarea (Starting from 2010)") +
  theme_minimal() +
  scale_y_reverse()  # Flip the y-axis

#percent change
ggplot(df, aes(x = Year, y = pct_change_DTW, color = Subbasin, linetype = Regulation)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("Regulated"="solid", "Unregulated"="dashed")) +
  labs(x = "Year", y = "% Change in DTW from 2010", title = "Percent Change in DTW in each Subbarea (From 2010)") +
  theme_minimal() +
  scale_y_reverse()  # Flip the y-axis

#regulation and unregulated subbasins separately
#split df
df_reg <- df %>% 
  filter(Regulation == "Regulated")
df_unreg <- df %>% 
  filter(Regulation == "Unregulated")

ggplot(df_reg, aes(x = Year, y = pct_change_DTW, color = Subbasin)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Year", y = "% Change in DTW from 2010", title = "Percent Change in DTW in Regulated Sub-basins (From 2010)") +
  theme_minimal() +
  scale_y_reverse() + # Flip the y-axis
  scale_color_brewer(palette = "Set2") +  # Use a distinct color palette
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))  # Increase legend text size


ggplot(df_unreg, aes(x = Year, y = pct_change_DTW, color = Subbasin)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Year", y = "% Change in DTW from 2010", title = "Percent Change in DTW in Unregulated Sub-basins (From 2010)") +
  theme_minimal() +
  scale_y_reverse() +  # Flip the y-axis
  scale_color_brewer(palette = "Set2") +  # Use a distinct color palette
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))  # Increase legend text size

# ---- Temp Trends ----
ggplot(df, aes(x = Year, y = Change_Temp, color = Subbasin)) +
  geom_line(linewidth = 1) +
  labs(x = "Year", y = "Change in Temperature (C)", title = "Change in Temperature in each Subbarea (Starting from 2010)") +
  theme_minimal()


ggplot(df_reg, aes(x = Year, y = Temp.C, color = Subbasin)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Year", y = "Temperature (C)", title = "Temperature Trends in Regulated Sub-basins (2010-2021)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +  # Use a distinct color palette
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))  # Increase legend text size


ggplot(df_unreg, aes(x = Year, y = Temp.C, color = Subbasin)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Year", y = "Temperature (C)", title = "Temperature Trends in Unregulated Sub-basins (2010-2021)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +  # Use a distinct color palette
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))  # Increase legend text size


# ---- Precip ----
ggplot(df, aes(x = Year, y = Change_Precip, color = Subbasin)) +
  geom_line(linewidth = 1) +
  labs(x = "Year", y = "Change in Precipitation (Inches)", title = "Change in Precipitation in each Subbarea (Starting from 2010)") +
  theme_minimal()

ggplot(df_reg, aes(x = Year, y = Precip, color = Subbasin)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Year", y = "Precipitation (Inches)", title = "Precipitation Trends in Regulated Sub-basins (2010-2021)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +  # Use a distinct color palette
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))  # Increase legend text size


ggplot(df_unreg, aes(x = Year, y = Precip, color = Subbasin)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Year", y = "Precipitation (Inches)", title = "Precipitation Trends in Unregulated Sub-basins (2010-2021)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +  # Use a distinct color palette
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))  # Increase legend text size



# ---- Irrigated Acres ----
ggplot(df, aes(x = Year, y = Change_IrrAcres, color = Subbasin, linetype = Regulation)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("Regulated"="solid", "Unregulated"="dashed")) +
  labs(x = "Year", y = "Change in Irrigated Cropland (Acres)", title = "Change in Irrigated Cropland in each Subbarea (Starting from 2010)") +
  theme_minimal()

# percent changes & split between reg and unreg sub-basins
#split df
df_reg <- df %>% 
  filter(Regulation == "Regulated")
df_unreg <- df %>% 
  filter(Regulation == "Unregulated")

ggplot(df_reg, aes(x = Year, y = pct_change_IrrAcres, color = Subbasin)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Year", y = "% Change in Planted Acreage from 2010", title = "Percent Change in Planted Acreage in Regulated Sub-basins (From 2010)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +  # Use a distinct color palette
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))  # Increase legend text size


ggplot(df_unreg, aes(x = Year, y = pct_change_IrrAcres, color = Subbasin)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Year", y = "% Change in Planted Acreage from 2010", title = "Percent Change in Planted Acreage in Unregulated Sub-basins (From 2010)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +  # Use a distinct color palette
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))  # Increase legend text size


# ---- Housing Units ----
ggplot(df, aes(x = Year, y = Change_Housing, color = Subbasin, linetype = Regulation)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("Regulated"="solid", "Unregulated"="dashed")) +
  labs(x = "Year", y = "Change in Housing Units", title = "Change in Housing Units in each Subbarea (Starting from 2010)") +
  theme_minimal()

# percent changes & split between reg and unreg sub-basins
#split df
df_reg <- df %>% 
  filter(Regulation == "Regulated")
df_unreg <- df %>% 
  filter(Regulation == "Unregulated")

ggplot(df_reg, aes(x = Year, y = pct_change_Housing, color = Subbasin)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Year", y = "% Change in # Housing Units from 2010", title = "Percent Change in # Housing Units in Regulated Sub-basins (From 2010)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +  # Use a distinct color palette
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))  # Increase legend text size


ggplot(df_unreg, aes(x = Year, y = pct_change_Housing, color = Subbasin)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Year", y = "% Change in # Housing Units from 2010", title = "Percent Change in # Housing Units in Unregulated Sub-basins (From 2010)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +  # Use a distinct color palette
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))  # Increase legend text size


# ---- Per Capita Income ----
ggplot(df, aes(x = Year, y = Change_Income, color = Subbasin, linetype = Regulation)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("Regulated"="solid", "Unregulated"="dashed")) +
  labs(x = "Year", y = "Change in Per Capita Income (USD)", title = "Change in Per Capita Income in each Subbarea (Starting from 2010)") +
  theme_minimal()
