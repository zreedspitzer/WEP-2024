## This is code to compare median income by ethnicity in each subarea
# Created: 9/7/23
# Purpose: WEP Revisions
# Author: Zoey Reed-Spitzer

#############################################################################
## Preparing Work Space
#############################################################################
setwd("C:/Users/zoeys/Documents/Data/Census Data/Median Income & Ethnicity")

library(readxl)
library(plm)
library(lme4)
library(lmtest)
library(zoo)
library(dplyr)
library(tidyr)
library(sandwich)

##############################################################################
## Importing Data
##############################################################################
white_2010 <- read_excel("White/2010_White.xlsx", sheet = "Sheet1")
white_2011 <- read_excel("White/2011_White.xlsx", sheet = "Sheet1")
white_2012 <- read_excel("White/2012_White.xlsx", sheet = "Sheet1")
white_2013 <- read_excel("White/2013_White.xlsx", sheet = "Sheet1")
white_2014 <- read_excel("White/2014_White.xlsx", sheet = "Sheet1")
white_2015 <- read_excel("White/2015_White.xlsx", sheet = "Sheet1")
white_2016 <- read_excel("White/2016_White.xlsx", sheet = "Sheet1")
white_2017 <- read_excel("White/2017_White.xlsx", sheet = "Sheet1")
white_2018 <- read_excel("White/2018_White.xlsx", sheet = "Sheet1")
white_2019 <- read_excel("White/2019_White.xlsx", sheet = "Sheet1")
white_2020 <- read_excel("White/2020_White.xlsx", sheet = "Sheet1")
white_2021 <- read_excel("White/2021_White.xlsx", sheet = "Sheet1")

asian_2010 <- read_excel("Asian/2010_Asian.xlsx", sheet = "Sheet1")
asian_2011 <- read_excel("Asian/2011_Asian.xlsx", sheet = "Sheet1")
asian_2012 <- read_excel("Asian/2012_Asian.xlsx", sheet = "Sheet1")
asian_2013 <- read_excel("Asian/2013_Asian.xlsx", sheet = "Sheet1")
asian_2014 <- read_excel("Asian/2014_Asian.xlsx", sheet = "Sheet1")
asian_2015 <- read_excel("Asian/2015_Asian.xlsx", sheet = "Sheet1")
asian_2016 <- read_excel("Asian/2016_Asian.xlsx", sheet = "Sheet1")
asian_2017 <- read_excel("Asian/2017_Asian.xlsx", sheet = "Sheet1")
asian_2018 <- read_excel("Asian/2018_Asian.xlsx", sheet = "Sheet1")
asian_2019 <- read_excel("Asian/2019_Asian.xlsx", sheet = "Sheet1")
asian_2020 <- read_excel("Asian/2020_Asian.xlsx", sheet = "Sheet1")
asian_2021 <- read_excel("Asian/2021_Asian.xlsx", sheet = "Sheet1")

black_2010 <- read_excel("Black/2010_Black.xlsx", sheet = "Sheet1")
black_2011 <- read_excel("Black/2011_Black.xlsx", sheet = "Sheet1")
black_2012 <- read_excel("Black/2012_Black.xlsx", sheet = "Sheet1")
black_2013 <- read_excel("Black/2013_Black.xlsx", sheet = "Sheet1")
black_2014 <- read_excel("Black/2014_Black.xlsx", sheet = "Sheet1")
black_2015 <- read_excel("Black/2015_Black.xlsx", sheet = "Sheet1")
black_2016 <- read_excel("Black/2016_Black.xlsx", sheet = "Sheet1")
black_2017 <- read_excel("Black/2017_Black.xlsx", sheet = "Sheet1")
black_2018 <- read_excel("Black/2018_Black.xlsx", sheet = "Sheet1")
black_2019 <- read_excel("Black/2019_Black.xlsx", sheet = "Sheet1")
black_2020 <- read_excel("Black/2020_Black.xlsx", sheet = "Sheet1")
black_2021 <- read_excel("Black/2021_Black.xlsx", sheet = "Sheet1")

Haw_2010 <- read_excel("Hawaiian & Pacific Islander/2010_Hawaii.xlsx", sheet = "Sheet1")
Haw_2011 <- read_excel("Hawaiian & Pacific Islander/2011_Hawaii.xlsx", sheet = "Sheet1")
Haw_2012 <- read_excel("Hawaiian & Pacific Islander/2012_Hawaii.xlsx", sheet = "Sheet1")
Haw_2013 <- read_excel("Hawaiian & Pacific Islander/2013_Hawaii.xlsx", sheet = "Sheet1")
Haw_2014 <- read_excel("Hawaiian & Pacific Islander/2014_Hawaii.xlsx", sheet = "Sheet1")
Haw_2015 <- read_excel("Hawaiian & Pacific Islander/2015_Hawaii.xlsx", sheet = "Sheet1")
Haw_2016 <- read_excel("Hawaiian & Pacific Islander/2016_Hawaii.xlsx", sheet = "Sheet1")
Haw_2017 <- read_excel("Hawaiian & Pacific Islander/2017_Hawaii.xlsx", sheet = "Sheet1")
Haw_2018 <- read_excel("Hawaiian & Pacific Islander/2018_Hawaii.xlsx", sheet = "Sheet1")
Haw_2019 <- read_excel("Hawaiian & Pacific Islander/2019_Hawaii.xlsx", sheet = "Sheet1")
Haw_2020 <- read_excel("Hawaiian & Pacific Islander/2020_Hawaii.xlsx", sheet = "Sheet1")
Haw_2021 <- read_excel("Hawaiian & Pacific Islander/2021_Hawaii.xlsx", sheet = "Sheet1")

hisp_2010 <- read_excel("Hispanic/2010_Hisp.xlsx", sheet = "Sheet1")
hisp_2011 <- read_excel("Hispanic/2011_Hisp.xlsx", sheet = "Sheet1")
hisp_2012 <- read_excel("Hispanic/2012_Hisp.xlsx", sheet = "Sheet1")
hisp_2013 <- read_excel("Hispanic/2013_Hisp.xlsx", sheet = "Sheet1")
hisp_2014 <- read_excel("Hispanic/2014_Hisp.xlsx", sheet = "Sheet1")
hisp_2015 <- read_excel("Hispanic/2015_Hisp.xlsx", sheet = "Sheet1")
hisp_2016 <- read_excel("Hispanic/2016_Hisp.xlsx", sheet = "Sheet1")
hisp_2017 <- read_excel("Hispanic/2017_Hisp.xlsx", sheet = "Sheet1")
hisp_2018 <- read_excel("Hispanic/2018_Hisp.xlsx", sheet = "Sheet1")
hisp_2019 <- read_excel("Hispanic/2019_Hisp.xlsx", sheet = "Sheet1")
hisp_2020 <- read_excel("Hispanic/2020_Hisp.xlsx", sheet = "Sheet1")
hisp_2021 <- read_excel("Hispanic/2021_Hisp.xlsx", sheet = "Sheet1")

ind_2010 <- read_excel("Indian & Alaskan Native/2010_Ind.xlsx", sheet = "Sheet1")
ind_2011 <- read_excel("Indian & Alaskan Native/2011_Ind.xlsx", sheet = "Sheet1")
ind_2012 <- read_excel("Indian & Alaskan Native/2012_Ind.xlsx", sheet = "Sheet1")
ind_2013 <- read_excel("Indian & Alaskan Native/2013_Ind.xlsx", sheet = "Sheet1")
ind_2014 <- read_excel("Indian & Alaskan Native/2014_Ind.xlsx", sheet = "Sheet1")
ind_2015 <- read_excel("Indian & Alaskan Native/2015_Ind.xlsx", sheet = "Sheet1")
ind_2016 <- read_excel("Indian & Alaskan Native/2016_Ind.xlsx", sheet = "Sheet1")
ind_2017 <- read_excel("Indian & Alaskan Native/2017_Ind.xlsx", sheet = "Sheet1")
ind_2018 <- read_excel("Indian & Alaskan Native/2018_Ind.xlsx", sheet = "Sheet1")
ind_2019 <- read_excel("Indian & Alaskan Native/2019_Ind.xlsx", sheet = "Sheet1")
ind_2020 <- read_excel("Indian & Alaskan Native/2020_Ind.xlsx", sheet = "Sheet1")
ind_2021 <- read_excel("Indian & Alaskan Native/2021_Ind.xlsx", sheet = "Sheet1")

mix_2010 <- read_excel("Mixed/2010_Mix.xlsx", sheet = "Sheet1")
mix_2011 <- read_excel("Mixed/2011_Mix.xlsx", sheet = "Sheet1")
mix_2012 <- read_excel("Mixed/2012_Mix.xlsx", sheet = "Sheet1")
mix_2013 <- read_excel("Mixed/2013_Mix.xlsx", sheet = "Sheet1")
mix_2014 <- read_excel("Mixed/2014_Mix.xlsx", sheet = "Sheet1")
mix_2015 <- read_excel("Mixed/2015_Mix.xlsx", sheet = "Sheet1")
mix_2016 <- read_excel("Mixed/2016_Mix.xlsx", sheet = "Sheet1")
mix_2017 <- read_excel("Mixed/2017_Mix.xlsx", sheet = "Sheet1")
mix_2018 <- read_excel("Mixed/2018_Mix.xlsx", sheet = "Sheet1")
mix_2019 <- read_excel("Mixed/2019_Mix.xlsx", sheet = "Sheet1")
mix_2020 <- read_excel("Mixed/2020_Mix.xlsx", sheet = "Sheet1")
mix_2021 <- read_excel("Mixed/2021_Mix.xlsx", sheet = "Sheet1")

oth_2010 <- read_excel("Other/2010_Oth.xlsx", sheet = "Sheet1")
oth_2011 <- read_excel("Other/2011_Oth.xlsx", sheet = "Sheet1")
oth_2012 <- read_excel("Other/2012_Oth.xlsx", sheet = "Sheet1")
oth_2013 <- read_excel("Other/2013_Oth.xlsx", sheet = "Sheet1")
oth_2014 <- read_excel("Other/2014_Oth.xlsx", sheet = "Sheet1")
oth_2015 <- read_excel("Other/2015_Oth.xlsx", sheet = "Sheet1")
oth_2016 <- read_excel("Other/2016_Oth.xlsx", sheet = "Sheet1")
oth_2017 <- read_excel("Other/2017_Oth.xlsx", sheet = "Sheet1")
oth_2018 <- read_excel("Other/2018_Oth.xlsx", sheet = "Sheet1")
oth_2019 <- read_excel("Other/2019_Oth.xlsx", sheet = "Sheet1")
oth_2020 <- read_excel("Other/2020_Oth.xlsx", sheet = "Sheet1")
oth_2021 <- read_excel("Other/2021_Oth.xlsx", sheet = "Sheet1")

#############################################################################
# Now changing variable names
#############################################################################
white_2010 <- white_2010 %>%
  rename(GeoID = "Geography", White_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name")
white_2011 <- white_2011 %>%
  rename(GeoID = "Geography", White_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)")
white_2012 <- white_2012 %>%
  rename(GeoID = "Geography", White_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)")
white_2013 <- white_2013 %>%
  rename(GeoID = "Geography", White_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)")
white_2014 <- white_2014 %>%
  rename(GeoID = "Geography", White_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)")
white_2015 <- white_2015 %>%
  rename(GeoID = "Geography", White_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)")
white_2016 <- white_2016 %>%
  rename(GeoID = "Geography", White_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)")
white_2017 <- white_2017 %>%
  rename(GeoID = "Geography", White_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)")
white_2018 <- white_2018 %>%
  rename(GeoID = "Geography", White_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)")
white_2019 <- white_2019 %>%
  rename(GeoID = "Geography", White_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)")
white_2020 <- white_2020 %>%
  rename(GeoID = "Geography", White_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)")
white_2021 <- white_2021 %>%
  rename(GeoID = "Geography", White_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)")

asian_2010 <- asian_2010 %>%
  rename(GeoID = "Geography", Asian_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name", 
         -"Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)")
asian_2011 <- asian_2011 %>%
  rename(GeoID = "Geography", Asian_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)")
asian_2012 <- asian_2012 %>%
  rename(GeoID = "Geography", Asian_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)")
asian_2013 <- asian_2013 %>%
  rename(GeoID = "Geography", Asian_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)")
asian_2014 <- asian_2014 %>%
  rename(GeoID = "Geography", Asian_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)")
asian_2015 <- asian_2015 %>%
  rename(GeoID = "Geography", Asian_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)")
asian_2016 <- asian_2016 %>%
  rename(GeoID = "Geography", Asian_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)")
asian_2017 <- asian_2017 %>%
  rename(GeoID = "Geography", Asian_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)")
asian_2018 <- asian_2018 %>%
  rename(GeoID = "Geography", Asian_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)")
asian_2019 <- asian_2019 %>%
  rename(GeoID = "Geography", Asian_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)")
asian_2020 <- asian_2020 %>%
  rename(GeoID = "Geography", Asian_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)")
asian_2021 <- asian_2021 %>%
  rename(GeoID = "Geography", Asian_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)")

black_2010 <- black_2010 %>%
  rename(GeoID = "Geography", Black_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name", 
         -"Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)")
black_2011 <- black_2011 %>%
  rename(GeoID = "Geography", Black_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)")
black_2012 <- black_2012 %>%
  rename(GeoID = "Geography", Black_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)")
black_2013 <- black_2013 %>%
  rename(GeoID = "Geography", Black_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)")
black_2014 <- black_2014 %>%
  rename(GeoID = "Geography", Black_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)")
black_2015 <- black_2015 %>%
  rename(GeoID = "Geography", Black_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)")
black_2016 <- black_2016 %>%
  rename(GeoID = "Geography", Black_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)")
black_2017 <- black_2017 %>%
  rename(GeoID = "Geography", Black_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)")
black_2018 <- black_2018 %>%
  rename(GeoID = "Geography", Black_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)")
black_2019 <- black_2019 %>%
  rename(GeoID = "Geography", Black_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)")
black_2020 <- black_2020 %>%
  rename(GeoID = "Geography", Black_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)")
black_2021 <- black_2021 %>%
  rename(GeoID = "Geography", Black_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)")

Haw_2010 <- Haw_2010 %>%
  rename(GeoID = "Geography", Haw_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name", 
         -"Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)")
Haw_2011 <- Haw_2011 %>%
  rename(GeoID = "Geography", Haw_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)")
Haw_2012 <- Haw_2012 %>%
  rename(GeoID = "Geography", Haw_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)")
Haw_2013 <- Haw_2013 %>%
  rename(GeoID = "Geography", Haw_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)")
Haw_2014 <- Haw_2014 %>%
  rename(GeoID = "Geography", Haw_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)")
Haw_2015 <- Haw_2015 %>%
  rename(GeoID = "Geography", Haw_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)")
Haw_2016 <- Haw_2016 %>%
  rename(GeoID = "Geography", Haw_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)")
Haw_2017 <- Haw_2017 %>%
  rename(GeoID = "Geography", Haw_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)")
Haw_2018 <- Haw_2018 %>%
  rename(GeoID = "Geography", Haw_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)")
Haw_2019 <- Haw_2019 %>%
  rename(GeoID = "Geography", Haw_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)")
Haw_2020 <- Haw_2020 %>%
  rename(GeoID = "Geography", Haw_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)")
Haw_2021 <- Haw_2021 %>%
  rename(GeoID = "Geography", Haw_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)")

hisp_2010 <- hisp_2010 %>%
  rename(GeoID = "Geography", hisp_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name", 
         -"Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)")
hisp_2011 <- hisp_2011 %>%
  rename(GeoID = "Geography", hisp_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)")
hisp_2012 <- hisp_2012 %>%
  rename(GeoID = "Geography", hisp_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)")
hisp_2013 <- hisp_2013 %>%
  rename(GeoID = "Geography", hisp_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)")
hisp_2014 <- hisp_2014 %>%
  rename(GeoID = "Geography", hisp_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)")
hisp_2015 <- hisp_2015 %>%
  rename(GeoID = "Geography", hisp_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)")
hisp_2016 <- hisp_2016 %>%
  rename(GeoID = "Geography", hisp_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)")
hisp_2017 <- hisp_2017 %>%
  rename(GeoID = "Geography", hisp_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)")
hisp_2018 <- hisp_2018 %>%
  rename(GeoID = "Geography", hisp_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)")
hisp_2019 <- hisp_2019 %>%
  rename(GeoID = "Geography", hisp_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)")
hisp_2020 <- hisp_2020 %>%
  rename(GeoID = "Geography", hisp_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)")
hisp_2021 <- hisp_2021 %>%
  rename(GeoID = "Geography", hisp_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)")

ind_2010 <- ind_2010 %>%
  rename(GeoID = "Geography", ind_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name", 
         -"Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)")
ind_2011 <- ind_2011 %>%
  rename(GeoID = "Geography", ind_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)")
ind_2012 <- ind_2012 %>%
  rename(GeoID = "Geography", ind_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)")
ind_2013 <- ind_2013 %>%
  rename(GeoID = "Geography", ind_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)")
ind_2014 <- ind_2014 %>%
  rename(GeoID = "Geography", ind_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)")
ind_2015 <- ind_2015 %>%
  rename(GeoID = "Geography", ind_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)")
ind_2016 <- ind_2016 %>%
  rename(GeoID = "Geography", ind_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)")
ind_2017 <- ind_2017 %>%
  rename(GeoID = "Geography", ind_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)")
ind_2018 <- ind_2018 %>%
  rename(GeoID = "Geography", ind_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)")
ind_2019 <- ind_2019 %>%
  rename(GeoID = "Geography", ind_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)")
ind_2020 <- ind_2020 %>%
  rename(GeoID = "Geography", ind_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)")
ind_2021 <- ind_2021 %>%
  rename(GeoID = "Geography", ind_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)")

mix_2010 <- mix_2010 %>%
  rename(GeoID = "Geography", mix_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name", 
         -"Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)")
mix_2011 <- mix_2011 %>%
  rename(GeoID = "Geography", mix_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)")
mix_2012 <- mix_2012 %>%
  rename(GeoID = "Geography", mix_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)")
mix_2013 <- mix_2013 %>%
  rename(GeoID = "Geography", mix_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)")
mix_2014 <- mix_2014 %>%
  rename(GeoID = "Geography", mix_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)")
mix_2015 <- mix_2015 %>%
  rename(GeoID = "Geography", mix_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)")
mix_2016 <- mix_2016 %>%
  rename(GeoID = "Geography", mix_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)")
mix_2017 <- mix_2017 %>%
  rename(GeoID = "Geography", mix_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)")
mix_2018 <- mix_2018 %>%
  rename(GeoID = "Geography", mix_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)")
mix_2019 <- mix_2019 %>%
  rename(GeoID = "Geography", mix_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)")
mix_2020 <- mix_2020 %>%
  rename(GeoID = "Geography", mix_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)")
mix_2021 <- mix_2021 %>%
  rename(GeoID = "Geography", mix_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)")

oth_2010 <- oth_2010 %>%
  rename(GeoID = "Geography", oth_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name", 
         -"Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2010 inflation-adjusted dollars)")
oth_2011 <- oth_2011 %>%
  rename(GeoID = "Geography", oth_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2011 inflation-adjusted dollars)")
oth_2012 <- oth_2012 %>%
  rename(GeoID = "Geography", oth_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2012 inflation-adjusted dollars)")
oth_2013 <- oth_2013 %>%
  rename(GeoID = "Geography", oth_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2013 inflation-adjusted dollars)")
oth_2014 <- oth_2014 %>%
  rename(GeoID = "Geography", oth_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)")
oth_2015 <- oth_2015 %>%
  rename(GeoID = "Geography", oth_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)")
oth_2016 <- oth_2016 %>%
  rename(GeoID = "Geography", oth_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2016 inflation-adjusted dollars)")
oth_2017 <- oth_2017 %>%
  rename(GeoID = "Geography", oth_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2017 inflation-adjusted dollars)")
oth_2018 <- oth_2018 %>%
  rename(GeoID = "Geography", oth_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)")
oth_2019 <- oth_2019 %>%
  rename(GeoID = "Geography", oth_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)")
oth_2020 <- oth_2020 %>%
  rename(GeoID = "Geography", oth_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)")
oth_2021 <- oth_2021 %>%
  rename(GeoID = "Geography", oth_Med_Inc = "Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)") %>%
  select(-"Geographic Area Name",
         -"Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Margin of Error!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)",
         -"Annotation of Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)")


#############################################################################
# Now merging the data sets by year by race to get 11 different data sets
#############################################################################

merged_2010 <- merge(white_2010,asian_2010, "GeoID", all = TRUE)
merged_2010 <- merge(merged_2010,black_2010, "GeoID", all = TRUE)
merged_2010 <- merge(merged_2010,hisp_2010, "GeoID", all = TRUE)
merged_2010 <- merge(merged_2010,ind_2010, "GeoID", all = TRUE)
merged_2010 <- merge(merged_2010,Haw_2010, "GeoID", all = TRUE)
merged_2010 <- merge(merged_2010,mix_2010, "GeoID", all = TRUE)
merged_2010 <- merge(merged_2010,oth_2010, "GeoID", all = TRUE)

merged_2011 <- merge(white_2011,asian_2011, "GeoID", all = TRUE)
merged_2011 <- merge(merged_2011,black_2011, "GeoID", all = TRUE)
merged_2011 <- merge(merged_2011,hisp_2011, "GeoID", all = TRUE)
merged_2011 <- merge(merged_2011,ind_2011, "GeoID", all = TRUE)
merged_2011 <- merge(merged_2011,Haw_2011, "GeoID", all = TRUE)
merged_2011 <- merge(merged_2011,mix_2011, "GeoID", all = TRUE)
merged_2011 <- merge(merged_2011,oth_2011, "GeoID", all = TRUE)

merged_2012 <- merge(white_2012,asian_2012, "GeoID", all = TRUE)
merged_2012 <- merge(merged_2012,black_2012, "GeoID", all = TRUE)
merged_2012 <- merge(merged_2012,hisp_2012, "GeoID", all = TRUE)
merged_2012 <- merge(merged_2012,ind_2012, "GeoID", all = TRUE)
merged_2012 <- merge(merged_2012,Haw_2012, "GeoID", all = TRUE)
merged_2012 <- merge(merged_2012,mix_2012, "GeoID", all = TRUE)
merged_2012 <- merge(merged_2012,oth_2012, "GeoID", all = TRUE)

merged_2013 <- merge(white_2013,asian_2013, "GeoID", all = TRUE)
merged_2013 <- merge(merged_2013,black_2013, "GeoID", all = TRUE)
merged_2013 <- merge(merged_2013,hisp_2013, "GeoID", all = TRUE)
merged_2013 <- merge(merged_2013,ind_2013, "GeoID", all = TRUE)
merged_2013 <- merge(merged_2013,Haw_2013, "GeoID", all = TRUE)
merged_2013 <- merge(merged_2013,mix_2013, "GeoID", all = TRUE)
merged_2013 <- merge(merged_2013,oth_2013, "GeoID", all = TRUE)

merged_2014 <- merge(white_2014,asian_2014, "GeoID", all = TRUE)
merged_2014 <- merge(merged_2014,black_2014, "GeoID", all = TRUE)
merged_2014 <- merge(merged_2014,hisp_2014, "GeoID", all = TRUE)
merged_2014 <- merge(merged_2014,ind_2014, "GeoID", all = TRUE)
merged_2014 <- merge(merged_2014,Haw_2014, "GeoID", all = TRUE)
merged_2014 <- merge(merged_2014,mix_2014, "GeoID", all = TRUE)
merged_2014 <- merge(merged_2014,oth_2014, "GeoID", all = TRUE)

merged_2015 <- merge(white_2015,asian_2015, "GeoID", all = TRUE)
merged_2015 <- merge(merged_2015,black_2015, "GeoID", all = TRUE)
merged_2015 <- merge(merged_2015,hisp_2015, "GeoID", all = TRUE)
merged_2015 <- merge(merged_2015,ind_2015, "GeoID", all = TRUE)
merged_2015 <- merge(merged_2015,Haw_2015, "GeoID", all = TRUE)
merged_2015 <- merge(merged_2015,mix_2015, "GeoID", all = TRUE)
merged_2015 <- merge(merged_2015,oth_2015, "GeoID", all = TRUE)

merged_2016 <- merge(white_2016,asian_2016, "GeoID", all = TRUE)
merged_2016 <- merge(merged_2016,black_2016, "GeoID", all = TRUE)
merged_2016 <- merge(merged_2016,hisp_2016, "GeoID", all = TRUE)
merged_2016 <- merge(merged_2016,ind_2016, "GeoID", all = TRUE)
merged_2016 <- merge(merged_2016,Haw_2016, "GeoID", all = TRUE)
merged_2016 <- merge(merged_2016,mix_2016, "GeoID", all = TRUE)
merged_2016 <- merge(merged_2016,oth_2016, "GeoID", all = TRUE)

merged_2017 <- merge(white_2017,asian_2017, "GeoID", all = TRUE)
merged_2017 <- merge(merged_2017,black_2017, "GeoID", all = TRUE)
merged_2017 <- merge(merged_2017,hisp_2017, "GeoID", all = TRUE)
merged_2017 <- merge(merged_2017,ind_2017, "GeoID", all = TRUE)
merged_2017 <- merge(merged_2017,Haw_2017, "GeoID", all = TRUE)
merged_2017 <- merge(merged_2017,mix_2017, "GeoID", all = TRUE)
merged_2017 <- merge(merged_2017,oth_2017, "GeoID", all = TRUE)

merged_2018 <- merge(white_2018,asian_2018, "GeoID", all = TRUE)
merged_2018 <- merge(merged_2018,black_2018, "GeoID", all = TRUE)
merged_2018 <- merge(merged_2018,hisp_2018, "GeoID", all = TRUE)
merged_2018 <- merge(merged_2018,ind_2018, "GeoID", all = TRUE)
merged_2018 <- merge(merged_2018,Haw_2018, "GeoID", all = TRUE)
merged_2018 <- merge(merged_2018,mix_2018, "GeoID", all = TRUE)
merged_2018 <- merge(merged_2018,oth_2018, "GeoID", all = TRUE)

merged_2019 <- merge(white_2019,asian_2019, "GeoID", all = TRUE)
merged_2019 <- merge(merged_2019,black_2019, "GeoID", all = TRUE)
merged_2019 <- merge(merged_2019,hisp_2019, "GeoID", all = TRUE)
merged_2019 <- merge(merged_2019,ind_2019, "GeoID", all = TRUE)
merged_2019 <- merge(merged_2019,Haw_2019, "GeoID", all = TRUE)
merged_2019 <- merge(merged_2019,mix_2019, "GeoID", all = TRUE)
merged_2019 <- merge(merged_2019,oth_2019, "GeoID", all = TRUE)

merged_2020 <- merge(white_2020,asian_2020, "GeoID", all = TRUE)
merged_2020 <- merge(merged_2020,black_2020, "GeoID", all = TRUE)
merged_2020 <- merge(merged_2020,hisp_2020, "GeoID", all = TRUE)
merged_2020 <- merge(merged_2020,ind_2020, "GeoID", all = TRUE)
merged_2020 <- merge(merged_2020,Haw_2020, "GeoID", all = TRUE)
merged_2020 <- merge(merged_2020,mix_2020, "GeoID", all = TRUE)
merged_2020 <- merge(merged_2020,oth_2020, "GeoID", all = TRUE)

merged_2021 <- merge(white_2021,asian_2021, "GeoID", all = TRUE)
merged_2021 <- merge(merged_2021,black_2021, "GeoID", all = TRUE)
merged_2021 <- merge(merged_2021,hisp_2021, "GeoID", all = TRUE)
merged_2021 <- merge(merged_2021,ind_2021, "GeoID", all = TRUE)
merged_2021 <- merge(merged_2021,Haw_2021, "GeoID", all = TRUE)
merged_2021 <- merge(merged_2021,mix_2021, "GeoID", all = TRUE)
merged_2021 <- merge(merged_2021,oth_2021, "GeoID", all = TRUE)

head(merged_2010)

#############################################################################
#Now will add GeoIDs with Subbasin names and merge to get year data sets
# for only needed GeoIDs for each year
#############################################################################
GeoRef_2020 <- read_excel("GeoReference.xlsx", sheet = "2020_2021")
GeoRef_2010 <- read_excel("GeoReference.xlsx", sheet = "2010_2019")

##First need to change GeoID format to match excel reference
# Specify the number of digits to remove (e.g., 2 digits)
library(stringr)
n <- 9

# Remove the first 'n' digits from each number in the column
merged_2010$GeoID <- str_sub(merged_2010$GeoID, start = n + 1)
merged_2011$GeoID <- str_sub(merged_2011$GeoID, start = n + 1)
merged_2012$GeoID <- str_sub(merged_2012$GeoID, start = n + 1)
merged_2013$GeoID <- str_sub(merged_2013$GeoID, start = n + 1)
merged_2014$GeoID <- str_sub(merged_2014$GeoID, start = n + 1)
merged_2015$GeoID <- str_sub(merged_2015$GeoID, start = n + 1)
merged_2016$GeoID <- str_sub(merged_2016$GeoID, start = n + 1)
merged_2017$GeoID <- str_sub(merged_2017$GeoID, start = n + 1)
merged_2018$GeoID <- str_sub(merged_2018$GeoID, start = n + 1)
merged_2019$GeoID <- str_sub(merged_2019$GeoID, start = n + 1)
merged_2020$GeoID <- str_sub(merged_2020$GeoID, start = n + 1)
merged_2021$GeoID <- str_sub(merged_2021$GeoID, start = n + 1)

#Now merging ref data set with all merged ones
merge_2010 <- merge(merged_2010, GeoRef_2010, "GeoID")
merge_2011 <- merge(merged_2011, GeoRef_2010, "GeoID")
merge_2012 <- merge(merged_2012, GeoRef_2010, "GeoID")
merge_2013 <- merge(merged_2013, GeoRef_2010, "GeoID")
merge_2014 <- merge(merged_2014, GeoRef_2010, "GeoID")
merge_2015 <- merge(merged_2015, GeoRef_2010, "GeoID")
merge_2016 <- merge(merged_2016, GeoRef_2010, "GeoID")
merge_2017 <- merge(merged_2017, GeoRef_2010, "GeoID")
merge_2018 <- merge(merged_2018, GeoRef_2010, "GeoID")
merge_2019 <- merge(merged_2019, GeoRef_2010, "GeoID")
merge_2020 <- merge(merged_2020, GeoRef_2020, "GeoID")
merge_2021 <- merge(merged_2021, GeoRef_2020, "GeoID")

#########################################################################
#Now that all data sets have been merged by year and filtered for only
# GeoID's in subbasins of interest, now I will add year and count var's
########################################################################
counts_2010 <- read_excel("EthnicityCounts.xlsx", sheet = "2010") #Loading in count data by year
counts_2011 <- read_excel("EthnicityCounts.xlsx", sheet = "2011")
counts_2012 <- read_excel("EthnicityCounts.xlsx", sheet = "2012")
counts_2013 <- read_excel("EthnicityCounts.xlsx", sheet = "2013")
counts_2014 <- read_excel("EthnicityCounts.xlsx", sheet = "2014")
counts_2015 <- read_excel("EthnicityCounts.xlsx", sheet = "2015")
counts_2016 <- read_excel("EthnicityCounts.xlsx", sheet = "2016")
counts_2017 <- read_excel("EthnicityCounts.xlsx", sheet = "2017")
counts_2018 <- read_excel("EthnicityCounts.xlsx", sheet = "2018")
counts_2019 <- read_excel("EthnicityCounts.xlsx", sheet = "2019")
counts_2020 <- read_excel("EthnicityCounts.xlsx", sheet = "2020")
counts_2021 <- read_excel("EthnicityCounts.xlsx", sheet = "2021")

# Need to rename GEOID to GeoID so I can merge the data sets and take out Subbasin Name column
counts_2010 <- counts_2010 %>% 
  rename(GeoID = "GEOID") %>% 
  select(-"Subbasin Name")
counts_2011 <- counts_2011 %>% 
  rename(GeoID = "GEOID") %>% 
  select(-"Subbasin Name")
counts_2012 <- counts_2012 %>% 
  rename(GeoID = "GEOID") %>% 
  select(-"Subbasin Name")
counts_2013 <- counts_2013 %>% 
  rename(GeoID = "GEOID") %>% 
  select(-"Subbasin Name")
counts_2014 <- counts_2014 %>% 
  rename(GeoID = "GEOID") %>% 
  select(-"Subbasin Name")
counts_2015 <- counts_2015 %>% 
  rename(GeoID = "GEOID") %>% 
  select(-"Subbasin Name")
counts_2016 <- counts_2016 %>% 
  rename(GeoID = "GEOID") %>% 
  select(-"Subbasin Name")
counts_2017 <- counts_2017 %>% 
  rename(GeoID = "GEOID") %>% 
  select(-"Subbasin Name")
counts_2018 <- counts_2018 %>% 
  rename(GeoID = "GEOID") %>% 
  select(-"Subbasin Name")
counts_2019 <- counts_2019 %>% 
  rename(GeoID = "GEOID") %>% 
  select(-"Subbasin Name")
counts_2020 <- counts_2020 %>% 
  rename(GeoID = "GEOID") %>% 
  select(-"Subbasin Name")
counts_2021 <- counts_2021 %>% 
  rename(GeoID = "GEOID") %>% 
  select(-"Subbasin Name")


countinc_2010 <- merge(merge_2010,counts_2010, "GeoID")
countinc_2011 <- merge(merge_2011,counts_2011, "GeoID")
countinc_2012 <- merge(merge_2012,counts_2012, "GeoID")
countinc_2013 <- merge(merge_2013,counts_2013, "GeoID")
countinc_2014 <- merge(merge_2014,counts_2014, "GeoID")
countinc_2015 <- merge(merge_2015,counts_2015, "GeoID")
countinc_2016 <- merge(merge_2016,counts_2016, "GeoID")
countinc_2017 <- merge(merge_2017,counts_2017, "GeoID")
countinc_2018 <- merge(merge_2018,counts_2018, "GeoID")
countinc_2019 <- merge(merge_2019,counts_2019, "GeoID")
countinc_2020 <- merge(merge_2020,counts_2020, "GeoID")
countinc_2021 <- merge(merge_2021,counts_2021, "GeoID")

################################################################################################
#making weights based on total count in each subbasin and calating weight average median income
################################################################################################
weight_avg_2010 <- countinc_2010 %>% 
  mutate(val_white = as.numeric(White_Med_Inc)*as.numeric(White_Count), #this is a more inefficient way to do that, refer to code below
         val_black = as.numeric(Black_Med_Inc)*as.numeric(Black_Count),
         val_asian = as.numeric(Asian_Med_Inc)*as.numeric(Asian_Count),
         val_haw = as.numeric(Haw_Med_Inc)*as.numeric(Haw_Count),
         val_ind = as.numeric(ind_Med_Inc)*as.numeric(Ind_Count),
         val_mix = as.numeric(mix_Med_Inc)*as.numeric(Mix_Count),
         val_oth = as.numeric(oth_Med_Inc)*as.numeric(Oth_Count),
         val_hisp = as.numeric(hisp_Med_Inc)*as.numeric(Hisp_Count)) %>% 
  group_by(Subbasin) %>% 
  summarize(wa_white = val_white/sum(White_Count),
            wa_black = val_black/sum(Black_Count),
            wa_asian = val_asian/sum(Asian_Count),
            wa_haw = val_haw/sum(Haw_Count),
            wa_ind = val_ind/sum(Ind_Count),
            wa_mix = val_mix/sum(Mix_Count),
            wa_oth = val_oth/sum(Oth_Count),
            wa_hisp = val_hisp/sum(Hisp_Count)) %>% 
  summarize(weight_avg_white = sum(wa_white),
            weight_avg_black = sum(wa_black),
            weight_avg_asian = sum(wa_asian),
            weight_avg_haw = sum(wa_haw),
            weight_avg_ind = sum(wa_ind),
            weight_avg_mix = sum(wa_mix),
            weight_avg_oth = sum(wa_oth),
            weight_avg_hisp = sum(wa_hisp))

### ^^^^ I need to fix NA's in Count Income data sets and then rerun code becuase it wont calcuate for sections with -
###############################################################################
#Going with this way below versus above because more efficient
###############################################################################
weight_avg_2010 <- countinc_2010 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), #creating numerators for weight average, multiplying each GEOID med inc by its count by race
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),  #Also telling r to change "-" symbols to NA's so I can take them into account in my equation
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE), #Then taking the numerator and dividing by total pop in each subbasin by race (thus calculating a weighted average)
            weight_avg_black = sum(val_black, na.rm = TRUE) / sum(Black_Count, na.rm = TRUE),   #This method takes into weights of each race in each geoid (white count in geoid/total white count in subbasin), multiplied by race's med income in each geoid, 
            weight_avg_asian = sum(val_asian, na.rm = TRUE) / sum(Asian_Count, na.rm = TRUE),    #then each value for each geoid summed to subbasin level
            weight_avg_haw = sum(val_haw, na.rm = TRUE) / sum(Haw_Count, na.rm = TRUE),
            weight_avg_ind = sum(val_ind, na.rm = TRUE) / sum(Ind_Count, na.rm = TRUE),
            weight_avg_mix = sum(val_mix, na.rm = TRUE) / sum(Mix_Count, na.rm = TRUE),
            weight_avg_oth = sum(val_oth, na.rm = TRUE) / sum(Oth_Count, na.rm = TRUE),
            weight_avg_hisp = sum(val_hisp, na.rm = TRUE) / sum(Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_2011 <- countinc_2011 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_black = sum(val_black, na.rm = TRUE) / sum(Black_Count, na.rm = TRUE),
            weight_avg_asian = sum(val_asian, na.rm = TRUE) / sum(Asian_Count, na.rm = TRUE),
            weight_avg_haw = sum(val_haw, na.rm = TRUE) / sum(Haw_Count, na.rm = TRUE),
            weight_avg_ind = sum(val_ind, na.rm = TRUE) / sum(Ind_Count, na.rm = TRUE),
            weight_avg_mix = sum(val_mix, na.rm = TRUE) / sum(Mix_Count, na.rm = TRUE),
            weight_avg_oth = sum(val_oth, na.rm = TRUE) / sum(Oth_Count, na.rm = TRUE),
            weight_avg_hisp = sum(val_hisp, na.rm = TRUE) / sum(Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_2012 <- countinc_2012 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)),
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_black = sum(val_black, na.rm = TRUE) / sum(Black_Count, na.rm = TRUE),
            weight_avg_asian = sum(val_asian, na.rm = TRUE) / sum(Asian_Count, na.rm = TRUE),
            weight_avg_haw = sum(val_haw, na.rm = TRUE) / sum(Haw_Count, na.rm = TRUE),
            weight_avg_ind = sum(val_ind, na.rm = TRUE) / sum(Ind_Count, na.rm = TRUE),
            weight_avg_mix = sum(val_mix, na.rm = TRUE) / sum(Mix_Count, na.rm = TRUE),
            weight_avg_oth = sum(val_oth, na.rm = TRUE) / sum(Oth_Count, na.rm = TRUE),
            weight_avg_hisp = sum(val_hisp, na.rm = TRUE) / sum(Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_2013 <- countinc_2013 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)),
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_black = sum(val_black, na.rm = TRUE) / sum(Black_Count, na.rm = TRUE),
            weight_avg_asian = sum(val_asian, na.rm = TRUE) / sum(Asian_Count, na.rm = TRUE),
            weight_avg_haw = sum(val_haw, na.rm = TRUE) / sum(Haw_Count, na.rm = TRUE),
            weight_avg_ind = sum(val_ind, na.rm = TRUE) / sum(Ind_Count, na.rm = TRUE),
            weight_avg_mix = sum(val_mix, na.rm = TRUE) / sum(Mix_Count, na.rm = TRUE),
            weight_avg_oth = sum(val_oth, na.rm = TRUE) / sum(Oth_Count, na.rm = TRUE),
            weight_avg_hisp = sum(val_hisp, na.rm = TRUE) / sum(Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_2014 <- countinc_2014 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)),
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_black = sum(val_black, na.rm = TRUE) / sum(Black_Count, na.rm = TRUE),
            weight_avg_asian = sum(val_asian, na.rm = TRUE) / sum(Asian_Count, na.rm = TRUE),
            weight_avg_haw = sum(val_haw, na.rm = TRUE) / sum(Haw_Count, na.rm = TRUE),
            weight_avg_ind = sum(val_ind, na.rm = TRUE) / sum(Ind_Count, na.rm = TRUE),
            weight_avg_mix = sum(val_mix, na.rm = TRUE) / sum(Mix_Count, na.rm = TRUE),
            weight_avg_oth = sum(val_oth, na.rm = TRUE) / sum(Oth_Count, na.rm = TRUE),
            weight_avg_hisp = sum(val_hisp, na.rm = TRUE) / sum(Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_2015 <- countinc_2015 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)),
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_black = sum(val_black, na.rm = TRUE) / sum(Black_Count, na.rm = TRUE),
            weight_avg_asian = sum(val_asian, na.rm = TRUE) / sum(Asian_Count, na.rm = TRUE),
            weight_avg_haw = sum(val_haw, na.rm = TRUE) / sum(Haw_Count, na.rm = TRUE),
            weight_avg_ind = sum(val_ind, na.rm = TRUE) / sum(Ind_Count, na.rm = TRUE),
            weight_avg_mix = sum(val_mix, na.rm = TRUE) / sum(Mix_Count, na.rm = TRUE),
            weight_avg_oth = sum(val_oth, na.rm = TRUE) / sum(Oth_Count, na.rm = TRUE),
            weight_avg_hisp = sum(val_hisp, na.rm = TRUE) / sum(Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_2016 <- countinc_2016 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)),
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_black = sum(val_black, na.rm = TRUE) / sum(Black_Count, na.rm = TRUE),
            weight_avg_asian = sum(val_asian, na.rm = TRUE) / sum(Asian_Count, na.rm = TRUE),
            weight_avg_haw = sum(val_haw, na.rm = TRUE) / sum(Haw_Count, na.rm = TRUE),
            weight_avg_ind = sum(val_ind, na.rm = TRUE) / sum(Ind_Count, na.rm = TRUE),
            weight_avg_mix = sum(val_mix, na.rm = TRUE) / sum(Mix_Count, na.rm = TRUE),
            weight_avg_oth = sum(val_oth, na.rm = TRUE) / sum(Oth_Count, na.rm = TRUE),
            weight_avg_hisp = sum(val_hisp, na.rm = TRUE) / sum(Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_2017 <- countinc_2017 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)),
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_black = sum(val_black, na.rm = TRUE) / sum(Black_Count, na.rm = TRUE),
            weight_avg_asian = sum(val_asian, na.rm = TRUE) / sum(Asian_Count, na.rm = TRUE),
            weight_avg_haw = sum(val_haw, na.rm = TRUE) / sum(Haw_Count, na.rm = TRUE),
            weight_avg_ind = sum(val_ind, na.rm = TRUE) / sum(Ind_Count, na.rm = TRUE),
            weight_avg_mix = sum(val_mix, na.rm = TRUE) / sum(Mix_Count, na.rm = TRUE),
            weight_avg_oth = sum(val_oth, na.rm = TRUE) / sum(Oth_Count, na.rm = TRUE),
            weight_avg_hisp = sum(val_hisp, na.rm = TRUE) / sum(Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_2018 <- countinc_2018 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)),
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_black = sum(val_black, na.rm = TRUE) / sum(Black_Count, na.rm = TRUE),
            weight_avg_asian = sum(val_asian, na.rm = TRUE) / sum(Asian_Count, na.rm = TRUE),
            weight_avg_haw = sum(val_haw, na.rm = TRUE) / sum(Haw_Count, na.rm = TRUE),
            weight_avg_ind = sum(val_ind, na.rm = TRUE) / sum(Ind_Count, na.rm = TRUE),
            weight_avg_mix = sum(val_mix, na.rm = TRUE) / sum(Mix_Count, na.rm = TRUE),
            weight_avg_oth = sum(val_oth, na.rm = TRUE) / sum(Oth_Count, na.rm = TRUE),
            weight_avg_hisp = sum(val_hisp, na.rm = TRUE) / sum(Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_2019 <- countinc_2019 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)),
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_black = sum(val_black, na.rm = TRUE) / sum(Black_Count, na.rm = TRUE),
            weight_avg_asian = sum(val_asian, na.rm = TRUE) / sum(Asian_Count, na.rm = TRUE),
            weight_avg_haw = sum(val_haw, na.rm = TRUE) / sum(Haw_Count, na.rm = TRUE),
            weight_avg_ind = sum(val_ind, na.rm = TRUE) / sum(Ind_Count, na.rm = TRUE),
            weight_avg_mix = sum(val_mix, na.rm = TRUE) / sum(Mix_Count, na.rm = TRUE),
            weight_avg_oth = sum(val_oth, na.rm = TRUE) / sum(Oth_Count, na.rm = TRUE),
            weight_avg_hisp = sum(val_hisp, na.rm = TRUE) / sum(Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_2020 <- countinc_2020 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)),
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_black = sum(val_black, na.rm = TRUE) / sum(Black_Count, na.rm = TRUE),
            weight_avg_asian = sum(val_asian, na.rm = TRUE) / sum(Asian_Count, na.rm = TRUE),
            weight_avg_haw = sum(val_haw, na.rm = TRUE) / sum(Haw_Count, na.rm = TRUE),
            weight_avg_ind = sum(val_ind, na.rm = TRUE) / sum(Ind_Count, na.rm = TRUE),
            weight_avg_mix = sum(val_mix, na.rm = TRUE) / sum(Mix_Count, na.rm = TRUE),
            weight_avg_oth = sum(val_oth, na.rm = TRUE) / sum(Oth_Count, na.rm = TRUE),
            weight_avg_hisp = sum(val_hisp, na.rm = TRUE) / sum(Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_2021 <- countinc_2021 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)),
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_black = sum(val_black, na.rm = TRUE) / sum(Black_Count, na.rm = TRUE),
            weight_avg_asian = sum(val_asian, na.rm = TRUE) / sum(Asian_Count, na.rm = TRUE),
            weight_avg_haw = sum(val_haw, na.rm = TRUE) / sum(Haw_Count, na.rm = TRUE),
            weight_avg_ind = sum(val_ind, na.rm = TRUE) / sum(Ind_Count, na.rm = TRUE),
            weight_avg_mix = sum(val_mix, na.rm = TRUE) / sum(Mix_Count, na.rm = TRUE),
            weight_avg_oth = sum(val_oth, na.rm = TRUE) / sum(Oth_Count, na.rm = TRUE),
            weight_avg_hisp = sum(val_hisp, na.rm = TRUE) / sum(Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

###############################################################################
#Now comparing white med inc with all other races
###############################################################################
weight_avg_wo_2010 <- countinc_2010 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_asian, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Asian_Count,  
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wo_2011 <- countinc_2011 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_asian, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Asian_Count,  
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wo_2012 <- countinc_2012 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_asian, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Asian_Count,  
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wo_2013 <- countinc_2013 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_asian, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Asian_Count,  
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wo_2014 <- countinc_2014 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_asian, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Asian_Count,  
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wo_2015 <- countinc_2015 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_asian, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Asian_Count,  
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wo_2016 <- countinc_2016 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_asian, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Asian_Count,  
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wo_2017 <- countinc_2017 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_asian, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Asian_Count,  
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wo_2018 <- countinc_2018 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_asian, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Asian_Count,  
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wo_2019 <- countinc_2019 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_asian, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Asian_Count,  
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wo_2020 <- countinc_2020 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_asian, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Asian_Count,  
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wo_2021 <- countinc_2021 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white = sum(val_white, na.rm = TRUE) / sum(White_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_asian, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Asian_Count,  
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

##########################################################################################
#Now making tables comparing weighted average med inc of white + asian compared to others
##########################################################################################
weight_avg_wao_2010 <- countinc_2010 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white_asian = sum(val_white, val_asian, na.rm = TRUE) / sum(White_Count, Asian_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wao_2011 <- countinc_2011 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white_asian = sum(val_white, val_asian, na.rm = TRUE) / sum(White_Count, Asian_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wao_2012 <- countinc_2012 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white_asian = sum(val_white, val_asian, na.rm = TRUE) / sum(White_Count, Asian_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wao_2013 <- countinc_2013 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white_asian = sum(val_white, val_asian, na.rm = TRUE) / sum(White_Count, Asian_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wao_2014 <- countinc_2014 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white_asian = sum(val_white, val_asian, na.rm = TRUE) / sum(White_Count, Asian_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wao_2015 <- countinc_2015 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white_asian = sum(val_white, val_asian, na.rm = TRUE) / sum(White_Count, Asian_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wao_2016 <- countinc_2016 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white_asian = sum(val_white, val_asian, na.rm = TRUE) / sum(White_Count, Asian_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wao_2017 <- countinc_2017 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white_asian = sum(val_white, val_asian, na.rm = TRUE) / sum(White_Count, Asian_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wao_2018 <- countinc_2018 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white_asian = sum(val_white, val_asian, na.rm = TRUE) / sum(White_Count, Asian_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wao_2019 <- countinc_2019                                                                                                                                              %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white_asian = sum(val_white, val_asian, na.rm = TRUE) / sum(White_Count, Asian_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wao_2020 <- countinc_2020 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white_asian = sum(val_white, val_asian, na.rm = TRUE) / sum(White_Count, Asian_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

weight_avg_wao_2021 <- countinc_2021 %>%
  mutate(val_white = ifelse(White_Med_Inc == "-", NA, as.numeric(White_Med_Inc) * as.numeric(White_Count)), 
         val_black = ifelse(Black_Med_Inc == "-", NA, as.numeric(Black_Med_Inc) * as.numeric(Black_Count)),   
         val_asian = ifelse(Asian_Med_Inc == "-", NA, as.numeric(Asian_Med_Inc) * as.numeric(Asian_Count)),
         val_haw = ifelse(Haw_Med_Inc == "-", NA, as.numeric(Haw_Med_Inc) * as.numeric(Haw_Count)),
         val_ind = ifelse(ind_Med_Inc == "-", NA, as.numeric(ind_Med_Inc) * as.numeric(Ind_Count)),
         val_mix = ifelse(mix_Med_Inc == "-", NA, as.numeric(mix_Med_Inc) * as.numeric(Mix_Count)),
         val_oth = ifelse(oth_Med_Inc == "-", NA, as.numeric(oth_Med_Inc) * as.numeric(Oth_Count)),
         val_hisp = ifelse(hisp_Med_Inc == "-", NA, as.numeric(hisp_Med_Inc) * as.numeric(Hisp_Count))) %>%
  group_by(Subbasin) %>%
  summarize(weight_avg_white_asian = sum(val_white, val_asian, na.rm = TRUE) / sum(White_Count, Asian_Count, na.rm = TRUE),
            weight_avg_other = sum(val_black, 
                                   val_haw, 
                                   val_ind, 
                                   val_mix, 
                                   val_oth, 
                                   val_hisp, na.rm = TRUE) / sum(Black_Count, 
                                                                 Haw_Count, 
                                                                 Ind_Count, 
                                                                 Mix_Count, 
                                                                 Oth_Count, 
                                                                 Hisp_Count, na.rm = TRUE)) #Note: NaN where counts are zero

###############################################################################
#Visualizing Tables - White & other, and White+asian & other
###############################################################################
library(openxlsx)
table_wo_2010 <- write.xlsx(weight_avg_wo_2010, file = "WA_Comparison_WhitevsOther2010.xlsx")
table_wo_2011 <- write.xlsx(weight_avg_wo_2011, file = "WA_Comparison_WhitevsOther2011.xlsx")
table_wo_2012 <- write.xlsx(weight_avg_wo_2012, file = "WA_Comparison_WhitevsOther2012.xlsx")
table_wo_2013 <- write.xlsx(weight_avg_wo_2013, file = "WA_Comparison_WhitevsOther2013.xlsx")
table_wo_2014 <- write.xlsx(weight_avg_wo_2014, file = "WA_Comparison_WhitevsOther2014.xlsx")
table_wo_2015 <- write.xlsx(weight_avg_wo_2015, file = "WA_Comparison_WhitevsOther2015.xlsx")
table_wo_2016 <- write.xlsx(weight_avg_wo_2016, file = "WA_Comparison_WhitevsOther2016.xlsx")
table_wo_2017 <- write.xlsx(weight_avg_wo_2017, file = "WA_Comparison_WhitevsOther2017.xlsx")
table_wo_2018 <- write.xlsx(weight_avg_wo_2018, file = "WA_Comparison_WhitevsOther2018.xlsx")
table_wo_2019 <- write.xlsx(weight_avg_wo_2019, file = "WA_Comparison_WhitevsOther2019.xlsx")
table_wo_2020 <- write.xlsx(weight_avg_wo_2020, file = "WA_Comparison_WhitevsOther2020.xlsx")
table_wo_2021 <- write.xlsx(weight_avg_wo_2021, file = "WA_Comparison_WhitevsOther2021.xlsx")

table_wao_2010 <- write.xlsx(weight_avg_wao_2010, file = "WA_Comparison_WhiteAsianvsOther2010.xlsx")
table_wao_2011 <- write.xlsx(weight_avg_wao_2011, file = "WA_Comparison_WhiteAsianvsOther2011.xlsx")
table_wao_2012 <- write.xlsx(weight_avg_wao_2012, file = "WA_Comparison_WhiteAsianvsOther2012.xlsx")
table_wao_2013 <- write.xlsx(weight_avg_wao_2013, file = "WA_Comparison_WhiteAsianvsOther2013.xlsx")
table_wao_2014 <- write.xlsx(weight_avg_wao_2014, file = "WA_Comparison_WhiteAsianvsOther2014.xlsx")
table_wao_2015 <- write.xlsx(weight_avg_wao_2015, file = "WA_Comparison_WhiteAsianvsOther2015.xlsx")
table_wao_2016 <- write.xlsx(weight_avg_wao_2016, file = "WA_Comparison_WhiteAsianvsOther2016.xlsx")
table_wao_2017 <- write.xlsx(weight_avg_wao_2017, file = "WA_Comparison_WhiteAsianvsOther2017.xlsx")
table_wao_2018 <- write.xlsx(weight_avg_wao_2018, file = "WA_Comparison_WhiteAsianvsOther2018.xlsx")
table_wao_2019 <- write.xlsx(weight_avg_wao_2019, file = "WA_Comparison_WhiteAsianvsOther2019.xlsx")
table_wao_2020 <- write.xlsx(weight_avg_wao_2020, file = "WA_Comparison_WhiteAsianvsOther2020.xlsx")
table_wao_2021 <- write.xlsx(weight_avg_wao_2021, file = "WA_Comparison_WhiteAsianvsOther2021.xlsx")













# ---- Gini Coefficients ----
#first stacking weighted average variables for each ethnicity
#2010
weight_avg_2010 <- weight_avg_2010 %>% 
  gather(variable, value, -Subbasin)
#sorting subbasin column alphabetically
weight_avg_2010 <- weight_avg_2010 %>% 
  arrange(Subbasin)
#gini calculating for each subbasin
gini_2010 <- weight_avg_2010 %>% 
  filter(value != 0) %>%  # Remove rows with zero values
  group_by(Subbasin) %>% 
  summarize(gini_med_inc_2010 = ineq(value, type = "Gini", na.rm = TRUE))

#2015
weight_avg_2015 <- weight_avg_2015 %>% 
  gather(variable, value, -Subbasin)
#sorting subbasin column alphabetically
weight_avg_2015 <- weight_avg_2015 %>% 
  arrange(Subbasin)
#gini calculating for each subbasin
gini_2015 <- weight_avg_2015 %>% 
  filter(value != 0) %>%  # Remove rows with zero values
  group_by(Subbasin) %>% 
  summarize(gini_med_inc_2015 = ineq(value, type = "Gini", na.rm = TRUE))

#2021
weight_avg_2021 <- weight_avg_2021 %>% 
  gather(variable, value, -Subbasin)
#sorting subbasin column alphabetically
weight_avg_2021 <- weight_avg_2021 %>% 
  arrange(Subbasin)
#gini calculating for each subbasin
gini_2021 <- weight_avg_2021 %>% 
  filter(value != 0) %>%  # Remove rows with zero values
  group_by(Subbasin) %>% 
  summarize(gini_med_inc_2021 = ineq(value, type = "Gini", na.rm = TRUE))

#merging
gini_compare <- merge(gini_2010,gini_2015,by="Subbasin")
gini_compare <- merge(gini_compare,gini_2021,by="Subbasin")

#exporting table
write.xlsx(gini_compare, file = "gini_compare.xlsx")


# ---- Chai Squared test to see statistical difference in means ----
#for white+asian vs all other races
#first adding year variable to each 
weight_avg_wao_2010 <- weight_avg_wao_2010 %>% mutate(Year = 2010) %>% 
  select(Subbasin,Year,everything())
weight_avg_wao_2011 <- weight_avg_wao_2011 %>% mutate(Year = 2011) %>% 
  select(Subbasin,Year,everything())
weight_avg_wao_2012 <- weight_avg_wao_2012 %>% mutate(Year = 2012) %>% 
  select(Subbasin,Year,everything())
weight_avg_wao_2013 <- weight_avg_wao_2013 %>% mutate(Year = 2013) %>% 
  select(Subbasin,Year,everything())
weight_avg_wao_2014 <- weight_avg_wao_2014 %>% mutate(Year = 2014) %>% 
  select(Subbasin,Year,everything())
weight_avg_wao_2015 <- weight_avg_wao_2015 %>% mutate(Year = 2015) %>% 
  select(Subbasin,Year,everything())
weight_avg_wao_2016 <- weight_avg_wao_2016 %>% mutate(Year = 2016) %>% 
  select(Subbasin,Year,everything())
weight_avg_wao_2017 <- weight_avg_wao_2017 %>% mutate(Year = 2017) %>% 
  select(Subbasin,Year,everything())
weight_avg_wao_2018 <- weight_avg_wao_2018 %>% mutate(Year = 2018) %>% 
  select(Subbasin,Year,everything())
weight_avg_wao_2019 <- weight_avg_wao_2019 %>% mutate(Year = 2019) %>% 
  select(Subbasin,Year,everything())
weight_avg_wao_2020 <- weight_avg_wao_2020 %>% mutate(Year = 2020) %>% 
  select(Subbasin,Year,everything())
weight_avg_wao_2021 <- weight_avg_wao_2021 %>% mutate(Year = 2021) %>% 
  select(Subbasin,Year,everything())

#combining
wa_income <- rbind(weight_avg_wao_2010,weight_avg_wao_2011)
wa_income <- rbind(wa_income,weight_avg_wao_2012)
wa_income <- rbind(wa_income,weight_avg_wao_2013)
wa_income <- rbind(wa_income,weight_avg_wao_2014)
wa_income <- rbind(wa_income,weight_avg_wao_2015)
wa_income <- rbind(wa_income,weight_avg_wao_2016)
wa_income <- rbind(wa_income,weight_avg_wao_2017)
wa_income <- rbind(wa_income,weight_avg_wao_2018)
wa_income <- rbind(wa_income,weight_avg_wao_2019)
wa_income <- rbind(wa_income,weight_avg_wao_2020)
wa_income <- rbind(wa_income,weight_avg_wao_2021)

#running ttest to see differences
t_test_results <- wa_income %>%
  group_by(Subbasin) %>%
  summarize(p_value = t.test(weight_avg_white_asian, weight_avg_other, 
                             paired = TRUE)$p.value)

ttest_ethnicity_inc <- write.xlsx(t_test_results,file = "ttest_ethnicity_inc.xlsx")
















