# extract ACIS PRISM data for AZ study area
# MAC 05/12/23
# Last updated: 6/27/24
# info on the RCC-ACIS webservice can be found here https://www.rcc-acis.org/docs_webservices.html

# Code used for calculating three rounds of subbasin additions - OG, 1-13, and 1-30/2-11 additions

# load necessary packages
library(RCurl)
library(jsonlite)
library(raster)    
library(rgdal)
library(httr)
library(jsonlite)
library(prism)
library(reshape2)
library(dplyr)
library(lubridate)
library(sf)

#####
# read in shapefile
#studyAreas<-readOGR("C:/Users/zoeys/Documents/Data/Weather/NewSB_Shape_1-13.shp")
studyAreas<- sf::st_read(dsn = "C:/Users/zoeys/Documents/WEP-2024/Code/Climate/Study_Area/NewSBs_130_MergeBut_Dissolve.shp") # read shapefile in as sf object
studyAreas<- sf::st_transform(studyAreas, crs = sf::st_crs("+proj=longlat +datum=WGS84")) # convert to lat/lon projection
studyAreas<- as(studyAreas, "Spatial") # convert back to sp object
#####

#####
# set parameters for downloading PRISM data from RCC-ACIS

# Set date ranges - monthly data from 1895-present are available, download 1991-present to calculate 1991-2020 normals period 
dateRangeStart="2009-01-01"
dateRangeEnd= "2022-12-31"

# generate date sequence -- keep with PRISM date
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),by="month")

# get bounding box from shapefile extent
bbox<-extent(studyAreas)
  ACISbbox<-paste0(bbox@xmin,",",bbox@ymin,",",bbox@xmax,",",bbox@ymax)

################################################################################ 
# ACIS query - change elems to variable of interest
#jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":"mly_pcpn","meta":"ll,elev","output":"json"}') # or uid
jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":[{"name":"mly_pcpn"},{"name":"mly_avgt"}],"meta":"ll,elev","output":"json"}') # or uid


out<-postForm("http://data.rcc-acis.org/GridData",
              .opts = list(postfields = jsonQuery,
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))

out<-fromJSON(out)
################################################################################

#### process precip data from out ####
# convert to list of matrices, flipud with PRISM
matrixList <- vector("list",length(out$data))
for(i in 1:length(out$data)){
  matrixList[[i]]<-apply(t(out$data[[i]][[2]]),1,rev)
}
# read into raster stack
rasterList<-lapply(matrixList, raster)
gridStack<-stack(rasterList)
gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
gridStack<-setExtent(gridStack, gridExtent, keepres=FALSE, snap=FALSE)
names(gridStack)<-allDates
# set 0 and neg to NA
gridStack[gridStack < 0] <- NA
#####

#### process temperature data from out #####
matrixList <- vector("list",length(out$data))
for(i in 1:length(out$data)){
  matrixList[[i]]<-apply(t(out$data[[i]][[3]]),1,rev)
}

# read into raster stack
rasterList<-lapply(matrixList, raster)
tempStack<-stack(rasterList)
gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
tempStack<-setExtent(tempStack, gridExtent, keepres=FALSE, snap=FALSE)
names(tempStack)<-allDates
# set 0 and neg to NA
tempStack[tempStack <= -999] <- NA
#####

#####
# plot some precip data - most recent month
plot(gridStack[[nlayers(gridStack)]])
plot(studyAreas, add=TRUE)
# plot some temp data - most recent month
plot(tempStack[[nlayers(gridStack)]])
plot(studyAreas, add=TRUE)

#####

#####
# save raster stack to tif for use in ArcGIS
#proj4string(gridStack) <- CRS("+init=epsg:4326") # set projection for ArcGIS
#writeRaster(gridStack, file="TO_PRISM_monthly_precip_1895_2022.tif")
#####

#####
# extract a monthly precip time series using study area boundary
moPrecip <- t(extract(gridStack, studyAreas, fun='mean', na.rm=TRUE, df=TRUE, weights = FALSE))
moPrecip <- moPrecip[2:nrow(moPrecip),] # drop that first ID row
moPrecip <- cbind.data.frame(allDates, moPrecip)
#colnames(moPrecip)<-c("date",as.character(studyAreas$Area_Name[1:5]),as.character(studyAreas$SUBBASIN_N[6:11])) # tried to add some names to the columns
#colnames(moPrecip)[10]<-"DOUGLAS2"
# extract a monthly temp time series using study area boundary
moTemp <- t(extract(tempStack, studyAreas, fun='mean', na.rm=TRUE, df=TRUE, weights = FALSE))
moTemp <- moTemp[2:nrow(moTemp),] # drop that first ID row
moTemp <- cbind.data.frame(allDates, moTemp)
#colnames(moTemp)<-c("date",as.character(studyAreas$Area_Name[1:5]),as.character(studyAreas$SUBBASIN_N[6:11])) # tried to add some names to the columns
#colnames(moTemp)[10]<-"DOUGLAS2"
#####

#####
# plot a precip time series
precipt<-tidyr::gather(moPrecip, area, precip, 2:2) #need to change "2:4" depending on how many columns in "moPrecip"
library(ggplot2)
ggplot(precipt, aes(allDates,precip, color=area))+
  geom_line()+
  ylab("Precip (in)")+
  xlab("Date")+
  ggtitle("Monthly average precip for study area sites - PRISM 2009-2022")
# plot a temp time series
tempr<-tidyr::gather(moTemp, area, temp, 2:2) #need to change "2:4" depending on how many columns in "tempr"
library(ggplot2)
ggplot(tempr, aes(allDates,temp, color=area))+
  geom_line()+
  ylab("Temp (F)")+
  xlab("Date")+
  ggtitle("Monthly average temps for study area sites - PRISM 2009-2022")


#####

##Zoey's additions:
#Going to export tempr and precipt to excel to keep only the dates that I want and average them by year
library(openxlsx)
write.xlsx(precipt, "C:/Users/zoeys/Documents/Thesis/Weather/precipt_monthly_BMR_2-11.xlsx")
write.xlsx(tempr, "C:/Users/zoeys/Documents/Thesis/Weather/tempr_monthly_BMR_2-11.xlsx")

##############################################################################
#creating yearly average by subbasin




#Importing yearly average temp and precip values by subbasin from excel
#library(readxl)
#Precip_data <- read.xlsx("C:/Users/zoeys/Documents/Data/Weather/precipt_excel.xlsx", sheet = "YearlyAvgPrecip")
#Temp_data <- read.xlsx("C:/Users/zoeys/Documents/Data/Weather/tempr_excel.xlsx", sheet = "YearlyAvgTemp")

#Now coding to visualize with ggplot
#library(ggplot2)
#ggplot(Temp_data, aes(Year,Temp, color=Subbasin))+
#  geom_line()+
#  ylab("Temp (F)")+
#  xlab("Year")+
#  ggtitle("Yearly Average Temps for Study Areas - PRISM 2010-2021")

#ggplot(Precip_data, aes(Year,Precip, color=Subbasin))+
#  geom_line()+
#  ylab("Precip (in.)")+
#  xlab("Year")+
#  ggtitle("Yearly Average Precips for Study Areas - PRISM 2010-2021")



