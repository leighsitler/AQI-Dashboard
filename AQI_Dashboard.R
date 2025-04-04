install.packages("rsconnect")

library(leaflet)
library(dplyr)
library(tidyverse)
library(tidyr)
library(arcgis)
library(arcgisbinding)
library(scales)
library(grDevices)
library(shiny)
library(shinythemes)
library(leaflet.esri)
library(shinyWidgets)
library(shinydashboard)
library(lubridate)
library(flexdashboard)
library(plotly)
library(ggplot2)
library(ggrepel)
library(DT)
library(lattice)
library(sf)
library(con2aqi)
library(ggnewscale)
library(plyr)
library(gridExtra)
library(RAQSAPI)
library(grid)
library(shinyBS)
library(tableHTML)
library(rsconnect)

rsconnect::writeManifest()

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
arc.check_product()
RAQSAPI::aqs_credentials(username = "leigh.sitler@phila.gov", key = "khakigazelle25")

#### MAP & CURRENT MEASUREMENTS INPUT DATA -----
AirQualityData <-  arc.data2sf(
  arc.select(
    arc.open("https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/LATEST_CORE_SITE_READINGS/FeatureServer/0")))

AirQualityData %>% 
  select(SITE_NAME,CARBON_MONOXIDE_PPM, NITROGEN_DIOXIDE_PPM, OZONE_PPM, PM10_UG_M3, PM25_UG_M3, SULFUR_DIOXIDE_PPB) -> AirDataSmall

Neighborhoods <- arc.data2sf(
  arc.select(
    arc.open("https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/Philadelphia_Neighborhoods_Keep/FeatureServer/0")))

CityLimits <- arc.data2sf(
  arc.select(
    arc.open("https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/City_Limits/FeatureServer/0")))

Transformed <- sf::st_transform(AirDataSmall, 4326)
CityLimits = sf::st_transform(CityLimits, 4326)
Neighborhoods = sf::st_transform(Neighborhoods, 4326)

Transformed$Time <- as_datetime(AirQualityData$SAMPLE_TIMESTAMP)
Transformed$Time <- as_datetime(as.POSIXct(Transformed$Time))
Timestamp <- Transformed$Time[[1]]
Timestamp <- format(Timestamp, "%b %d, %Y, %I:%M%p") 

risklevels <-  c('Good', 'Moderate', 'Unhealthy for Sensitive Groups', 'Unhealthy', 'Very Unhealthy')
colors <- c('#999933', '#DDCC77', '#CC6677', '#882255', '#330033')

############ CARBON MONOXIDE daily data ----
CarbMono <- Transformed %>% 
  select(1,2,8) %>% 
  subset(!is.na(CARBON_MONOXIDE_PPM)) 

COOutput <- AirQualityData %>% 
  select(2,5,6,12) %>% 
  subset(!is.na(CARBON_MONOXIDE_PPM)) %>% 
  setNames(c("Site", "Address", "PPM", "Timestamp")) 
COOutput$Address <- str_to_title(COOutput$Address)
COOutput$Timestamp <- format(COOutput$Timestamp, "%m/%d/%Y %H:%M")
COOutput <- as.data.frame(COOutput)[,-ncol(COOutput)]

MeanCO <- mean(CarbMono$CARBON_MONOXIDE_PPM)
MeanCOround <- round(MeanCO, digits = 2)
CO_NAAQS <- 9.0

CO_riskvalues <- c('0 - 4.4 ppm', '4.5 - 9.4 ppm', '9.5 - 12.4 ppm', '12.5 - 15.4 ppm', '15.5 - 30.4 ppm')
CO_breaks <- c(0,4.4,9.4,12.4,15.4,30.4)
CO_max <- 30.4

palCO <- colorNumeric(palette = c("#999933", "#882255"), domain = CarbMono$CARBON_MONOXIDE_PPM)

############ NITROGEN DIOXIDE daily data ----
NO2dat <- Transformed %>% 
  select(1,3,8) %>% 
  subset(!is.na(NITROGEN_DIOXIDE_PPM)) 

NO2Output <- AirQualityData %>% 
  select(2,5,7,12) %>% 
  subset(!is.na(NITROGEN_DIOXIDE_PPM)) %>% 
  setNames(c("Site", "Address", "PPB", "Timestamp")) 
NO2Output$Address <- str_to_title(NO2Output$Address)
NO2Output$Timestamp <- format(NO2Output$Timestamp, "%m/%d/%Y %H:%M")
NO2Output <- as.data.frame(NO2Output)[,-ncol(NO2Output)]

MeanNO2 <- mean(NO2dat$NITROGEN_DIOXIDE_PPM)
MeanNO2round <- round(MeanNO2, digits = 2)
NO2_riskvalues <- c('0 - 53 ppb', '54 - 100 ppb', '101 - 360 ppb', '361 - 644 ppb', '645 - 1244 ppb')
NO2_breaks <- c(0,53,100,360,644,1244)
NO2_max <- 1244

palNO2 <- colorNumeric(palette = c("#999933", "#882255"), domain = NO2dat$NITROGEN_DIOXIDE_PPM)


############ OZONE daily data ----
Ozone <- Transformed %>% 
  select(1,4,8) %>% 
  subset(!is.na(OZONE_PPM)) 
Ozone$OZONE_PPM <- Ozone$OZONE_PPM/1000
Ozone[Ozone$OZONE_PPM<0,c("OZONE_PPM")]<-NA
MeanOzoneOrig <- mean(Ozone$OZONE_PPM, na.rm=T)
MeanOzone <- MeanOzoneOrig*100
Ozone_riskvalues <- c('0 - 0.059 ppm', '0.060 - 0.075 ppm', '0.076 - 0.095 ppm', '0.096 - 0.115 ppm', '0.116 - 0.374 ppm')
Ozone_breaks_orig <- c(0,0.059, 0.075, 0.095, 0.115, 0.274)
Ozone_breaks_labels <- c(0,0.059, 0.075, 0.095, 0.115, 0.374)
Ozone_breaks <- Ozone_breaks_orig*100
Ozone_max_orig <- 0.374
Ozone_max <- Ozone_max_orig*100-10
MeanOzoneroundOrig <- round(MeanOzoneOrig, digits = 3)
MeanOzoneround <- MeanOzoneroundOrig*100

OzoneOutput <- AirQualityData %>% 
  select(2,5,8,12) %>% 
  subset(!is.na(OZONE_PPM)) %>% 
  setNames(c("Site", "Address", "PPM", "Timestamp")) 
OzoneOutput$PPM <- OzoneOutput$PPM/1000
OzoneOutput[OzoneOutput$PPM<0,c("PPM")]<-NA
OzoneOutput$Address <- str_to_title(OzoneOutput$Address)
OzoneOutput$Address <- gsub(",.*$", "", OzoneOutput$Address)
OzoneOutput$Timestamp <- format(OzoneOutput$Timestamp, "%m/%d/%Y %H:%M")
OzoneOutput <- as.data.frame(OzoneOutput)[,-ncol(OzoneOutput)]

palOzone <- colorNumeric(palette = c("#999933", "#882255"), domain = Ozone$OZONE_PPM)

############ SULFUR DIOXIDE Daily data -----
SO2dat <- Transformed %>% 
  select(1,7,8) %>% 
  subset(!is.na(SULFUR_DIOXIDE_PPB)) 

MeanSO2 <- mean(SO2dat$SULFUR_DIOXIDE_PPB)
MeanSO2 <- mean(SO2dat$SULFUR_DIOXIDE_PPB)*100
SO2_breaks <- c(0,35,75,185,304,604)
SO2_riskvalues <- c('0 - 35 ppb', '36 - 75 ppb', '76 - 185 ppb', '186 - 304 ppb', '305 - 604 ppb')
So2_max <- 604
MeanSO2round <- round(MeanSO2, digits = 2)

SO2Output <- AirQualityData %>% 
  select(2,5,11,12) %>% 
  subset(!is.na(SULFUR_DIOXIDE_PPB)) %>% 
  setNames(c("Site", "Address", "PPB", "Timestamp")) 
SO2Output$Address <- str_to_title(SO2Output$Address)
SO2Output$Address <- gsub(",.*$", "", SO2Output$Address)
SO2Output$Timestamp <- format(SO2Output$Timestamp, "%m/%d/%Y %H:%M")
SO2Output <- as.data.frame(SO2Output)[,-ncol(SO2Output)]

palSO2 <- colorNumeric(palette = c("#999933", "#882255"), domain = SO2dat$SULFUR_DIOXIDE_PPB)


############ PM2.5 daily data ----
PM25dat <- Transformed %>% 
  select(1,6,8) %>% 
  subset(!is.na(PM25_UG_M3)) 

MeanPM25 <- mean(PM25dat$PM25_UG_M3)
PM25_breaks <- c(0,12, 35.4, 55.4, 150.4, 250.4)
PM25_riskvalues <- c('0 - 12 µg/m³', '13 - 35.4 µg/m³', '35.5 - 55.4 µg/m³', '55.5 - 150.4 µg/m³', '150.5 - 250.4 µg/m³')
PM25_max <- 250.4
MeanPM25round <- round(MeanPM25, digits = 2)

PM25Output <- AirQualityData %>% 
  select(2,5,10,12) %>% 
  subset(!is.na(PM25_UG_M3)) %>% 
  setNames(c("Site", "Address", "µg/m³", "Timestamp")) 
PM25Output$Address <- str_to_title(PM25Output$Address)
PM25Output$Address <- gsub(",.*$", "", PM25Output$Address)
PM25Output$Timestamp <- format(PM25Output$Timestamp, "%m/%d/%Y %H:%M")
PM25Output <- as.data.frame(PM25Output)[,-ncol(PM25Output)]

palPM25 <- colorNumeric(palette = c("#999933", "#882255"), domain = PM25dat$PM25_UG_M3)


############ PM10 daily data -----
PM10dat <- Transformed %>% 
  select(1,5,8) %>% 
  subset(!is.na(PM10_UG_M3)) 

MeanPM10 <- mean(PM10dat$PM10_UG_M3)
PM10_breaks <- c(0, 54, 154, 254, 354, 424)
PM10_riskvalues <- c('0 - 54 µg/m³', '55 - 154 µg/m³', '155 - 254 µg/m³', '255 - 354 µg/m³', '355 - 424 µg/m³')
PM10_max <- 424
MeanPM10round <- round(MeanPM10, digits = 2)

PM10Output <- AirQualityData %>% 
  select(2,5,9,12) %>% 
  subset(!is.na(PM10_UG_M3)) %>% 
  setNames(c("Site", "Address", "µg/m³", "Timestamp")) 
PM10Output$Address <- str_to_title(PM10Output$Address)
PM10Output$Address <- gsub(",.*$", "", PM10Output$Address)
PM10Output$Timestamp <- format(PM10Output$Timestamp, "%m/%d/%Y %H:%M")
PM10Output <- as.data.frame(PM10Output)[,-ncol(PM10Output)]


palPM10 <- colorNumeric(palette = c("#999933", "#882255"), domain = PM10dat$PM10_UG_M3)

#### AQI ----

CO_AQI <- as.numeric(con2aqi::con2aqi("co", MeanCO))
NO2_AQI <- con2aqi::con2aqi("no2", MeanNO2)
O3_AQI <- con2aqi::con2aqi("o3", MeanOzoneOrig, "8h")
SO2_AQI <- con2aqi::con2aqi("so2", MeanSO2)
PM25_AQI <- con2aqi::con2aqi("pm25", MeanPM25)
PM10_AQI <- con2aqi::con2aqi("pm10", MeanPM10)

AQI <- data.frame(
  Risk = c('Good', 'Moderate', 'Unhealthy for Sensitive Groups', 'Unhealthy', 'Very Unhealthy'),
  minvalue = c(0,51, 101, 151, 201),  
  maxvalue = c(50, 100, 150, 200, 300))
color <- c('Good' ='green',
           'Moderate'='yellow', 
           'Unhealthy for Sensitive Groups'='orange', 
           'Unhealthy'='red', 
           'Very Unhealthy'='#8f3f97')
AQI <- AQI %>% 
  setNames(c("RiskLevel", "minvalue", "maxvalue")) 

for(i in unique(AirQualityData$SITE_NAME)) {
  nam <- paste("AirQualityData", i, sep = "_")
  assign(nam, AirQualityData[AirQualityData$SITE_NAME==i,])
}

###### Site = NEW ----
AirQualityData_NEW$CO_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "co", con = AirQualityData_NEW$CARBON_MONOXIDE_PPM))
AirQualityData_NEW$NO2_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "no2", con = AirQualityData_NEW$NITROGEN_DIOXIDE_PPM))
AirQualityData_NEW$O3_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "o3", con = AirQualityData_NEW$OZONE_PPM/1000, type = "1h"))
AirQualityData_NEW$SO2_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "so2", con = AirQualityData_NEW$SULFUR_DIOXIDE_PPB))
AirQualityData_NEW$PM25_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "pm25", con = AirQualityData_NEW$PM25_UG_M3))
AirQualityData_NEW$PM10_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "pm10", con = AirQualityData_NEW$PM10_UG_M3))

###### Site = TOR ----
AirQualityData_TOR$CO_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "co", con = AirQualityData_TOR$CARBON_MONOXIDE_PPM))
AirQualityData_TOR$NO2_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "no2", con = AirQualityData_TOR$NITROGEN_DIOXIDE_PPM))
AirQualityData_TOR$PM25_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "pm25", con = AirQualityData_TOR$PM25_UG_M3))


###### Site = MON ----
AirQualityData_MON$CO_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "co", con = AirQualityData_MON$CARBON_MONOXIDE_PPM))
AirQualityData_MON$NO2_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "no2", con = AirQualityData_MON$NITROGEN_DIOXIDE_PPM))
AirQualityData_MON$PM25_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "pm25", con = AirQualityData_MON$PM25_UG_M3))

###### Site = RIT ----
AirQualityData_RIT$SO2_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "so2", con = AirQualityData_RIT$SULFUR_DIOXIDE_PPB))
AirQualityData_RIT$PM25_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "pm25", con = AirQualityData_RIT$PM25_UG_M3))

###### Site = LABP ----
AirQualityData_LABP$O3_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "o3", con = AirQualityData_LABP$OZONE_PPM/1000, type = "8h"))
AirQualityData_LABP$PM25_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "pm25", con = AirQualityData_LABP$PM25_UG_M3))

###### Site = NEA ----
AirQualityData_NEA$O3_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "o3", con = AirQualityData_NEA$OZONE_PPM/1000, type = "8h"))

###### Site = FAB ----
AirQualityData_FAB$PM25_aqi <- as.numeric(con2aqi::con2aqi(pollutant = "pm25", con = AirQualityData_FAB$PM25_UG_M3))

#### AQI Continued ----
AirQualityData <-  bind_rows(AirQualityData_FAB, AirQualityData_LABP, AirQualityData_MON, AirQualityData_NEA, AirQualityData_NEW, AirQualityData_RIT, AirQualityData_TOR)
AirQualityData %>% 
  st_transform(4326) %>% 
  select(2,16,17,15,18,14,19) %>% 
  pivot_longer(cols = ends_with("aqi"),
               values_drop_na = T,
               names_to = "Pollutant",
               values_to = "AQI") -> AirQualityDataLong
AirQualityDataLong$Pollutant <- gsub("_aqi", "", AirQualityDataLong$Pollutant)
AirQualityDataLong$Label <- paste(AirQualityDataLong$Pollutant, AirQualityDataLong$AQI, sep = ": ")
AirQualityDataLong %>% 
  dplyr::mutate(label = paste0(SITE_NAME, Label, collapse = "<br>"))-> AirQualityDataLong

AirQualityDataLong %>% 
  group_by(SITE_NAME) %>% 
  dplyr::summarise(MeanAQI = mean(AQI)) %>% 
  st_transform(4326)-> AirQualityDataWide
AirQualityDataWide$MeanAQI <- as.numeric(round(AirQualityDataWide$MeanAQI, 1))

agg <- aggregate(Label~SITE_NAME, data = AirQualityDataLong, paste0, collapse="<br>")
AirQualityDataMap <- merge(AirQualityDataWide, agg, by = "SITE_NAME", all = T)

AQI_Levels <- read.csv("AQI_Levels.csv")
AQI_Levels %>% 
  as.data.frame() %>% 
  select(2,1,3)-> AQI_Levels
#### 2024 INPUT DATA -----
############ 2024 Carbon Monoxide Data ----
CO_2024_small <- read.csv("CO_2024DailyData.csv")
CO_2024_small$date_local <- as.Date(CO_2024_small$date_local, format = "%m/%d/%Y")
CO_2024_small$SiteName <- gsub("48", "NEW", CO_2024_small$SiteName)
CO_2024_small$SiteName <- gsub("75", "TOR", CO_2024_small$SiteName)
CO_2024_small$SiteName <- gsub("76", "MON", CO_2024_small$SiteName)
CO_2024_small %>% 
  subset(sample_duration == "1 HOUR") -> CO_2024_1hour
CO_2024_small %>% 
  subset(sample_duration == "8-HR RUN AVG END HOUR") -> CO_2024_8hour
CO_2024_8hour %>%
  group_by(SiteName) %>%
  dplyr::summarize(Min = min(aqi, na.rm = F),
                   Q1=quantile(aqi,probs = 0.25),
                   Median = median(aqi),
                   Q3=quantile(aqi, probs = 0.75),
                   Max = max(aqi),
                   Mean = mean(aqi),
                   Latitude = mean(latitude),
                   Longitude = mean(longitude)) -> CO_aqistats

############ 2024 Nitrogen Dioxide Data ------
NO2_2024_small <- read.csv("NO2_2024DailyData.csv")
NO2_2024_small$date_local <- as.Date(NO2_2024_small$date_local, format = "%m/%d/%Y")
NO2_2024_small$SiteName <- gsub("48", "NEW", NO2_2024_small$SiteName)
NO2_2024_small$SiteName <- gsub("75", "TOR", NO2_2024_small$SiteName)
NO2_2024_small$SiteName <- gsub("76", "MON", NO2_2024_small$SiteName)
NO2_2024_small %>%
  group_by(SiteName) %>%
  dplyr::summarize(Min = min(aqi, na.rm = F),
                   Q1=quantile(aqi,probs = 0.25),
                   Median = median(aqi),
                   Q3=quantile(aqi, probs = 0.75),
                   Max = max(aqi),
                   Mean = mean(aqi),
                   Latitude = mean(latitude),
                   Longitude = mean(longitude)) -> NO2_aqistats

############ 2024 Ozone Data ----
O3_2024_small <- read.csv("O3_2024DailyData.csv")
O3_2024_small$date_local <- as.Date(O3_2024_small$date_local, format = "%m/%d/%Y")
O3_2024_small$SiteName <- gsub("48", "NEW", O3_2024_small$SiteName)
O3_2024_small$SiteName <- gsub("4", "LABP", O3_2024_small$SiteName)
O3_2024_small$SiteName <- gsub("24", "NEA", O3_2024_small$SiteName)
O3_2024_small %>%
  group_by(SiteName) %>%
  dplyr::summarize(Min = min(aqi, na.rm = F),
                   Q1=quantile(aqi,probs = 0.25),
                   Median = median(aqi),
                   Q3=quantile(aqi, probs = 0.75),
                   Max = max(aqi),
                   Mean = mean(aqi),
                   Latitude = mean(latitude),
                   Longitude = mean(longitude)) -> O3_aqistats
############ 2024 Sulfur Dioxide Data ----
SO2_2024_small <- read.csv("SO2_2024DailyData.csv")
SO2_2024_small$date_local <- as.Date(SO2_2024_small$date_local, format = "%m/%d/%Y")
SO2_2024_small$SiteName <- gsub("48", "NEW", SO2_2024_small$SiteName)
SO2_2024_small$SiteName <- gsub("55", "RIT", SO2_2024_small$SiteName)
SO2_2024_small %>%
  group_by(SiteName) %>%
  dplyr::summarize(Min = min(aqi, na.rm = F),
                   Q1=quantile(aqi,probs = 0.25),
                   Median = median(aqi),
                   Q3=quantile(aqi, probs = 0.75),
                   Max = max(aqi),
                   Mean = mean(aqi),
                   Latitude = mean(latitude),
                   Longitude = mean(longitude)) -> SO2_aqistats

############ 2024 PM2.5 Data ----
PM25_2024_small <- read.csv("PM25_2024DailyData.csv")
PM25_2024_small$date_local <- as.Date(PM25_2024_small$date_local, format = "%m/%d/%Y")
PM25_2024_small$SiteName <- gsub("48", "NEW", PM25_2024_small$SiteName)
PM25_2024_small$SiteName <- gsub("75", "TOR", PM25_2024_small$SiteName)
PM25_2024_small$SiteName <- gsub("76", "MON", PM25_2024_small$SiteName)
PM25_2024_small$SiteName <- gsub("55", "RIT", PM25_2024_small$SiteName)
PM25_2024_small$SiteName <- gsub("4", "LABP", PM25_2024_small$SiteName)
PM25_2024_small$SiteName <- gsub("57", "FAB", PM25_2024_small$SiteName)
PM25_2024_small %>%
  group_by(SiteName) %>%
  dplyr::summarize(Min = min(aqi, na.rm = F),
                   Q1=quantile(aqi,probs = 0.25),
                   Median = median(aqi),
                   Q3=quantile(aqi, probs = 0.75),
                   Max = max(aqi),
                   Mean = mean(aqi),
                   Latitude = mean(latitude),
                   Longitude = mean(longitude)) -> PM25_aqistats
############ 2024 PM10 Data ----
PM10_2024_small <- read.csv("PM10_2024DailyData.csv")
PM10_2024_small$date_local <- as.Date(PM10_2024_small$date_local, format = "%m/%d/%Y")
PM10_2024_small$SiteName <- gsub("48", "NEW", PM10_2024_small$SiteName)
PM10_2024_small %>%
  group_by(SiteName) %>%
  dplyr::summarize(Min = min(aqi, na.rm = F),
                   Q1=quantile(aqi,probs = 0.25),
                   Median = median(aqi),
                   Q3=quantile(aqi, probs = 0.75),
                   Max = max(aqi),
                   Mean = mean(aqi),
                   Latitude = mean(latitude),
                   Longitude = mean(longitude)) -> PM10_aqistats

#### 2025 INPUT DATA ----
PhilaNames <- c("LAPB", "NEA", "NEW", "FAB", "TOR", "MON")
PhilaLat <- c(40.00892, 40.0767, 39.991389, 39.9603, 40.054171, 39.988842)
PhilaLong <- c(-75.09777, -75.0108, -75.080833, -75.1424, -74.985166, -75.207205)
PhilaSites <- data.frame(PhilaNames, PhilaLat, PhilaLong) %>% 
  setNames(c("SiteName", "Latitude", "Longitude"))

### Retrieve existing 2025 data from source folder
CurrentPhila2025data <- read.csv("Phila2025data.csv")
CurrentPhila2025data$Date <- as.Date(CurrentPhila2025data$Date, format = "%Y-%m-%d")
StartDate <- as.Date(max(CurrentPhila2025data$Date))
EndDate <- as.Date(Sys.Date()-1)
Dates2025 <- seq.Date(from = StartDate, to = EndDate, by = "1 day")
Dates2025 <- format(as.Date(Dates2025), "%Y%m%d")
### Retrieve new 2025 data from AirNow site
All2025data <- ldply(Dates2025, function(x) read.table(
  paste("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/2025/", x, "/daily_data.dat", sep = ""), 
  header=FALSE, 
  quote = "",
  sep="|",
  stringsAsFactors=FALSE,
  fill = T))
All2025data %>% 
  setNames(c("Date", "SiteNumber", "SiteName", "Pollutant", "Unit", "Measurement", "Interval", "Name")) %>% 
  subset(SiteName %in% PhilaNames) -> NewPhila2025data
NewPhila2025data$Date <- as.Date(NewPhila2025data$Date, format = "%m/%d/%y")
NewPhila2025data$Pollutant <- gsub("\\.", "", NewPhila2025data$Pollutant)
NewPhila2025data$Pollutant <- gsub("-", "_", NewPhila2025data$Pollutant)
NewPhila2025data$SiteNumber <- as.integer(NewPhila2025data$SiteNumber)
NewPhila2025data <- join(NewPhila2025data, PhilaSites, by = "SiteName")
### Join old and new
CurrentPhila2025data <- union(CurrentPhila2025data, NewPhila2025data)
### Save new, updated dataset to source folder
write.csv(CurrentPhila2025data, "Phila2025data.csv", row.names = F)
for(i in unique(CurrentPhila2025data$Pollutant)) {
  nam <- paste("Phila2025", i, sep = "_")
  assign(nam, CurrentPhila2025data[CurrentPhila2025data$Pollutant==i,])
}
############ CO 2025 STATS ------
Phila2025_CO_8hr %>%
  group_by(SiteName) %>%
  dplyr::summarize(Min = min(Measurement, na.rm = F),
                   Q1=quantile(Measurement,probs = 0.25),
                   Median = median(Measurement),
                   Q3=quantile(Measurement, probs = 0.75),
                   Max = max(Measurement),
                   Mean = mean(Measurement),
                   Latitude = mean(Latitude),
                   Longitude = mean(Longitude)) -> CO2025_PPMstats
############ NO2 2025 DATA & STATS ----
##### Read in existing 2025 NO2 data set
Phila2025_NO <- read.csv("Phila2025NO2Dailydata.csv")
Phila2025_NO %>% 
  mutate(as.numeric(SiteNumber)) %>% 
  mutate(as.numeric(Latitude)) %>% 
  mutate(as.numeric(Longitude)) ->Phila2025_NO
Phila2025_NO$Date <- as.Date(Phila2025_NO$Date)
##### Determine series of dates/times needed to retrieve to update 2025 dataset
StartDateTime <- as.POSIXct(max(Phila2025_NO$Date)-1)
DateTimes2025_1 <- seq(StartDateTime, as.POSIXct(Sys.Date()-1), by="1 hour")
DateTimes2025 <- format(DateTimes2025_1, "%Y%m%d%H")

##### Retrieve new NO Air Quality Data to present date
NewNO2_2025data <- ldply(DateTimes2025, function(x) read.table(paste("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/2025/", substr(x,1,nchar(x)-2), "/HourlyAQObs_", x, ".dat", sep = ""),  
                                                               header=T,
                                                               sep=",",
                                                               stringsAsFactors=FALSE,
                                                               fill = T,
                                                               col.names = c("AQSID","SiteName", "Status","EPARegion", "Latitude", "Longitude", "Elevation", "GMTOffset", "CountryCode","StateName", "ValidDate", "ValidTime", "DataSource", "ReportingArea_PipeDelimited", "OZONE_AQI", "PM10_AQI", "PM25_AQI", "NO2_AQI", "OZONE_Measured", "PM10_Measured", "PM25_Measured", "NO2_Measured", "PM25", "PM25_Unit", "OZONE", "OZONE_Unit", "NO2", "NO2_Unit", "CO", "CO_Unit", "SO2", "SO2_Unit", "PM10", "PM10_Unit")))

###### Sort for only Philly stations
NewNO2_2025data %>% 
  subset(SiteName %in% PhilaNames) -> CurrentPhila_2025_NO2data

CurrentPhila_2025_NO2data %>% 
  dplyr::rename("Date" = ValidDate) %>%
  dplyr::rename("SiteNumber" = AQSID)  -> CurrentPhila_2025_NO2data 

CurrentPhila_2025_NO2data %>%
  select(1,2,5,6,11,27,28) %>% 
  subset(!(is.na(NO2))) %>% 
  group_by(SiteName, SiteNumber, Latitude, Longitude, Date) %>% 
  dplyr::summarise(Measurement = mean(NO2, na.rm=T),
                   Unit = max(NO2_Unit)) %>%  
  ungroup()-> CurrentPhila_2025_NO2data

CurrentPhila_2025_NO2data %>% 
  select(5,1,2,7,6,3,4)-> CurrentPhila_2025_NO2data
CurrentPhila_2025_NO2data$SiteNumber <- as.numeric(as.character(CurrentPhila_2025_NO2data$SiteNumber))
CurrentPhila_2025_NO2data$Latitude <- as.numeric(as.character(CurrentPhila_2025_NO2data$Latitude))
CurrentPhila_2025_NO2data$Longitude <- as.numeric(as.character(CurrentPhila_2025_NO2data$Longitude))
CurrentPhila_2025_NO2data$Date <- as.Date(CurrentPhila_2025_NO2data$Date, format = "%m/%d/%Y")

CurrentPhila_2025_NO2data <- union(CurrentPhila_2025_NO2data, Phila2025_NO)

##### Save updated NO2 file
write.csv(CurrentPhila_2025_NO2data, "Phila2025NO2Dailydata.csv", row.names = F)

CurrentPhila_2025_NO2data %>%
  group_by(SiteName) %>%
  dplyr::summarize(Min = min(Measurement, na.rm = F),
                   Q1=quantile(Measurement,probs = 0.25),
                   Median = median(Measurement),
                   Q3=quantile(Measurement, probs = 0.75),
                   Max = max(Measurement),
                   Mean = mean(Measurement),
                   Latitude = mean(Latitude),
                   Longitude = mean(Longitude)) -> NO2_2025_PPMstats

############ OZONE 2025 STATS -----
Phila2025_OZONE_8HR %>%
  group_by(SiteName) %>%
  dplyr::summarize(Min = min(Measurement, na.rm = F),
                   Q1=quantile(Measurement,probs = 0.25),
                   Median = median(Measurement),
                   Q3=quantile(Measurement, probs = 0.75),
                   Max = max(Measurement),
                   Mean = mean(Measurement),
                   Latitude = mean(Latitude),
                   Longitude = mean(Longitude)) -> O3_2025_PPMstats

############ SO2 2025 STATS -----
Phila2025_SO2_24HR %>%
  group_by(SiteName) %>%
  dplyr::summarize(Min = min(Measurement, na.rm = F),
                   Q1=quantile(Measurement,probs = 0.25),
                   Median = median(Measurement),
                   Q3=quantile(Measurement, probs = 0.75),
                   Max = max(Measurement),
                   Mean = mean(Measurement),
                   Latitude = mean(Latitude),
                   Longitude = mean(Longitude)) -> SO2_2025_PPMstats

############ PM25 2025 STATS -----
Phila2025_PM25_24hr %>%
  group_by(SiteName) %>%
  dplyr::summarize(Min = min(Measurement, na.rm = F),
                   Q1=quantile(Measurement,probs = 0.25),
                   Median = median(Measurement),
                   Q3=quantile(Measurement, probs = 0.75),
                   Max = max(Measurement),
                   Mean = mean(Measurement),
                   Latitude = mean(Latitude),
                   Longitude = mean(Longitude)) -> PM25_2025_PPMstats

############ PM10 2025 STATS -----
Phila2025_PM10_24hr %>%
  group_by(SiteName) %>%
  dplyr::summarize(Min = min(Measurement, na.rm = F),
                   Q1=quantile(Measurement,probs = 0.25),
                   Median = median(Measurement),
                   Q3=quantile(Measurement, probs = 0.75),
                   Max = max(Measurement),
                   Mean = mean(Measurement),
                   Latitude = mean(Latitude),
                   Longitude = mean(Longitude)) -> PM10_2025_PPMstats



#### DASHBOARD------

ui=dashboardPage(title="Philadelphia Air Quality",
                 dashboardHeader(
                   titleWidth='100%',
                   title=span(
                     column(12,
                            class="title-box",
                            tags$h1(class="title", style='margin-top:5px;',"Philadelphia Air Quality")))),
                 dashboardSidebar(
                   sidebarMenu(
                     menuItem("Air Quality Index", tabName = "AQI"),
                     menuItem("Carbon Monoxide", tabName = "CO_map"),
                     menuItem("Nitrogen Dioxide", tabName = "NO2_map"),
                     menuItem("Ozone", tabName = "O3_map"),
                     menuItem("Sulfur Dioxide", tabName = "SO2_map"),
                     menuItem("Particulate Matter (PM) 2.5", tabName = "PM25_map"),
                     menuItem("Particulate Matter (PM) 10", tabName = "PM10_map")
                   )
                 ),
                 dashboardBody(
                   tags$head( 
                     tags$style(HTML(".main-sidebar { font-size: 18px;}"))),
                   tabItems(
                     ###### AQI PAGE -----  
                     tabItem('AQI',
                             fluidPage(
                               verticalLayout(
                                 box(width=12,
                                     fluidRow(
                                       column(6,
                                              p(strong("Air Quality Index"), style= "font-size:26px;"),
                                              p("Developed by the Environmental Protection Agency (EPA) in 1980, the Air Quality Index (AQI) is used to report and communicate ambient air quality and the associated risks to public health.", style= "font-size:16px;"),
                                              p("AQI is based on measurements of 5 pollutants:", style= "font-size:16px;"),
                                              p("- Ground-level ozone", style="font-size:16px; margin-left: 40px;margin-top: -5px;"),
                                              p("-	Particulate matter", style="font-size:16px; margin-left: 40px; margin-top: -10px;"),
                                              p("-	Carbon monoxide", style="font-size:16px; margin-left: 40px; margin-top: -10px;"),
                                              p("-	Sulfur dioxide", style="font-size:16px; margin-left: 40px; margin-top: -10px;"),
                                              p("-	Nitrogen dioxide", style="font-size:16px; margin-left: 40px; margin-top: -10px;"),
                                              p("The AQI consists of 6 color-coded risk categories, representing increasing levels of health concern. AQI values range from 0 to over 500. They are based on the short-term National Ambient Air Quality Standards (NAAQS) set by the EPA, with AQI values of 100 approximately corresponding with the current NAAQS level for each pollutant.", style= "font-size:16px;"),
                                              p("Values below 100 are generally considered satisfactory, and values higher than 100 indicate unhealthy air quality. Each AQI level is associated with pollutant-specific health concerns and includes guidance for reducing risk and exposure to air pollution.", style= "font-size:16px;"),
                                              p("The Clean Air Act of 1990 requires  NAAQS to be reviewed every 5 years and revised as necessary to reflect the most up-to-date scientific findings. The Air Quality Index is periodically updated to incorporate any changes to the NAAQS.", style= "font-size:16px;"),
                                              DT::dataTableOutput("AQI_Table", width = 680)),
                                       column(6,
                                              p(strong("Current Mean AQI Values"), style="font-size:22px; text-align:center"),
                                              p("Click on points for individual pollutant AQI values", style= "font-size:16px; margin-top: -10px; text-align:center"),
                                              leafletOutput("AQI_Map", height = 710),
                                              wellPanel(textOutput("zip7"),  style= "font-size:18px;"))))))), 
                     ###### CO PAGE ---------------
                     tabItem('CO_map',
                             fluidPage(
                               tabBox(width = 12,
                                      selected = "Current CO Measurements",
                                      tags$head(tags$style(HTML('.nav-tabs-custom>.nav-tabs {font-size: 18px;font-weight: bold; font-color: #9c9c9c}',
                                                                '.nav-tabs-custom>.nav-tabs>li[class=active]>a {background-color: #ccdbfb;
                                                 color: #000099;}'))),
                                      ### ABOUT CARBON MONOXIDE TAB ----
                                      tabPanel("About Carbon Monoxide",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       fluidRow(
                                                         column(9,
                                                                p("Carbon monoxide (CO) is a poisonous, flammable gas that is slightly less dense than air. 
                                                     It is created by the incomplete combustion of carbon-containing fuels, such as gasoline, 
                                                     natural gas, oil, or wood. Internal combustion engines such as motor vehicles, generators, 
                                                     and lawn mowers are common outdoor sources.",
                                                                  style="font-size:16px;"),
                                                                p("Carbon monoxide is a contributing factor to climate change, reacting with oxygen in the 
                                                     atmosphere to create ozone (O3) and carbon dioxide (CO2).",
                                                                  style="font-size:16px;")),
                                                         column(3,
                                                                tags$img(src='CO_2.jpg', height = 120),
                                                                p())),
                                                       fluidRow(
                                                         column(5,
                                                                tags$img(src='CO_1.jpg', height = 330)),
                                                         column(7,
                                                                p("Because it is colorless, odorless, and tasteless, carbon monoxide is undetectable to human 
                                                     senses. Indoors, it is a dangerous air contaminant. Carbon monoxide poisoning causes 
                                                     thousands of hospitalizations and hundreds of deaths each year. Very high CO levels are 
                                                     unlikely to occur outdoors, but are possible, especially from accumulation of exhaust 
                                                     fumes in poorly ventilated areas. Elevated atmospheric levels are a particular risk for 
                                                     individuals with some types of heart disease, especially when exercising or under increased 
                                                     stress, as CO exposure can reduce the blood’s oxygen-carrying capacity.",
                                                                  style="font-size:16px;"),
                                                                p("Carbon monoxide pollution is typically measured in parts per million (ppm) in ambient 
                                                     (referring to outdoor or atmospheric) air. In 1971, the Environmental Protection Agency 
                                                     (EPA) set two National Ambient Air Quality Standards (NAAQS) for public health protection 
                                                     that relate to carbon monoxide:",
                                                                  style="font-size:16px;"),
                                                                p(paste("- 9 ppm in ambient air, measured over 8 hours"), 
                                                                  style="font-size:16px; margin-left: 40px;"),
                                                                p("- 35 ppm in ambient air, measured over 1 hour", 
                                                                  style="font-size:16px; margin-left: 40px; margin-top: -10px;"),
                                                                p("The EPA conducted reviews of those standards in 1985 and 2011, and they are still in place 
                                                     today. As of 2022, there were 274 monitoring sites reporting CO concentrations to the 
                                                     EPA, mostly located in urban areas.", style="font-size:16px;")))),
                                                   box(width = 12,
                                                       p(strong("Visit the following links for more information:"), 
                                                         style="font-size:20px;"),
                                                       p(tags$a(href="https://www.epa.gov/co-pollution/basic-information-about-
                                      carbon-monoxide-co-outdoor-air-pollution", 
                                                                "EPA - Basic Information about Carbon Monoxide (CO) Outdoor Air Pollution"),
                                                         style="font-size:18px;"),
                                                       p(tags$a(href="https://www.epa.gov/sites/default/files/2016-07/documents/cofactsheetaugust12v4.pdf", 
                                                                "EPA Fact Sheet - National Ambient Air Quality Standards for Carbon Monoxide"),
                                                         style="font-size:18px;"),
                                                       p(tags$a(href="https://www.cdc.gov/carbon-monoxide/about/index.html", 
                                                                "CDC - Carbon Monoxide Poisoning Basics"),
                                                         style="font-size:18px;"))))),
                                      ### CO CURRENT MEASUREMENTS TAB -----
                                      tabPanel("Current CO Measurements",
                                               verticalLayout(
                                                 fluidRow(
                                                   column(4,
                                                          wellPanel(textOutput("zip1"),  style= "font-size:18px;")),
                                                   column(3,
                                                          plotOutput("CO_AQIbar", width = 330, height = 80)),
                                                   column(5,
                                                          wellPanel(p(strong(tags$a(href="https://www.epa.gov/criteria-air-pollutants/naaqs-table",
                                                                                    "Link to National Ambient Air Quality Standards")),
                                                                      style = "text-align:center; font-size:20px;")))),
                                                 mainPanel(
                                                   fluidRow(
                                                     column(10,
                                                            leafletOutput("CO", height = 650)),
                                                     column(2,
                                                            fluidRow(
                                                              plotOutput("CO_Gauge", width = 600, height = 280)),
                                                            fluidRow(
                                                              plotOutput("CO_Plot", width = 600, height = 130)),
                                                            fluidRow(
                                                              DT::dataTableOutput("CO_Table", width = 600))))))),
                                      ### 2025 CO TRENDS TAB ----
                                      tabPanel("2025 CO Trends",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       column(6,
                                                              p("2025 to Present - Mean Carbon Dioxide Measurements by Site Location", 
                                                                style="font-size: 20px; font-weight: bold;"),
                                                              leafletOutput("CO_2025Map", width=650, height=700)),
                                                       column(6,
                                                              div(id="COTrends2025",
                                                                  plotOutput("CO_2025Trends", width = 650, height = 400)),
                                                              bsModal("popout21", "CO_2025Trends", "CarbonMonoxideTrends",
                                                                      size = "large", title = "2025 Daily Carbon Monoxide Trends", plotOutput("CO_2025Trends1")),
                                                              p(),
                                                              div(id="CODist",
                                                                  plotOutput("CO_boxplots2025", width = 600, height = 200)),
                                                              bsModal("popout22", "CO_boxplots2025", "CODist",
                                                                      size = "large", title = "2025 Carbon Monoxide Distributions", plotOutput("CO_boxplots2025_1")),
                                                              DT::dataTableOutput("CO_DistTable", width = 600)))))),
                                      ### 2024 CO TRENDS TAB ----
                                      tabPanel("2024 CO Trends",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       column(6,
                                                              p("2024 Mean Air Quality Index by Site Location", style="font-size: 20px; font-weight: bold;"),
                                                              leafletOutput("CO_2024Map", width=650, height=550),
                                                              DT::dataTableOutput("CO_Table_2024", width = 650)),
                                                       column(6,
                                                              div(id="COTrends", 
                                                                  plotOutput("CO_2024Trends", width = 650, height = 500)),
                                                              bsModal("popout1", "CO_2024Trends", "CarbonMonoxideTrends", 
                                                                      size = "large", title = "2024 Daily Carbon Monoxide Trends", plotOutput("CO_2024Trends1")),
                                                              div(id="CODist", 
                                                                  plotOutput("CO_boxplots", width = 620, height = 200)),
                                                              bsModal("popout2", "CO_boxplots", "CODist", 
                                                                      size = "large", title = "2024 Carbon Monoxide Distributions", plotOutput("CO_boxplots1")),
                                                              tags$img(src="CO_DistTable.jpg", width = "600", 
                                                                       style="padding-left:70px; padding-top: 20px;"))))))))),
                     ###### NO2 PAGE ----
                     tabItem('NO2_map',
                             fluidPage(
                               tabBox(width=12,
                                      selected= HTML(paste("Current  NO",tags$sub("2"), "Measurements")),
                                      tags$head(tags$style(HTML('.nav-tabs-custom>.nav-tabs {font-size: 20px;font-weight: bold;}'))),
                                      ### ABOUT NO2 TAB -----
                                      tabPanel("About Nitrogen Dioxide",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       fluidRow(
                                                         column(9,
                                                                p("Nitrogen dioxide (NO2) is a highly reactive, poisonous gas, part of a group of atmospheric pollutant gases known as nitrogen oxides (NOX). It is a reddish-brown gas, heavier than air, with a harsh, pungent odor at higher concentrations. It is formed by burning fuels, such as in internal combustion engines, or butane and kerosene stoves, but can also be created naturally through bacterial respiration, volcanoes, or lightning.",
                                                                  style="font-size:16px;"),
                                                                p("Nitrogen dioxide has many adverse environmental effects. When it reacts with water, oxygen, and other chemicals in the atmosphere, it can form acid rain. It is a major contributor to haze and smog, and can harm vegetation, damaging trees and forests, reducing growth and crop yields. It also contributes to nutrient pollution in rivers, lakes, and coastal waters, causing issues such as algal blooms and oxygen depletion, which negatively affect the health of both humans and aquatic life.",
                                                                  style="font-size:16px;")),
                                                         column(3,
                                                                tags$img(src='NO2_gas.jpg', height = 180),
                                                                p())),
                                                       fluidRow(
                                                         column(5,
                                                                tags$img(src='NO2_smog.jpg', height = 330)),
                                                         column(7,
                                                                p("Exposure to high levels of nitrogen dioxide in the air can lead to adverse health effects. At low levels, it is irritating to the eyes and respiratory system, aggravating respiratory diseases such as asthma. At higher concentrations, it can cause serious burns, edema (accumulation of fluid in the lungs), and death. Long-term, low-level exposure potentially contributes to the development of asthma and increased susceptibility to respiratory infections, and may be particularly harmful to children, decreasing lung function growth.",
                                                                  style="font-size:16px;"),
                                                                p("Nitrogen dioxide is typically measured in parts per billion (ppb) in ambient (referring to outdoor or atmospheric) air.  In 1971, the Environmental Protection Agency (EPA) set an annual standard for nitrogen dioxide concentrations, and in 2010, added a 1-hour standard. These National Ambient Air Quality Standards (NAAQS) for nitrogen dioxide are:",
                                                                  style="font-size:16px;"),
                                                                p(paste("- 100 ppb in ambient air, measured over 1 hour (1-hour standard)"), 
                                                                  style="font-size:16px; margin-left: 40px;"),
                                                                p("- 53 ppb in ambient air, measured as an annual average (annual standard)", 
                                                                  style="font-size:16px; margin-left: 40px; margin-top: -10px;"),
                                                                p("As of 2021, there were 491 monitoring sites reporting NO2 concentrations to the EPA.", style="font-size:16px;")))),
                                                   box(width = 12,
                                                       p(strong("Visit the following links for more information:"), 
                                                         style="font-size:20px;"),
                                                       p(tags$a(href="https://www.epa.gov/no2-pollution/basic-information-about-no2", 
                                                                "EPA - Basic Information about NO2"), style="font-size:18px;"),
                                                       p(tags$a(href="https://wwwn.cdc.gov/TSP/ToxFAQs/ToxFAQsDetails.aspx?faqid=396&toxid=69f", 
                                                                "CDC - FAQs for Nitrogen Oxides"), style="font-size:18px;"),
                                                       p(tags$a(href="", 
                                                                ""),style="font-size:16px;"))))),
                                      ### NO2 CURRENT MEASUREMENTS TAB --------
                                      tabPanel(HTML(paste("Current  NO",tags$sub("2"), "Measurements")),
                                               verticalLayout(
                                                 fluidRow(
                                                   column(4,
                                                          wellPanel(textOutput("zip2"),  style= "font-size:18px;")),
                                                   column(3,
                                                          plotOutput("NO2_AQIbar", width = 330, height = 80)),
                                                   column(5,
                                                          wellPanel(p(strong(tags$a(href="https://www.epa.gov/criteria-air-pollutants/naaqs-table",
                                                                                    "Link to National Ambient Air Quality Standards")), 
                                                                      style = "text-align:center; font-size:20px;")))),
                                                 mainPanel(
                                                   fluidRow(
                                                     column(10,
                                                            leafletOutput("NO2", height = 650)),
                                                     column(2,
                                                            fluidRow(
                                                              plotOutput("NO2_Gauge", width = 600, height = 280)),
                                                            fluidRow(
                                                              plotOutput("NO2_Plot", width = 600, height = 130)),
                                                            fluidRow(
                                                              DT::dataTableOutput("NO2_Table", width = 600))))))),
                                      ### 2025 NO2 TRENDS TAB ----
                                      tabPanel(HTML(paste("2025 NO",tags$sub("2")," Trends")),
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       column(6,
                                                              p("2025 to Present - Mean Nitrogen Dioxide Measurements by Site Location", 
                                                                style="font-size: 20px; font-weight: bold;"),
                                                              leafletOutput("NO2_2025Map", width=650, height=700)),
                                                       column(6,
                                                              div(id="NO2Trends2025",
                                                                  plotOutput("NO2_2025Trends", width = 650, height = 400)),
                                                              bsModal("popout23", "NO2_2025Trends", "NitrogenDioxideTrends",
                                                                      size = "large", title = "2025 Daily Nitrogen Dioxide Trends", plotOutput("NO2_2025Trends1")),
                                                              p(),
                                                              div(id="NODist",
                                                                  plotOutput("NO2_boxplots2025", width = 600, height = 200)),
                                                              bsModal("popout24", "NO2_boxplots2025", "NO2Dist",
                                                                      size = "large", title = "2025 Nitrogen Dioxide Distributions", plotOutput("NO2_boxplots2025_1")),
                                                              DT::dataTableOutput("NO2_DistTable", width = 600)))))),
                                      ### 2024 NO2 TRENDS TAB ----
                                      tabPanel(HTML(paste("2024 NO",tags$sub("2")," Trends")),
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       column(6,
                                                              p("2024 Mean Air Quality Index by Site Location", style="font-size: 20px; font-weight: bold;"),
                                                              leafletOutput("NO2_2024Map", width=650, height=600),
                                                              DT::dataTableOutput("NO2_Table_2024", width = 650)),
                                                       column(6,
                                                              div(id="NO2Trends", 
                                                                  plotOutput("NO2_2024Trends", width = 650, height = 525)),
                                                              bsModal("popout3", "NO2_2024Trends", "NitrogenDioxideTrends", 
                                                                      size = "large", title = "2024 Daily Nitrogen Dioxide Trends", plotOutput("NO2_2024Trends1")),
                                                              div(id="NO2Dist", 
                                                                  plotOutput("NO2_boxplots", width = 620, height = 200)),
                                                              bsModal("popout4", "NO2_boxplots", "NO2Dist", 
                                                                      size = "large", title = "2024 Nitrogen Dioxide Distributions", plotOutput("NO2_boxplots1")),
                                                              tags$img(src="NO2_DistTable.jpg", width = "600", 
                                                                       style="padding-left:70px; padding-top: 20px;"))))))))),
                     ###### OZONE PAGE --------------------
                     tabItem('O3_map',
                             fluidPage(
                               tabBox(width=12,
                                      selected= "Current Ozone Measurements",
                                      tags$head(tags$style(HTML('.nav-tabs-custom>.nav-tabs {font-size: 18px;font-weight: bold;}'))),
                                      ### ABOUT OZONE TAB -----
                                      tabPanel("About Ozone",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       fluidRow(
                                                         column(9,
                                                                p("Ozone (O3) is a pale blue gas, with a pungent smell similar to chlorine. The majority of ozone (~90%) is found in the stratosphere, 6-10 miles above the Earth’s surface, where it is known as the ozone layer. Stratospheric ozone is sometimes referred to as “good ozone,” because it absorbs damaging ultraviolet (UV) sunlight and helps regulate atmospheric temperature. Ozone in the lower, ground level of the atmosphere, the troposphere, is considered a harmful and destructive air pollutant. It is the main component of smog.",
                                                                  style="font-size:16px;"),
                                                                p("Ozone in the lower level of the atmosphere is created when pollutants from sources such as cars, power plants and refineries react with sunlight. Peak levels typically occur in urban areas during afternoon hours on hot, sunny days. However, high levels of formation and concentration are not limited to urban locations, and can occur even during colder months.",
                                                                  style="font-size:16px;"),
                                                                p("Exposure to ozone can lead to numerous adverse health effects, primarily impacting the respiratory system. It has also been shown to affect the central nervous system, cognitive development, and cardiovascular and reproductive health. Ozone exposure has also been linked to increased mortality, especially in older populations.",
                                                                  style="font-size:16px;")),
                                                         column(3,
                                                                tags$img(src='O3_layer.png', height = 250),
                                                                p())),
                                                       fluidRow(
                                                         column(6,
                                                                tags$img(src='O3_smokestack.jpg', height = 330)),
                                                         column(6,
                                                                p("Ozone pollution also has harmful environmental and economic effects. Ozone exposure can damage materials such as rubber, plastic, and metals, reducing their functionality and life span. It can be harmful to vegetation, reducing photosynthesis, slowing plant growth, and increasing susceptibility to disease, pest damage, cold, and drought. It has also been linked to reductions in crop and timber yields. ",
                                                                  style="font-size:16px;"),
                                                                p(paste("Ozone is typically measured in parts per million (ppm) in ambient (referring to outdoor or atmospheric) air. In 1971, the Environmental Protection Agency (EPA) set a 1-hour standard for ozone at 0.08 ppm. This standard has been revised periodically in the years since, most recently in 2015. The current National Ambient Air Quality Standards (NAAQS) for ozone is:"), 
                                                                  style="font-size:16px; "),
                                                                p("- 0.070 parts per million (ppm), as the fourth-highest daily maximum 8-hour concentration, averaged across three consecutive years.", 
                                                                  style="font-size:16px; margin-left: 40px;"),
                                                                p("The EPA monitors ground-level ozone in the US through more than 4,000 monitoring stations, most of which are owned and operated by state environmental agencies.", style="font-size:16px;")))),
                                                   box(width = 12,
                                                       p(strong("Visit the following links for more information:"), 
                                                         style="font-size:18px;"),
                                                       p(tags$a(href="https://www.epa.gov/ground-level-ozone-pollution/ground-level-ozone-basics", 
                                                                "EPA - Ground-level Ozone Basics"),
                                                         style="font-size:16px;"),
                                                       p(tags$a(href="", 
                                                                ""),
                                                         style="font-size:16px;"),
                                                       p(tags$a(href="", 
                                                                ""),
                                                         style="font-size:16px;"))))),
                                      ### CURRENT O3 MEASUREMENTS TAB ------
                                      tabPanel("Current Ozone Measurements",
                                               verticalLayout(
                                                 fluidRow(
                                                   column(4,
                                                          wellPanel(textOutput("zip3"),  style= "font-size:18px;")),
                                                   column(3,
                                                          plotOutput("O3_AQIbar", width = 330, height = 80)),
                                                   column(5,
                                                          wellPanel((p(strong(tags$a(href="https://www.epa.gov/criteria-air-pollutants/naaqs-table",
                                                                                     "Link to National Ambient Air Quality Standards")), 
                                                                       style = "text-align:center; font-size:20px;"))))),
                                                 mainPanel(
                                                   fluidRow(
                                                     column(10,
                                                            leafletOutput("O3", height = 650)),
                                                     column(2,
                                                            fluidRow(
                                                              plotOutput("Ozone_Gauge", width = 600, height = 280)),
                                                            fluidRow(
                                                              plotOutput("Ozone_Plot", width = 600, height = 130)),
                                                            fluidRow(
                                                              DT::dataTableOutput("Ozone_Table", width = 600))))))),
                                      ### 2025 OZONE TRENDS TAB ----
                                      tabPanel("2025 Ozone Trends",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       column(6,
                                                              p("2025 to Present - Mean Ozone Measurements by Site Location", 
                                                                style="font-size: 20px; font-weight: bold;"),
                                                              leafletOutput("O3_2025Map", width=650, height=700)),
                                                       column(6,
                                                              div(id="O3Trends2025",
                                                                  plotOutput("O3_2025Trends", width = 650, height = 400)),
                                                              bsModal("popout25", "O3_2025Trends", "OzoneTrends",
                                                                      size = "large", title = "2025 Daily Ozone Trends", plotOutput("O3_2025Trends1")),
                                                              p(),
                                                              div(id="O3Dist",
                                                                  plotOutput("O3_boxplots2025", width = 600, height = 200)),
                                                              bsModal("popout26", "O3_boxplots2025", "O3Dist",
                                                                      size = "large", title = "2025 Ozone Distributions", plotOutput("O3_boxplots2025_1")),
                                                              DT::dataTableOutput("O3_DistTable", width = 600)))))),
                                      ### 2024 OZONE TRENDS TAB ----
                                      tabPanel("2024 Ozone Trends",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       column(6,
                                                              p("2024 Mean Air Quality Index by Site Location", style="font-size: 20px; font-weight: bold;"),
                                                              leafletOutput("O3_2024Map", width=650, height=600),
                                                              DT::dataTableOutput("Ozone_Table_2024", width = 650)),
                                                       column(6,
                                                              div(id="OzoneTrends", 
                                                                  plotOutput("O3_2024Trends", width = 650, height = 525)),
                                                              bsModal("popout5", "O3_2024Trends", "OzoneTrends", 
                                                                      size = "large", title = "2024 Daily Ozone Trends", plotOutput("O3_2024Trends1")),
                                                              div(id="OzoneDist", 
                                                                  plotOutput("O3_boxplots", width = 620, height = 200)),
                                                              bsModal("popout6", "O3_boxplots", "OzoneDist", 
                                                                      size = "large", title = "2024 Ozone Distributions", plotOutput("O3_boxplots1")),
                                                              tags$img(src="O3_DistTable.jpg", width = "600", 
                                                                       style="padding-left:70px; padding-top: 20px;"))))))))),
                     ###### SO2 PAGE ---------------
                     tabItem('SO2_map',
                             fluidPage(
                               tabBox(width=12,
                                      selected= HTML(paste("Current  SO",tags$sub("2"), "Measurements")),
                                      tags$head(tags$style(HTML('.nav-tabs-custom>.nav-tabs {font-size: 18px;font-weight: bold;}'))),
                                      ### ABOUT SO2 TAB -----
                                      tabPanel("About Sulfur Dioxide",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       fluidRow(
                                                         column(9,
                                                                p(HTML(paste("Sulfur dioxide (SO", tags$sub("2"),") is a colorless gas, with a pungent odor described as being like burnt matches or rotten eggs. It is part of a group of gases known as sulfur oxides (SO", tags$sub("x"),"). It is a byproduct of copper extraction and is created by the burning of sulfur-bearing fossil fuels, such as coal, oil, and natural gas. It is also released into the atmosphere naturally through volcanic activity. The greatest contributors to SO", tags$sub("2")," in the atmosphere are power plants and industrial facilities.")),
                                                                  style="font-size:16px;"),
                                                                p("Sulfur dioxide can cause considerable environmental damage. It reacts with water and oxygen in the atmosphere to form sulfuric acid, which is one of the major components of acid rain. Acid rain contributes to the harmful acidification of soil and surface water (such as rivers, lakes, wetlands, and reservoirs), killing and injuring marine life, damaging forests, depleting soil nutrients, and reducing biodiversity. Acid rain also causes damage to buildings, monuments, and other man-made structures. Additionally, the reaction of sulfur dioxide with other pollutants in the atmosphere, such as ammonia, creates particles that contribute to particulate matter (PM) pollution.",
                                                                  style="font-size:16px;")),
                                                         column(3,
                                                                tags$img(src='SO2_volcano.jpg', height = 200))),
                                                       fluidRow(
                                                         column(4,
                                                                tags$img(src='SO2_powerplant.jpg', height = 300)),
                                                         column(8,
                                                                p("Sulfur dioxide has various adverse health effects, depending on the level of exposure. At lower levels, it can cause respiratory irritation and harm, and leads to breathing difficulties, such as wheezing, shortness of breath, and chest tightness, especially during exercise. At greatest risk of experiencing adverse effects are children, the elderly, and individuals who suffer from asthma, chronic lung diseases such as emphysema, and cardiovascular disease. Long-term exposure has been linked to increases in pulmonary disease, respiratory illnesses, cardiovascular disease, and an increased risk of mortality.",
                                                                  style="font-size:16px;"),
                                                                p("Sulfur dioxide is typically measured in parts per billion (PPB) in ambient (referring to outdoor or atmospheric) air. The EPA first set air quality standards relating to sulfur dioxide in 1971, and they have been revised several times since. The current National Ambient Air Quality Standards (NAAQS) for sulfur dioxide, set in 2010, are:",
                                                                  style="font-size:16px;"),
                                                                p(paste("-	75 ppm in ambient air, based on the 3-year average of the 99th percentile of the yearly distribution of 1-hour daily maximum concentrations"), 
                                                                  style="font-size:16px; margin-left: 40px;"),
                                                                p("As of 201, there were 505 monitoring sites reporting sulfur dioxide concentrations to the EPA. This includes 71 monitors in areas with stationary emission sources over 2,000 tons per year, as required by a 2015 EPA rule.", style="font-size:16px;")))),
                                                   box(width = 12,
                                                       p(strong("Visit the following links for more information:"), 
                                                         style="font-size:18px;"),
                                                       p(tags$a(href="https://www.epa.gov/so2-pollution/sulfur-dioxide-basics", 
                                                                "EPA - Sulfur Dioxide Basics"),
                                                         style="font-size:16px;"),
                                                       p(tags$a(href="", 
                                                                ""),
                                                         style="font-size:16px;"),
                                                       p(tags$a(href="https://wwwn.cdc.gov/TSP/ToxFAQs/ToxFAQsDetails.aspx?faqid=252&toxid=46", 
                                                                "CDC – FAQs for Sulfur Dioxide"),
                                                         style="font-size:16px;"),
                                                       p(tags$a(href="https://www.nps.gov/subjects/air/humanhealth-sulfur.htm#:~:text=Sulfur%20dioxide%20can%20convert%20to,at%20our%20national%20cultural%20monuments", 
                                                                "National Park Service (NPS) - Sulfur Dioxide Effects on Health"),
                                                         style="font-size:16px;"))))),
                                      ### CURRENT SO2 MEASUREMENTS TAB ------
                                      tabPanel(HTML(paste("Current  SO",tags$sub("2"), "Measurements")),
                                               verticalLayout(
                                                 fluidRow(
                                                   column(4,
                                                          wellPanel(textOutput("zip4"),  style= "font-size:18px;")),
                                                   column(3,
                                                          plotOutput("SO2_AQIbar", width = 330, height = 80)),
                                                   column(5,
                                                          wellPanel(p(strong(tags$a(href="https://www.epa.gov/criteria-air-pollutants/naaqs-table",
                                                                                    "Link to National Ambient Air Quality Standards")), 
                                                                      style = "text-align:center; font-size:20px;")))),
                                                 mainPanel(
                                                   fluidRow(
                                                     column(10,
                                                            leafletOutput("SO2", height = 650)),
                                                     column(2,
                                                            fluidRow(
                                                              plotOutput("SO2_Gauge", width = 600, height = 280)),
                                                            fluidRow(
                                                              plotOutput("SO2_Plot", width = 600, height = 130)),
                                                            fluidRow(
                                                              DT::dataTableOutput("SO2_Table", width = 600))))))),
                                      ### 2025 SO2 TRENDS TAB ----
                                      tabPanel(HTML(paste("2025 SO",tags$sub("2")," Trends")),
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       column(6,
                                                              p("2025 to Present - Mean Sulfur Dioxide Measurements by Site Location", 
                                                                style="font-size: 20px; font-weight: bold;"),
                                                              leafletOutput("SO2_2025Map", width=650, height=700)),
                                                       column(6,
                                                              div(id="SO2Trends2025",
                                                                  plotOutput("SO2_2025Trends", width = 650, height = 400)),
                                                              bsModal("popout27", "SO2_2025Trends", "SulfurDioxideTrends",
                                                                      size = "large", title = "2025 Daily Sulfur Dioxide Trends", plotOutput("SO2_2025Trends1")),
                                                              p(),
                                                              div(id="SO2Dist",
                                                                  plotOutput("SO2_boxplots2025", width = 600, height = 200)),
                                                              bsModal("popout28", "SO2_boxplots2025", "SO2Dist",
                                                                      size = "large", title = "2025 Sulfur Dioxide Distributions", plotOutput("SO2_boxplots2025_1")),
                                                              DT::dataTableOutput("SO2_DistTable", width = 600)))))),
                                      ### 2024 SO2 TRENDS TAB ----
                                      tabPanel(HTML(paste("2024 SO",tags$sub("2")," Trends")),
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       column(6,
                                                              p("2024 Mean Air Quality Index by Site Location", style="font-size: 20px; font-weight: bold;"),
                                                              leafletOutput("SO2_2024Map", width=650, height=600),
                                                              DT::dataTableOutput("SO2_Table_2024", width = 650)),
                                                       column(6,
                                                              div(id="SO2Trends", 
                                                                  plotOutput("SO2_2024Trends", width = 650, height = 525)),
                                                              bsModal("popout7", "SO2_2024Trends", "SO2Trends", 
                                                                      size = "large", title = "2024 Daily Sulfur Dioxide Trends", plotOutput("SO2_2024Trends1")),
                                                              div(id="SO2Dist", 
                                                                  plotOutput("SO2_boxplots", width = 620, height = 200)),
                                                              bsModal("popout8", "SO2_boxplots", "SO2Dist", 
                                                                      size = "large", title = "2024 Sulfur Dioxide Distributions", plotOutput("SO2_boxplots1")),
                                                              tags$img(src="SO2_DistTable.jpg", width = "600", 
                                                                       style="padding-left:70px; padding-top: 20px;"))))))))),
                     ######## PM2.5 PAGE -----------
                     tabItem('PM25_map',
                             fluidPage(
                               tabBox(width=12,
                                      selected= "Current PM 2.5 Measurements",
                                      tags$head(tags$style(HTML('.nav-tabs-custom>.nav-tabs {font-size: 18px;font-weight: bold;}'))),
                                      ### ABOUT PM2.5 TAB -----
                                      tabPanel("About Particulate Matter (PM) 2.5",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       fluidRow(
                                                         column(9,
                                                                p("Particulate Matter (PM) 2.5 is the term for the fine, inhalable mixture of solid particles and liquid droplets in the atmosphere. Also called particle pollution, the “2.5” refers to particle diameter of 2.5 micrometers or smaller (for size comparison, the average human hair is approximately 70 micrometers in diameter). The particles may be any number of hundreds of different pollutants, including dust, dirt, soot, smoke, heavy metals, and chemicals such as nitrogen oxides (NOx).",
                                                                  style="font-size:16px;"),
                                                                p("Particulate matter pollution comes from a wide variety of sources. It may be released directly, such as from smokestacks, construction sites, fires, and automobile emissions, but can also be created through chemical reactions in the atmosphere, such as sulfur dioxide pollutants reacting with water vapor and oxygen to form sulfate particles. The largest source of particle pollution is the combustion of carbon-based fuels: wood stoves, diesel or gasoline motor vehicles, power plants, factories, wildfires, and agricultural fires all release fine particles into the atmosphere.",
                                                                  style="font-size:16px;")),
                                                         column(3,
                                                                tags$img(src='PM25.jpg', height = 200),
                                                                p())),
                                                       fluidRow(
                                                         column(5,
                                                                tags$img(src='PM25_lungs.jpg', height = 400)),
                                                         column(7,
                                                                p("Particulate matter pollution can have negative environmental effects, especially depending on its composition. PM 2.5 are the primary cause of atmospheric haze in the United States, affecting how light is absorbed or scattered, and leading to reduced visibility. Some particles, such as black carbon, contribute to climate warming, while others contribute to water and soil acidification, cause damage to vegetation, wildlife, and ecosystems, or impact water quality. In addition to the damaging effects of chemicals on plants, particulate matter can clog their stomatal openings, interfering with photosynthesis, stunting growth, and reducing crop yields. Wind can carry the fine particles great distances from emission sources, resulting in impacts to a wide range of communities and ecosystems, including even remote or rural areas, national parks, and wilderness areas.",
                                                                  style="font-size:16px;"),
                                                                p("There is considerable evidence of the numerous adverse health effects of exposure to PM 2.5. Due to their small size, inhaled particles can get deep into the lungs or even enter the bloodstream. Short-term exposure to PM 2.5 pollution has been linked to premature mortality, reduced lung function, and an increase in asthma, heart- and lung-related hospital admissions, acute and chronic bronchitis, and nonfatal heart attacks. Long-term exposure is associated with chronic kidney disease (CKD), worsening asthma and chronic obstructive pulmonary disease (COPD), premature death, reduced lung function growth in children, and neurodegenerative diseases such as Alzheimer’s and Parkinson’s diseases. Exposure during pregnancy has been associated with negative birth outcomes, including preterm birth, low birth rate, and post neonatal infant mortality.",
                                                                  style="font-size:16px;"),
                                                                p(paste("Particulate matter pollution is typically measured in micrograms per cubic meter (µg/m³). The EPA first set standards relating to particulate matter pollution in 1971. They have been revised multiple times, most recently in February 2024. The current National Ambient Air Quality Standards for PM 2.5 are:"), 
                                                                  style="font-size:16px;"),
                                                                p("- 9.0 micrograms per cubic meter, as an annual arithmetic mean, averaged over 3 years (annual standard)", 
                                                                  style="font-size:16px; margin-left: 40px;"),
                                                                p("- 35 micrograms per cubic meter, as an average concentration over a 24-hour period (24-hour standard)", 
                                                                  style="font-size:16px; margin-left: 40px; margin-top: -10px;"),
                                                                p("As of 2024, there were nearly 1,000 PM 2.5 monitoring stations across the US that report concentration measurements to the EPA.", style="font-size:16px;")))),
                                                   box(width = 12,
                                                       p(strong("Visit the following links for more information:"), 
                                                         style="font-size:18px;"),
                                                       p(tags$a(href="https://www.epa.gov/pm-pollution/particulate-matter-pm-basics", 
                                                                "EPA - Particulate Matter (PM) Basics"),
                                                         style="font-size:16px;"),
                                                       p(tags$a(href="https://www.epa.gov/pm-pollution/health-and-environmental-effects-particulate-matter-pm", 
                                                                "EPA - Health and Environmental Effects of Particulate Matter (PM)"),
                                                         style="font-size:16px;"),
                                                       p(tags$a(href="https://www.cdc.gov/air-quality/pollutants/index.html#cdc_listing_add_info-additional-information-on-ozone-particulate-matter", 
                                                                "CDC - Air Pollutants"),
                                                         style="font-size:16px;"))))),
                                      ### PM2.5 CURRENT MEASUREMENTS TAB --------------
                                      tabPanel("Current PM 2.5 Measurements",
                                               verticalLayout(
                                                 fluidRow(
                                                   column(4,
                                                          wellPanel(textOutput("zip5"),  style= "font-size:18px;")),
                                                   column(3,
                                                          plotOutput("PM25_AQIbar", width = 330, height = 80)),
                                                   column(5,
                                                          wellPanel(p(strong(tags$a(href="https://www.epa.gov/criteria-air-pollutants/naaqs-table",
                                                                                    "Link to National Ambient Air Quality Standards")), 
                                                                      style = "text-align:center; font-size:20px;")))),
                                                 mainPanel(
                                                   fluidRow(
                                                     column(10,
                                                            leafletOutput("PM25", height = 650)),
                                                     column(2,
                                                            fluidRow(
                                                              plotOutput("PM25_Gauge", width = 600, height = 280)),
                                                            fluidRow(
                                                              plotOutput("PM25_Plot", width = 600, height = 130)),
                                                            fluidRow(
                                                              DT::dataTableOutput("PM25_Table", width = 600))))))),
                                      ### 2025 PM2.5 TRENDS TAB ----
                                      tabPanel("2025 PM 2.5 Trends",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       column(6,
                                                              p("2025 to Present - Mean PM 2.5 Measurements by Site Location", 
                                                                style="font-size: 20px; font-weight: bold;"),
                                                              leafletOutput("PM25_2025Map", width=650, height=700)),
                                                       column(6,
                                                              div(id="PM25Trends2025",
                                                                  plotOutput("PM25_2025Trends", width = 650, height = 400)),
                                                              bsModal("popout29", "PM25_2025Trends", "PM25Trends",
                                                                      size = "large", title = "2025 Daily PM 2.5 Trends", plotOutput("PM25_2025Trends1")),
                                                              p(),
                                                              div(id="PM25Dist",
                                                                  plotOutput("PM25_boxplots2025", width = 600, height = 200)),
                                                              bsModal("popout30", "PM25_boxplots2025", "PM25Dist",
                                                                      size = "large", title = "2025 PM 2.5 Distributions", plotOutput("PM25_boxplots2025_1")),
                                                              DT::dataTableOutput("PM25_DistTable", width = 600)))))),
                                      ### 2024 PM2.5 TRENDS TAB ----
                                      tabPanel("2024 PM 2.5 Trends",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       column(6,
                                                              p("2024 Mean Air Quality Index by Site Location", style="font-size: 20px; font-weight: bold;"),
                                                              leafletOutput("PM25_2024Map", width=650, height=600),
                                                              DT::dataTableOutput("PM25_Table_2024", width = 650)),
                                                       column(6,
                                                              div(id="PM25Trends", 
                                                                  plotOutput("PM25_2024Trends", width = 650, height = 525)),
                                                              bsModal("popout9", "PM25_2024Trends", "PM25Trends", 
                                                                      size = "large", title = "2024 Daily PM 2.5 Trends", plotOutput("PM25_2024Trends1")),
                                                              div(id="PM25Dist", 
                                                                  plotOutput("PM25_boxplots", width = 620, height = 200)),
                                                              bsModal("popout10", "PM25_boxplots", "PM25Dist", 
                                                                      size = "large", title = "2024 PM 2.5 Distributions", plotOutput("PM25_boxplots1")),
                                                              tags$img(src="PM25_DistTable.jpg", width = "600", 
                                                                       style="padding-left:70px; padding-top: 20px;"))))))))),
                     ######## PM10 PAGE -------------
                     tabItem('PM10_map',
                             fluidPage(
                               tabBox(width=12,
                                      selected= "Current PM 10 Measurements",
                                      tags$head(tags$style(HTML('.nav-tabs-custom>.nav-tabs {font-size: 18px;font-weight: bold;}'))),
                                      ### ABOUT PM10 TAB -----
                                      tabPanel("About Particulate Matter (PM) 10",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       fluidRow(
                                                         column(8,
                                                                p(HTML(paste("Particulate Matter (PM) 10 is the term for the comparatively coarse, inhalable mixture of solid particles and liquid droplets in the atmosphere. Also called particle pollution, the “10” refers to particle diameter of 10 micrometers or smaller (for size comparison, the average human hair is approximately 70 micrometers in diameter). The particles may be any number of hundreds of different pollutants, including dust, dirt, soot, smoke, heavy metals, and chemicals such as nitrogen oxides (NO", tags$sub("x"),").")),
                                                                  style="font-size:16px;"),
                                                                p("Particulate matter pollution comes from a wide variety of sources. It may be released directly, such as from smokestacks, construction sites, fires, and automobile emissions, but can also be created through chemical reactions in the atmosphere, such as sulfur dioxide pollutants reacting with water vapor and oxygen to form sulfate particles. PM 10 can also come from the dust of roads, farms, dry riverbeds, and mines.  The largest source of particle pollution is the combustion of carbon-based fuels: wood stoves, diesel or gasoline motor vehicles, power plants, factories, wildfires, and agricultural fires all release fine particles into the atmosphere. ",
                                                                  style="font-size:16px;")),
                                                         column(4,
                                                                tags$img(src='PM10.jpg', height = 220),
                                                                p())),
                                                       fluidRow(
                                                         column(12,
                                                                p("Particulate matter pollution can have negative environmental effects, especially depending on its composition. Particulate matter affects how light is absorbed or scattered, reducing visibility. Some particles, such as black carbon, contribute to climate warming, while others contribute to water and soil acidification, cause damage to vegetation, wildlife, and ecosystems, or impact water quality. Wind can carry particles great distances from emission sources, resulting in impacts to a wide range of communities and ecosystems, including even remote or rural areas, national parks, and wilderness areas.",
                                                                  style="font-size:16px;"))),
                                                       fluidRow(
                                                         column(5,
                                                                tags$img(src='PM10_size.jpg', height = 380)),
                                                         column(7,
                                                                p("There is evidence of numerous adverse health effects as a result of exposure to PM 10. Due to their small size, inhaled particles can get deep into the lungs or even enter the bloodstream. Short-term exposure to PM 10 pollution can be irritating to the eyes, nose and throat, and has been linked to worsening of respiratory diseases such as asthma and chronic obstructive pulmonary disease (COPD), as well as increased hospitalizations. Long-term exposure may be linked to respiratory mortality and lung cancer. ",
                                                                  style="font-size:16px;"),
                                                                p(paste("Particulate matter pollution is typically measured in micrograms per cubic meter (µg/m³). The EPA first set standards relating to particulate matter pollution in 1971. They have been revised multiple times, most recently in 2012. The current National Ambient Air Quality Standards for PM 10 are:"), 
                                                                  style="font-size:16px; "),
                                                                p("-	150 micrograms per cubic meter, as an average concentration over a 24-hour period, not to be exceeded more than once per year on average over 3 years (24-hour standard)", 
                                                                  style="font-size:16px; margin-left: 40px; margin-top: -10px;")))),
                                                   box(width = 12,
                                                       p(strong("Visit the following links for more information:"), 
                                                         style="font-size:18px;"),
                                                       p(tags$a(href="https://www.epa.gov/pm-pollution/particulate-matter-pm-basics", 
                                                                "EPA - Particulate Matter (PM) Basics"),
                                                         style="font-size:16px;"),
                                                       p(tags$a(href="https://www.epa.gov/pm-pollution/health-and-environmental-effects-particulate-matter-pm", 
                                                                "EPA - Health and Environmental Effects of Particulate Matter (PM)"),
                                                         style="font-size:16px;"),
                                                       p(tags$a(href="https://ww2.arb.ca.gov/resources/inhalable-particulate-matter-and-health?keywords=2025#:~:text=Short%2Dterm%20exposures%20to%20PM10,air%20pollution%20causes%20lung%20cancer.", 
                                                                "California Air Resources Board - Inhalable Particulate Matter and Health (PM2.5 and PM10)"),
                                                         style="font-size:16px;"))))),
                                      ### CURRENT PM10 MEASUREMENTS TAB ------
                                      tabPanel("Current PM 10 Measurements",
                                               verticalLayout(
                                                 fluidRow(
                                                   column(4,
                                                          wellPanel(textOutput("zip6"),  style= "font-size:18px;")),
                                                   column(3,
                                                          plotOutput("PM10_AQIbar", width = 330, height = 80)),
                                                   column(5,
                                                          wellPanel((p(strong(tags$a(href="https://www.epa.gov/criteria-air-pollutants/naaqs-table",
                                                                                     "Link to National Ambient Air Quality Standards")), 
                                                                       style = "text-align:center; font-size:20px;"))))),
                                                 mainPanel(
                                                   fluidRow(
                                                     column(10,
                                                            leafletOutput("PM10", height = 650)),
                                                     column(2,
                                                            fluidRow(
                                                              plotOutput("PM10_Gauge", width = 600, height = 280)),
                                                            fluidRow(
                                                              plotOutput("PM10_Plot", width = 600, height = 130)),
                                                            fluidRow(
                                                              DT::dataTableOutput("PM10_Table", width = 600))))))),
                                      ### 2025 PM10 TRENDS TAB ----
                                      tabPanel("2025 PM 10 Trends",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       column(6,
                                                              p("2025 to Present - Mean PM 10 Measurements by Site Location", 
                                                                style="font-size: 20px; font-weight: bold;"),
                                                              leafletOutput("PM10_2025Map", width=650, height=700)),
                                                       column(6,
                                                              div(id="PM10Trends2025",
                                                                  plotOutput("PM10_2025Trends", width = 650, height = 400)),
                                                              bsModal("popout31", "PM10_2025Trends", "PM10Trends",
                                                                      size = "large", title = "2025 Daily PM 10 Trends", plotOutput("PM10_2025Trends1")),
                                                              p(),
                                                              div(id="PM10Dist",
                                                                  plotOutput("PM10_boxplots2025", width = 600, height = 200)),
                                                              bsModal("popout32", "PM10_boxplots2025", "PM10Dist",
                                                                      size = "large", title = "2025 PM 10 Distributions", plotOutput("PM10_boxplots2025_1")),
                                                              DT::dataTableOutput("PM10_DistTable", width = 600)))))),
                                      ### 2024 PM10 TRENDS TAB ----
                                      tabPanel("2024 PM 10 Trends",
                                               verticalLayout(
                                                 fluidRow(
                                                   box(width = 12,
                                                       column(6,
                                                              p("2024 Mean Air Quality Index by Site Location", style="font-size: 20px; font-weight: bold;"),
                                                              leafletOutput("PM10_2024Map", width=650, height=600),
                                                              DT::dataTableOutput("PM10_Table_2024", width = 650)),
                                                       column(6,
                                                              div(id="PM10Trends", 
                                                                  plotOutput("PM10_2024Trends", width = 650, height = 525)),
                                                              bsModal("popout11", "PM10_2024Trends", "PM10Trends", 
                                                                      size = "large", title = "2024 Daily PM 10 Trends", plotOutput("PM10_2024Trends1")),
                                                              div(id="PM10Dist", 
                                                                  plotOutput("PM10_boxplots", width = 620, height = 200)),
                                                              bsModal("popout12", "PM10_boxplots", "PM10Dist", 
                                                                      size = "large", title = "2024 PM 10 Distributions", plotOutput("PM10_boxplots1")),
                                                              tags$img(src="PM10_DistTable.jpg", width = "600", 
                                                                       style="padding-left:70px; padding-top: 20px;"))))))))))))
##### End of Dashboard Layout -----
##### Dashboard Input/Outputs -----
server = function(input, output, session){
  
  #####  AQI PAGE OUTPUTS -----
  
  output$AQI_Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = AirQualityDataMap,
                       fillColor = ifelse(AirQualityDataMap$MeanAQI<=50, "green", ifelse(
                         AirQualityDataMap$MeanAQI<=100, "yellow", ifelse(
                           AirQualityDataMap$MeanAQI<=150,  "orange", ifelse(
                             AirQualityDataMap$MeanAQI<=200,  "red", ifelse(
                               AirQualityDataMap$MeanAQI<300, "#8f3f97"))))), 
                       fill = AirQualityDataMap$MeanAQI, 
                       fillOpacity = 0.55, 
                       stroke = FALSE,  
                       radius = ~rescale(AirQualityDataMap$MeanAQI, c(5,10)),
                       label = ~paste(AirQualityDataMap$SITE_NAME, AirQualityDataMap$MeanAQI, sep = ": "),
                       popup = ~Label, 
                       labelOptions = labelOptions(textsize = "14px", noHide = T, textOnly = F, opacity = .75, na.label = "-"))
  })
  
  output$AQI_Table <- renderDataTable({
    DT::datatable(AQI_Levels,
                  rownames=F,
                  colnames = c("Index Value", "Level of Concern", "Air Quality Description"),
                  options = list(paging=F,
                                 searching=F,
                                 ordering=F,
                                 responsive = F,
                                 autowidth=T,
                                 columnDefs= list(list(width = '100px', targets= "Index.Value"),
                                                  list(className = 'dt-center', targets = "_all")),
                                 class = "display",
                                 headerCallback = DT::JS(
                                   "function(thead) {",
                                   "  $(thead).css('font-size', '16px');",
                                   "}"
                                 ))) %>% 
      formatStyle("Index.Value",
                  target = "row",
                  backgroundColor = styleEqual(c("0 to 50", "51 to 100", "101 to 150", "151 to 200", "201 to 300", "300 +"),
                                               c("#00e400", "yellow", "orange", "red", "#8f3f97", "#7e0023")),
                  color = styleEqual(c("151 to 200","201 to 300", "300 +"), "white")) -> AQI_LevelsDisplay
    AQI_LevelsDisplay})
  
  #####  CURRENT MEASUREMENT TAB OUTPUTS ------
  #####  CURRENT MEASUREMENT TIMESTAMP OUTPUTS -------
  output$zip7 <- output$zip6 <-output$zip5 <- output$zip4 <- output$zip3 <- output$zip2 <- output$zip1 <- renderText(
    {paste("Measurements Last Updated:", Timestamp, sep = '\n')
    })
  
  #####  CURRENT MEASUREMENTS AQI BARS -----   
  ############# CO AQI 
  output$CO_AQIbar <- renderPlot({
    ggplot(AQI)+
      geom_rect(aes(xmin=minvalue, xmax = maxvalue, ymin = 0, ymax = 25, fill = RiskLevel))+
      scale_fill_manual(values = color)+
      scale_color_identity()+
      new_scale_color()+
      coord_fixed()+
      theme_bw()+
      geom_label(aes(x = CO_AQI, y = 5, label = CO_AQI), size = 5.5, vjust = 0.1,  
                 fill = ifelse(CO_AQI<=50, "green", ifelse(
                   CO_AQI<=100, "yellow", ifelse(
                     CO_AQI<=150,  "orange", ifelse(
                       CO_AQI<=200,  "red", ifelse(
                         CO_AQI<300, "#8f3f97"))))))+
      scale_x_continuous(breaks = c(0, AQI$maxvalue))+
      theme(panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            axis.text.y = element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank(),
            plot.title.position = 'plot',
            plot.title = element_text(face = "bold",
                                      hjust=0.5, 
                                      size = 18),
            legend.position="none",
            axis.text.x = element_text(margin = margin(t = -3), size = 12))+
      labs(title = "Air Quality Index (AQI) Value")
  }, bg="transparent")
  
  ############# NO2 AQI
  output$NO2_AQIbar <- renderPlot({
    ggplot(AQI)+
      geom_rect(aes(xmin=minvalue, xmax = maxvalue, ymin = 0, ymax = 25, fill = RiskLevel))+
      scale_fill_manual(values = color)+
      scale_color_identity()+
      new_scale_color()+
      coord_fixed()+
      theme_bw()+
      geom_label(aes(x = NO2_AQI, y = 5, label = NO2_AQI), size = 5.5, vjust = 0.1,  
                 fill = ifelse(NO2_AQI<=50, "green", ifelse(
                   NO2_AQI<=100, "yellow", ifelse(
                     NO2_AQI<=150,  "orange", ifelse(
                       NO2_AQI<=200,  "red", ifelse(
                         NO2_AQI<300, "#8f3f97"))))))+
      scale_x_continuous(breaks = c(0, AQI$maxvalue))+
      theme(panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            axis.text.y = element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank(),
            plot.title.position = 'plot',
            plot.title = element_text(face = "bold",
                                      hjust=0.5, 
                                      size = 18),
            legend.position="none",
            axis.text.x = element_text(margin = margin(t = -3), size = 12))+
      labs(title = "Air Quality Index (AQI) Value")
  }, bg="transparent")
  
  ############# Ozone AQI
  output$O3_AQIbar <- renderPlot({
    ggplot(AQI)+
      geom_rect(aes(xmin=minvalue, xmax = maxvalue, ymin = 0, ymax = 25, fill = RiskLevel))+
      scale_fill_manual(values = color)+
      scale_color_identity()+
      new_scale_color()+
      coord_fixed()+
      theme_bw()+
      geom_label(aes(x = O3_AQI, y = 5, label = O3_AQI), size = 5.5, vjust = 0.1,  
                 fill = ifelse(O3_AQI<=50, "green", ifelse(
                   O3_AQI<=100, "yellow", ifelse(
                     O3_AQI<=150,  "orange", ifelse(
                       O3_AQI<=200,  "red", ifelse(
                         O3_AQI<300, "#8f3f97"))))))+
      scale_x_continuous(breaks = c(0, AQI$maxvalue))+
      theme(panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            axis.text.y = element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank(),
            plot.title.position = 'plot',
            plot.title = element_text(face = "bold",
                                      hjust=0.5, 
                                      size = 18),
            legend.position="none",
            axis.text.x = element_text(margin = margin(t = -3), size = 12))+
      labs(title = "Air Quality Index (AQI) Value")
  }, bg="transparent")
  
  ############# SO2 AQI
  output$SO2_AQIbar <- renderPlot({
    ggplot(AQI)+
      geom_rect(aes(xmin=minvalue, xmax = maxvalue, ymin = 0, ymax = 25, fill = RiskLevel))+
      scale_fill_manual(values = color)+
      scale_color_identity()+
      new_scale_color()+
      coord_fixed()+
      theme_bw()+
      geom_label(aes(x = SO2_AQI, y = 5, label = SO2_AQI), size = 5.5, vjust = 0.1,  
                 fill = ifelse(SO2_AQI<=50, "green", ifelse(
                   SO2_AQI<=100, "yellow", ifelse(
                     SO2_AQI<=150,  "orange", ifelse(
                       SO2_AQI<=200,  "red", ifelse(
                         SO2_AQI<300, "#8f3f97"))))))+
      scale_x_continuous(breaks = c(0, AQI$maxvalue))+
      theme(panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            axis.text.y = element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank(),
            plot.title.position = 'plot',
            plot.title = element_text(face = "bold",
                                      hjust=0.5, 
                                      size = 18),
            legend.position="none",
            axis.text.x = element_text(margin = margin(t = -3), size = 12))+
      labs(title = "Air Quality Index (AQI) Value")
  }, bg="transparent")
  
  ############# PM25 AQI
  output$PM25_AQIbar <- renderPlot({
    ggplot(AQI)+
      geom_rect(aes(xmin=minvalue, xmax = maxvalue, ymin = 0, ymax = 25, fill = RiskLevel))+
      scale_fill_manual(values = color)+
      scale_color_identity()+
      new_scale_color()+
      coord_fixed()+
      theme_bw()+
      geom_label(aes(x = PM25_AQI, y = 5, label = PM25_AQI), size = 5.5, vjust = 0.1,  
                 fill = ifelse(PM25_AQI<=50, "green", ifelse(
                   PM25_AQI<=100, "yellow", ifelse(
                     PM25_AQI<=150,  "orange", ifelse(
                       PM25_AQI<=200,  "red", ifelse(
                         PM25_AQI<300, "#8f3f97"))))))+
      scale_x_continuous(breaks = c(0, AQI$maxvalue))+
      theme(panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            axis.text.y = element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank(),
            plot.title.position = 'plot',
            plot.title = element_text(face = "bold",
                                      hjust=0.5, 
                                      size = 18),
            legend.position="none",
            axis.text.x = element_text(margin = margin(t = -3), size = 12))+
      labs(title = "Air Quality Index (AQI) Value")
  }, bg="transparent")
  
  ############# PM10 AQI 
  output$PM10_AQIbar <- renderPlot({
    ggplot(AQI)+
      geom_rect(aes(xmin=minvalue, xmax = maxvalue, ymin = 0, ymax = 25, fill = RiskLevel))+
      scale_fill_manual(values = color)+
      scale_color_identity()+
      new_scale_color()+
      coord_fixed()+
      theme_bw()+
      geom_label(aes(x = PM10_AQI, y = 5, label = PM10_AQI), size = 5.5, vjust = 0.1,  
                 fill = ifelse(PM10_AQI<=50, "green", ifelse(
                   PM10_AQI<=100, "yellow", ifelse(
                     PM10_AQI<=150,  "orange", ifelse(
                       PM10_AQI<=200,  "red", ifelse(
                         PM10_AQI<300, "#8f3f97"))))))+
      scale_x_continuous(breaks = c(0, AQI$maxvalue))+
      theme(panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            axis.text.y = element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank(),
            plot.title.position = 'plot',
            plot.title = element_text(face = "bold",
                                      hjust=0.5, 
                                      size = 18),
            legend.position="none",
            axis.text.x = element_text(margin = margin(t = -3), size = 12))+
      labs(title = "Air Quality Index (AQI) Value")
  }, bg="transparent")
  
  #####  CURRENT MEASUREMENTS MAPS ------
  ### Carbon Monoxide MAP ----
  output$CO <-  renderLeaflet(
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = CarbMono, 
                       fillColor = ifelse(CarbMono$CARBON_MONOXIDE_PPM<=50, "#999933", ifelse(
                         CarbMono$CARBON_MONOXIDE_PPM<=100, "#DDCC77", ifelse(
                           CarbMono$CARBON_MONOXIDE_PPM<=150,  "#CC6677", ifelse(
                             CarbMono$CARBON_MONOXIDE_PPM<=200,  "#882255", ifelse(
                               CarbMono$CARBON_MONOXIDE_PPM<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(CARBON_MONOXIDE_PPM, c(5,10)), 
                       popup = ~as.character(CarbMono$SITE_NAME),
                       label = ~paste(sprintf("%.2f",CARBON_MONOXIDE_PPM), "PPM", sep = " "), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .75, na.label = "")))
  observeEvent(input$CO_marker_click, {
    leafletProxy("CO", session) %>%
      removeMarker(input$CO_marker_click$id)
  })
  ### Nitrogen Dioxide MAP ----
  output$NO2  <-  renderLeaflet(
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = NO2dat, 
                       fillColor = ifelse(NO2dat$NITROGEN_DIOXIDE_PPM<=50, "#999933", ifelse(
                         NO2dat$NITROGEN_DIOXIDE_PPM<=100, "#DDCC77", ifelse(
                           NO2dat$NITROGEN_DIOXIDE_PPM<=150,  "#CC6677", ifelse(
                             NO2dat$NITROGEN_DIOXIDE_PPM<=200,  "#882255", ifelse(
                               NO2dat$NITROGEN_DIOXIDE_PPM<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(NITROGEN_DIOXIDE_PPM, c(5,10)), 
                       popup = ~as.character(NO2dat$SITE_NAME),
                       label = ~paste(sprintf("%.2f",NITROGEN_DIOXIDE_PPM), "PPB", sep = " "), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .75, na.label = "")))
  observeEvent(input$NO2_marker_click, {
    leafletProxy("NO2", session) %>%
      removeMarker(input$NO2_marker_click$id)
  })
  ### Ozone MAP ----
  output$O3  <-  renderLeaflet(
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = Ozone, 
                       fillColor = ifelse(Ozone$OZONE_PPM<=50, "#999933", ifelse(
                         Ozone$OZONE_PPM<=100, "#DDCC77", ifelse(
                           Ozone$OZONE_PPM<=150,  "#CC6677", ifelse(
                             Ozone$OZONE_PPM<=200,  "#882255", ifelse(
                               Ozone$OZONE_PPM<300, "#330033"))))),
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(OZONE_PPM, c(5,10)), 
                       popup = ~as.character(Ozone$SITE_NAME),
                       label = ~paste(sprintf("%.2f",OZONE_PPM), "PPM", sep = " "), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .75, na.label = "")))
  observeEvent(input$O3_marker_click, {
    leafletProxy("O3", session) %>%
      removeMarker(input$O3_marker_click$id)
  })
  ### Sulfur Dioxide MAP ----
  output$SO2  <-  renderLeaflet(
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = SO2dat, 
                       fillColor = ifelse(SO2dat$SULFUR_DIOXIDE_PPB<=50, "#999933", ifelse(
                         SO2dat$SULFUR_DIOXIDE_PPB<=100, "#DDCC77", ifelse(
                           SO2dat$SULFUR_DIOXIDE_PPB<=150,  "#CC6677", ifelse(
                             SO2dat$SULFUR_DIOXIDE_PPB<=200,  "#882255", ifelse(
                               SO2dat$SULFUR_DIOXIDE_PPB<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(SULFUR_DIOXIDE_PPB, c(5,10)), 
                       popup = ~as.character(SO2dat$SITE_NAME),
                       label = ~paste(sprintf("%.2f",SULFUR_DIOXIDE_PPB), "PPB", sep = " "), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .75, na.label = "")))
  observeEvent(input$SO2_marker_click, {
    leafletProxy("SO2", session) %>%
      removeMarker(input$SO2_marker_click$id)
  })
  ### PM2.5 MAP ----
  output$PM25 <- renderLeaflet(
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = PM25dat, 
                       fillColor = ifelse(PM25dat$PM25_UG_M3<=50, "#999933", ifelse(
                         PM25dat$PM25_UG_M3<=100, "#DDCC77", ifelse(
                           PM25dat$PM25_UG_M3<=150,  "#CC6677", ifelse(
                             PM25dat$PM25_UG_M3<=200,  "#882255", ifelse(
                               PM25dat$PM25_UG_M3<300, "#330033"))))),
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(PM25_UG_M3, c(5,10)), 
                       popup = ~as.character(PM25dat$SITE_NAME),
                       label = ~paste(sprintf("%.2f",PM25_UG_M3), "ug/m³", sep = " "), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .75, na.label = "")))
  observeEvent(input$PM25_marker_click, {
    leafletProxy("PM25", session) %>%
      removeMarker(input$PM25_marker_click$id)
  })
  ### PM10 MAP ----
  output$PM10 <- renderLeaflet(
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = PM10dat, 
                       fillColor = ifelse(PM10dat$PM10_UG_M3<=50, "#999933", ifelse(
                         PM10dat$PM10_UG_M3<=100, "#DDCC77", ifelse(
                           PM10dat$PM10_UG_M3<=150,  "#CC6677", ifelse(
                             PM10dat$PM10_UG_M3<=200,  "#882255", ifelse(
                               PM10dat$PM10_UG_M3<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(PM10_UG_M3, c(5,10)), 
                       popup = ~as.character(PM10dat$SITE_NAME),
                       label = ~paste(sprintf("%.2f",PM10_UG_M3), "ug/m³", sep = " "), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .75, na.label = "")))
  observeEvent(input$PM10_marker_click, {
    leafletProxy("PM10", session) %>%
      removeMarker(input$PM10_marker_click$id)
  })
  
  
  
  #####  CURRENT MEASUREMENT GAUGE & TABLE OUTPUTS ------
  ### CARBON MONOXIDE------
  ########### CO Table
  output$CO_Table <- DT::renderDataTable({
    DT::datatable(COOutput, rownames = F,
                  extensions = 'Buttons',
                  options = list(
                    paging = F,
                    searching = F,
                    fixedColumns = TRUE,
                    autoWidth = F,
                    dom = 'Bfrtip',
                    ordering = TRUE,
                    buttons = list(
                      list(extend = 'csv', filename = "PhiladelphiaCOmeasurements"),
                      list(extend = 'excel', filename = "PhiladelphiaCOmeasurements")),
                    pagelength=6),
                  class="display")},server=F)
  
  ########## CO Plot
  output$CO_Plot <- renderPlot({
    par(mar=c(0,5,1,1),xpd=T)
    par(oma=c(0,3,0,3))
    CO_Plot <- function(x,y,legend_text){
      plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
      L = legend("topleft", legend = risklevels, pch=15, pt.cex=3, cex=1.2, bty='n', col = colors)
      legend(x = L$rect$left, y = L$rect$top, legend = CO_riskvalues, col=rep(NA,2), cex=1.2, lty=c(1,1), ncol=1, x.intersp = 17, bg = NA)
    }
    CO_Plot()
  })
  
  ######### CO Gauge
  output$CO_Gauge <- renderPlot({
    gg.gauge <- function(pos,breaks = CO_breaks) {
      get.poly <- function(a,b,r1=0.35,r2=1.0) {
        th.start <- pi*(1-a/CO_max)
        th.end   <- pi*(1-b/CO_max)
        th       <- seq(th.start,th.end,length=CO_max)
        x        <- c(r1*cos(th),rev(r2*cos(th)))
        y        <- c(r1*sin(th),rev(r2*sin(th)))
        return(data.frame(x,y))
      }
      ggplot()+ 
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="#999933")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="#DDCC77")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="#CC6677")+
        geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="#882255")+
        geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="#330033")+
        geom_polygon(data=get.poly(pos-0.1,pos+0.1,0),aes(x,y))+
        geom_text_repel(data=as.data.frame(breaks), size=5, 
                        aes(x=cos(pi*(1-breaks/CO_max)),
                            y=0.875*sin(pi*(1-breaks/CO_max)),
                            label="",
                            bg.color = "white", 
                            bg.r = 0.2))+
        annotate("text",x=0,y=0,label=pos,vjust=0,size=9)+
        coord_fixed()+
        theme_bw()+
        labs(title ="Most Recent Mean Carbon Monoxide Measurements",
             subtitle = "")+ 
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank(),
              plot.title.position = 'plot',
              plot.title = element_text(face = "bold", 
                                        hjust=0.5, 
                                        size = 18)) 
    }
    gg.gauge(MeanCOround,breaks=CO_breaks)
  })
  
  
  ### NITROGEN DIOXIDE------    
  ######### NO2 Table
  output$NO2_Table <- DT::renderDataTable({
    DT::datatable(NO2Output, rownames = F,
                  extensions = 'Buttons',
                  options = list(
                    paging = F,
                    searching = F,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    dom = 'Bfrtip',
                    ordering = TRUE,
                    buttons = list(
                      list(extend = 'csv', filename = "PhiladelphiaNO2measurements"),
                      list(extend = 'excel', filename = "Philadelphia2NO2measuremens")),
                    pagelength=6),
                  class="display")},server=F)
  
  ############# NO2 Plot
  output$NO2_Plot <- renderPlot({
    par(mar=c(0,5,1,1),xpd=T)
    par(oma=c(0,3,0,3))
    NO2_Plot <- function(x,y,legend_text){
      plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
      L = legend("topleft", legend = risklevels, pch=15, pt.cex=3, cex=1.2, bty='n', col = colors)
      legend(x = L$rect$left, y = L$rect$top, legend = NO2_riskvalues, col=rep(NA,2), cex=1.2, lty=c(1,1), ncol=1, x.intersp = 17, bg = NA)
    }
    NO2_Plot()
  })
  
  ########### NO2 Gauge
  output$NO2_Gauge <- renderPlot({
    gg.gauge1 <- function(pos,breaks = NO2_breaks) {
      require(ggplot2)
      get.poly <- function(a,b,r1=0.35,r2=1.0) {
        th.start <- pi*(1-a/NO2_max)
        th.end   <- pi*(1-b/NO2_max)
        th       <- seq(th.start,th.end,length=NO2_max)
        x        <- c(r1*cos(th),rev(r2*cos(th)))
        y        <- c(r1*sin(th),rev(r2*sin(th)))
        return(data.frame(x,y))
      }
      ggplot()+ 
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="#999933")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="#DDCC77")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="#CC6677")+
        geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="#882255")+
        geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="#330033")+
        geom_polygon(data=get.poly(pos-3,pos+3,0),aes(x,y))+
        geom_text_repel(data=as.data.frame(breaks), size=5, 
                        aes(x=cos(pi*(1-breaks/NO2_max)),
                            y=sin(pi*(1-breaks/NO2_max))-0.01,
                            label="",
                            bg.color = "white", 
                            bg.r = 0.2))+
        annotate("text",x=0,y=0,label=pos,vjust=0,size=9)+
        coord_fixed()+
        theme_bw()+
        labs(title ="Most Recent Mean Nitrogen Dioxide Measurements",
             subtitle = "")+
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank(),
              plot.title.position = 'plot',
              plot.title = element_text(face = "bold",
                                        hjust=0.5,
                                        size = 18))
    }
    gg.gauge1(MeanNO2round,breaks=NO2_breaks)
  })
  
  ### OZONE -------------------------
  ########## Ozone Table
  output$Ozone_Table <- DT::renderDataTable({
    DT::datatable(OzoneOutput, 
                  rownames = F,
                  extensions = 'Buttons',
                  options = list(
                    paging = F,
                    searching = F,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    dom = 'Bfrtip',
                    ordering = TRUE,
                    buttons = list(
                      list(extend = 'csv', filename = "PhiladelphiaO3measurements"),
                      list(extend = 'excel', filename = "PhiladelphiaO3measurements")),
                    pagelength=6),
                  class="display")},server=F)
  
  ########## Ozone Plot
  output$Ozone_Plot <- renderPlot({
    par(mar=c(0,4,1,1),xpd=T)
    par(oma=c(0,3,0,3))
    O3_Plot <- function(x,y,legend_text){
      plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
      L = legend("topleft", legend = risklevels, pch=15, pt.cex=3, cex=1.2, bty='n', col = colors)
      legend(x = L$rect$left, y = L$rect$top, legend = Ozone_riskvalues, col=rep(NA,2), cex=1.2, lty=c(1,1), ncol=1, x.intersp = 17, bg = NA)
    }
    O3_Plot()
  })
  
  ###### Ozone Gauge
  output$Ozone_Gauge <- renderPlot({
    gg.gauge <- function(pos,breaks = Ozone_breaks) {
      get.poly <- function(a,b,r1=0.35,r2=1.0) {
        th.start <- pi*(1-a/Ozone_max)
        th.end   <- pi*(1-b/Ozone_max)
        th       <- seq(th.start,th.end,length=Ozone_max)
        x        <- c(r1*cos(th),rev(r2*cos(th)))
        y        <- c(r1*sin(th),rev(r2*sin(th)))
        return(data.frame(x,y))
      }
      ggplot()+ 
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="#999933")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="#DDCC77")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="#CC6677")+
        geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="#882255")+
        geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="#330033")+
        geom_polygon(data=get.poly(100*pos-0.1,100*pos+0.05,0),aes(x,y))+
        geom_text_repel(data=as.data.frame(breaks), size=5,
                        aes(x=cos(pi*(1-breaks/Ozone_max)),
                            y=0.875*sin(pi*(1-breaks/Ozone_max)),
                            label="",
                            bg.color = "white",
                            bg.r = 0.2))+
        annotate("text",x=0,y=0,label=pos,vjust=0,size=9)+
        coord_fixed()+
        theme_bw()+
        labs(title ="Most Recent Mean Ozone Measurements",
             subtitle = "")+ 
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank(),
              plot.title.position = 'plot',
              plot.title = element_text(face = "bold", 
                                        hjust=0.5, 
                                        size = 18))
    }
    gg.gauge(MeanOzoneroundOrig,breaks=Ozone_breaks)
  })
  
  ### SULFUR DIOXIDE------
  
  ########## Sulfur Dioxide Table
  output$SO2_Table <- DT::renderDataTable({
    DT::datatable(SO2Output, rownames = F,
                  extensions = 'Buttons',
                  options = list(
                    paging = F,
                    searching = F,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    dom = 'Bfrtip',
                    ordering = TRUE,
                    buttons = list(
                      list(extend = 'csv', filename = "PhiladelphiaSO2measurements"),
                      list(extend = 'excel', filename = "Philadelphia2SO2measurements")),
                    pagelength=6),
                  class="display")},server=F)
  
  ########## Sulfur Dioxide Plot
  output$SO2_Plot <- renderPlot({
    par(mar=c(0,5,1,1),xpd=T)
    par(oma=c(0,3,0,3))
    SO2_Plot <- function(x,y,legend_text){
      plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
      L = legend("topleft", legend = risklevels, pch=15, pt.cex=3, cex=1.2, bty='n', col = colors)
      legend(x = L$rect$left, y = L$rect$top, legend = SO2_riskvalues, col=rep(NA,2), cex=1.2, lty=c(1,1), ncol=1, x.intersp = 17, bg = NA)
    }
    SO2_Plot()
  })
  
  ########### Sulfur Dioxide Gauge    
  output$SO2_Gauge <- renderPlot({
    gg.gaugeSO2 <- function(pos,breaks = SO2_breaks) {
      require(ggplot2)
      get.poly <- function(a,b,r1=0.35,r2=1.0) {
        th.start <- pi*(1-a/So2_max)
        th.end   <- pi*(1-b/So2_max)
        th       <- seq(th.start,th.end,length=So2_max)
        x        <- c(r1*cos(th),rev(r2*cos(th)))
        y        <- c(r1*sin(th),rev(r2*sin(th)))
        return(data.frame(x,y))
      }
      ggplot()+
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="#999933")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="#DDCC77")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="#CC6677")+
        geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="#882255")+
        geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="#330033")+
        geom_polygon(data=get.poly(pos-1.5,pos+1.5,0),aes(x,y))+
        geom_text_repel(data=as.data.frame(breaks), size=5, 
                        aes(x=cos(pi*(1-breaks/So2_max)),
                            y=0.875*sin(pi*(1-breaks/So2_max)),
                            label="",
                            bg.color = "white", 
                            bg.r = 0.2))+
        annotate("text",x=0,y=0,label=pos,vjust=0,size=9)+
        coord_fixed()+
        theme_bw()+
        labs(title ="Most Recent Mean Sulfur Dioxide Measurements",
             subtitle = "")+ 
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank(),
              plot.title.position = 'plot',
              plot.title = element_text(face = "bold", 
                                        hjust=0.5, 
                                        size = 18)) 
    }
    gg.gaugeSO2(MeanSO2round,breaks=SO2_breaks)
  })
  
  ### PM 2.5 -------------
  
  ########## PM 2.5 Table
  output$PM25_Table <- DT::renderDataTable({
    DT::datatable(PM25Output, rownames = F,
                  extensions = 'Buttons',
                  options = list(
                    paging = F,
                    searching = F,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    dom = 'Bfrtip',
                    ordering = TRUE,
                    buttons = list(
                      list(extend = 'csv', filename = "PhiladelphiaPM25measurements"),
                      list(extend = 'excel', filename = "PhiladelphiaPM25measurements")),
                    pagelength=6),
                  class="display")},server=F)
  
  ########## PM 2.5 Plot
  output$PM25_Plot <- renderPlot({
    par(mar=c(0,4,1,1),xpd=T)
    par(oma=c(0,3,0,3))
    PM25_Plot <- function(x,y,legend_text){
      plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
      L = legend("topleft", legend = risklevels, pch=15, pt.cex=3, cex=1.2, bty='n', col = colors)
      legend(x = L$rect$left, y = L$rect$top, legend = PM25_riskvalues, col=rep(NA,2), cex=1.2, lty=c(1,1), ncol=1, x.intersp = 17, bg = "transparent")
    }
    PM25_Plot()
  })
  
  ########### PM 2.5 Gauge    
  output$PM25_Gauge <- renderPlot({
    gg.gauge <- function(pos,breaks = PM25_breaks) {
      require(ggplot2)
      get.poly <- function(a,b,r1=0.35,r2=1.0) {
        th.start <- pi*(1-a/PM25_max)
        th.end   <- pi*(1-b/PM25_max)
        th       <- seq(th.start,th.end,length=PM25_max)
        x        <- c(r1*cos(th),rev(r2*cos(th)))
        y        <- c(r1*sin(th),rev(r2*sin(th)))
        return(data.frame(x,y))
      }
      ggplot()+
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="#999933")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="#DDCC77")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="#CC6677")+
        geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="#882255")+
        geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="#330033")+
        geom_polygon(data=get.poly(pos-0.75,pos+0.75,0),aes(x,y))+
        geom_text_repel(data=as.data.frame(breaks), size=5, 
                        aes(x=cos(pi*(1-breaks/PM25_max)),
                            y=sin(pi*(1-breaks/PM25_max)),
                            label="",
                            bg.color = "white", 
                            bg.r = 0.2))+
        annotate("text",x=0,y=0,label=pos,vjust=0,size=9)+
        coord_fixed()+
        theme_bw()+
        labs(title ="Most Recent Mean PM 2.5 Measurements",
             subtitle = "")+ 
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank(),
              plot.title.position = 'plot',
              plot.title = element_text(face = "bold", 
                                        hjust=0.5, 
                                        size = 18)) 
    }
    gg.gauge(MeanPM25round,breaks=PM25_breaks)
  })
  
  
  ### PM 10 -------------
  ### PM 10 Table
  output$PM10_Table <- DT::renderDataTable({
    DT::datatable(PM10Output, rownames = F,
                  extensions = 'Buttons',
                  options = list(
                    paging = F,
                    searching = F,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    dom = 'Bfrtip',
                    ordering = TRUE,
                    buttons = list(
                      list(extend = 'csv', filename = "PhiladelphiaPM10measurements"),
                      list(extend = 'excel', filename = "PhiladelphiaPM10measurements")),
                    pagelength=6),
                  class="display")},server=F)
  
  ########## PM 10 Plot
  output$PM10_Plot <- renderPlot({
    par(mar=c(0,5,1,1),xpd=T)
    par(oma=c(0,3,0,3))
    PM10_Plot <- function(x,y,legend_text){
      plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
      L = legend("topleft", legend = risklevels, pch=15, pt.cex=3, cex=1.2, bty='n', col = colors)
      legend(x = L$rect$left, y = L$rect$top, legend = PM10_riskvalues, col=rep(NA,2), cex=1.2, lty=c(1,1), ncol=1, x.intersp = 17, bg = NA)
    }
    PM10_Plot()
  })
  
  ########### PM 10 Gauge    
  output$PM10_Gauge <- renderPlot({
    gg.gauge <- function(pos,breaks = PM10_breaks) {
      require(ggplot2)
      get.poly <- function(a,b,r1=0.35,r2=1.0) {
        th.start <- pi*(1-a/PM10_max)
        th.end   <- pi*(1-b/PM10_max)
        th       <- seq(th.start,th.end,length=PM10_max)
        x        <- c(r1*cos(th),rev(r2*cos(th)))
        y        <- c(r1*sin(th),rev(r2*sin(th)))
        return(data.frame(x,y))
      }
      ggplot()+
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="#999933")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="#DDCC77")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="#CC6677")+
        geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="#882255")+
        geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="#330033")+
        geom_polygon(data=get.poly(pos-1,pos+1,0),aes(x,y))+
        geom_text_repel(data=as.data.frame(breaks), size=5, 
                        aes(x=cos(pi*(1-breaks/PM10_max)),
                            y=sin(pi*(1-breaks/PM10_max)),
                            label="",
                            bg.color = "white", 
                            bg.r = 0.2))+
        annotate("text",x=0,y=0,label=pos,vjust=0,size=9)+
        coord_fixed()+
        theme_bw()+
        labs(title ="Most Recent Mean PM 10 Measurements",
             subtitle = "")+ 
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank(),
              plot.title.position = 'plot',
              plot.title = element_text(face = "bold", 
                                        hjust=0.5, 
                                        size = 18)) 
    }
    gg.gauge(MeanPM10round,breaks=PM10_breaks)
  })
  
  
  
  ##### 2024 TRENDS OUTPUTS ------   
  ### 2024 CARBON MONOXIDE ----
  output$CO_2024Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = CO_aqistats, 
                       fillColor = ifelse(CO_aqistats$Mean<=50, "#999933", ifelse(
                         CO_aqistats$Mean<=100, "#DDCC77", ifelse(
                           CO_aqistats$Mean<=150,  "#CC6677", ifelse(
                             CO_aqistats$Mean<=200,  "#882255", ifelse(
                               CO_aqistats$Mean$aqi<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(CO_aqistats$Mean, c(5,10)), 
                       popup = ~as.character(CarbMono$SITE_NAME),
                       label = ~round(CO_aqistats$Mean,1), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .8, na.label = ""))
  })
  
  output$CO_2024Trends <- renderPlot({
    ##### Plot Carbon Monoxide ppm Plot ----
    CO_2024valuesPlot <- ggplot()+
      geom_smooth(data = CO_2024_1hour, 
                  aes(date_local, arithmetic_mean, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,0.6, 0.1))+
      labs(title = "2024 Daily Carbon Monoxide Trends",
           x = "",
           y = "CO Parts per Million")+
      theme(axis.text.y=element_text(size=14),
            axis.text.x = element_blank(),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))
    
    ##### CO AQI TREND PLOT -------
    CO_2024aqiPlot <- ggplot()+
      geom_smooth(data = CO_2024_8hour, 
                  aes(date_local, aqi, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA))+
      labs(x = "",
           y = "Air Quality Index",
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            legend.position = "right", vjust=-5,
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'), 
                          labels = c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'), 
                        labels = c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      legend}
    COlegend <- g_legend(CO_2024aqiPlot)
    CO_2024Plots <- grid.arrange(CO_2024valuesPlot, CO_2024aqiPlot+
                                   theme(legend.position = 'none'),
                                 nrow=2, heights = c(1/2,1/2))
    CO_2024Plots <- grid.arrange(CO_2024Plots, COlegend, nrow=1, widths= c(5/6, 1/6))
  })
  
  output$CO_2024Trends1 <- renderPlot({
    ##### Plot Carbon Monoxide ppm Plot ----
    CO_2024valuesPlot <- ggplot()+
      geom_smooth(data = CO_2024_1hour, 
                  aes(date_local, arithmetic_mean, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA))+
      labs(title = "2024 Daily Carbon Monoxide Trends",
           x = "",
           y = "CO Parts per Million")+
      theme(axis.text.y=element_text(size=14),
            axis.text.x = element_blank(),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))
    
    ##### CO AQI TREND PLOT -------
    CO_2024aqiPlot <- ggplot()+
      geom_smooth(data = CO_2024_8hour, 
                  aes(date_local, aqi, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA))+
      labs(x = "",
           y = "Air Quality Index",
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            legend.position = "right", vjust=-5,
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'), 
                          labels = c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'), 
                        labels = c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      legend}
    COlegend <- g_legend(CO_2024aqiPlot)
    CO_2024Plots <- grid.arrange(CO_2024valuesPlot, CO_2024aqiPlot+
                                   theme(legend.position = 'none'),
                                 nrow=2, heights = c(1/2,1/2))
    CO_2024Plots <- grid.arrange(CO_2024Plots, COlegend, nrow=1, widths= c(5/6, 1/6))
  })
  
  output$CO_boxplots <- renderPlot({
    #########  Carbon Monoxide AQI BOXPLOT-----
    CO_2024_API_box <- ggplot(CO_2024_8hour, aes(aqi, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      labs(y = "",
           x = "Air Quality Index",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,18,3))+
      scale_y_discrete(labels = c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    CO_2024_API_box
    
    
    #########  Carbon Monoxide PPM BOXPLOT-----
    CO_2024_PPM_box <- ggplot(CO_2024_small, aes(arithmetic_mean, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      labs(y = "",
           x = "CO Parts per Million",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,1.25, 0.25))+
      scale_y_discrete(labels = c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))+
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    CO_2024_PPM_box
    
    CO_2024_boxPlots <- grid.arrange(CO_2024_API_box, CO_2024_PPM_box,
                                     nrow=1, 
                                     top= textGrob("2024 Carbon Monoxide Distributions",
                                                   gp=gpar(fontsize=20, fontface="bold")))
  })
  
  output$CO_boxplots1 <- renderPlot({
    #########  Carbon Monoxide AQI BOXPLOT-----
    CO_2024_API_box <- ggplot(CO_2024_8hour, aes(aqi, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      labs(y = "",
           x = "Air Quality Index",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,18,3))+
      scale_y_discrete(labels = c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    CO_2024_API_box
    
    
    #########  Carbon Monoxide PPM BOXPLOT-----
    CO_2024_PPM_box <- ggplot(CO_2024_small, aes(arithmetic_mean, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      labs(y = "",
           x = "CO Parts per Million",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,1.25, 0.25))+
      scale_y_discrete(labels = c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))+
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    CO_2024_PPM_box
    
    CO_2024_boxPlots <- grid.arrange(CO_2024_API_box, CO_2024_PPM_box,
                                     nrow=1, 
                                     top= textGrob("2024 Carbon Monoxide Distributions",
                                                   gp=gpar(fontsize=20, fontface="bold")))
  })
  
  output$CO_Table_2024 <- DT::renderDataTable({
    CO_2024_8hour %>% 
      select(1,6,8,7,9) -> CO_2024_Display 
    datatable(CO_2024_Display, 
              rownames = F,
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                dom = 'Bfrtip',
                ordering = TRUE,
                buttons = list(
                  list(extend = 'csv', filename = "Philadelphia2024CarbonMonoxide"),
                  list(extend = 'excel', filename = "Philadelphia2024CarbonMonoxide")),
                pagelength=6),
              class="display")},server=F)
  
  ### 2024 NITROGEN DIOXIDE ----  
  
  output$NO2_2024Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = NO2_aqistats, 
                       fillColor = ifelse(NO2_aqistats$Mean<=50, "#999933", ifelse(
                         NO2_aqistats$Mean<=100, "#DDCC77", ifelse(
                           NO2_aqistats$Mean<=150,  "#CC6677", ifelse(
                             NO2_aqistats$Mean<=200,  "#882255", ifelse(
                               NO2_aqistats$Mean$aqi<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(NO2_aqistats$Mean, c(5,10)), 
                       popup = ~as.character(NO2dat$SITE_NAME),
                       label = ~round(NO2_aqistats$Mean,1), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .8, na.label = ""))
  })
  
  output$NO2_2024Trends <- renderPlot({
    ##### Plot Nitrogen Dioxide ppb Plot ----
    NO2_2024valuesPlot <- ggplot()+
      geom_smooth(data = NO2_2024_small, 
                  aes(date_local, arithmetic_mean, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(5, NA))+
      labs(title = "2024 Daily Nitrogen Dioxide Trends",
           x = "",
           y = expression("NO"["2"]*" Parts per Billion"))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x = element_blank(),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))
    
    ##### Nitrogen Dioxide AQI TREND PLOT -------
    NO2_2024aqiPlot <- ggplot()+
      geom_smooth(data = NO2_2024_small, 
                  aes(date_local, aqi, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(10, NA))+
      labs(x = "",
           y = "Air Quality Index",
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            legend.position = "right", vjust=-5,
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,5,30, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'), 
                          labels =c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'), 
                        labels =c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      legend}
    NO2legend <- g_legend(NO2_2024aqiPlot)
    NO2_2024Plots <- grid.arrange(NO2_2024valuesPlot, NO2_2024aqiPlot+
                                    theme(legend.position = 'none'),
                                  nrow=2, heights = c(4/9,5/9))
    NO2_2024Plots <- grid.arrange(NO2_2024Plots, NO2legend, nrow=1, widths= c(5/6, 1/6))
  })
  
  output$NO2_2024Trends1 <- renderPlot({
    ##### Plot Nitrogen Dioxide ppb Plot ----
    NO2_2024valuesPlot <- ggplot()+
      geom_smooth(data = NO2_2024_small, 
                  aes(date_local, arithmetic_mean, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(5, NA))+
      labs(title = "2024 Daily Nitrogen Dioxide Trends",
           x = "",
           y = expression("NO"["2"]*" Parts per Billion"))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x = element_blank(),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))
    
    ##### Nitrogen Dioxide AQI TREND PLOT -------
    NO2_2024aqiPlot <- ggplot()+
      geom_smooth(data = NO2_2024_small, 
                  aes(date_local, aqi, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(10, NA))+
      labs(x = "",
           y = "Air Quality Index",
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            legend.position = "right", vjust=-5,
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'), 
                          labels =c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'), 
                        labels =c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      legend}
    NO2legend <- g_legend(NO2_2024aqiPlot)
    NO2_2024Plots <- grid.arrange(NO2_2024valuesPlot, NO2_2024aqiPlot+
                                    theme(legend.position = 'none'),
                                  nrow=2, heights = c(4/9,5/9))
    NO2_2024Plots <- grid.arrange(NO2_2024Plots, NO2legend, nrow=1, widths= c(5/6, 1/6))
  })
  
  output$NO2_boxplots <- renderPlot({
    #########  Nitrogen Dioxide AQI BOXPLOT-----
    NO2_2024_API_box <- ggplot(NO2_2024_small, aes(aqi, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      labs(y = "",
           x = "Air Quality Index",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,65,10))+
      scale_y_discrete(labels =c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    NO2_2024_API_box
    
    #########  Nitrogen Dioxide PPM BOXPLOT-----
    NO2_2024_PPM_box <- ggplot(NO2_2024_small, aes(arithmetic_mean, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      labs(y = "",
           x = expression("NO"["2"]*" Parts per Billion"),
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA))+
      scale_y_discrete(labels =c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))+
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    NO2_2024_PPM_box
    
    NO2_2024_boxPlots <- grid.arrange(NO2_2024_API_box, NO2_2024_PPM_box,
                                      nrow=1, 
                                      top= textGrob("2024 Nitrogen Dioxide Distributions",
                                                    gp=gpar(fontsize=20, fontface="bold")))
  })
  
  output$NO2_boxplots1 <- renderPlot({
    #########  Nitrogen Dioxide AQI BOXPLOT-----
    NO2_2024_API_box <- ggplot(NO2_2024_small, aes(aqi, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      labs(y = "",
           x = "Air Quality Index",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,18,3))+
      scale_y_discrete(labels =c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    NO2_2024_API_box
    
    #########  Nitrogen Dioxide PPM BOXPLOT-----
    NO2_2024_PPM_box <- ggplot(NO2_2024_small, aes(arithmetic_mean, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      labs(y = "",
           x = expression("NO"["2"]*" Parts per Billion"),
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA))+
      scale_y_discrete(labels =c("NEW" = "NEW", "TOR" = "TOR", "MON" = "MON"))+
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    NO2_2024_PPM_box
    
    NO2_2024_boxPlots <- grid.arrange(NO2_2024_API_box, NO2_2024_PPM_box,
                                      nrow=1, 
                                      top= textGrob("2024 Nitrogen Dioxide Distributions",
                                                    gp=gpar(fontsize=20, fontface="bold")))
  })
  
  output$NO2_Table_2024 <- DT::renderDataTable({
    NO2_2024_small %>% 
      select(1,6,8,7,9) -> NO2_2024_Display 
    datatable(NO2_2024_Display, rownames = F,
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                dom = 'Bfrtip',
                ordering = TRUE,
                buttons = list(
                  list(extend = 'csv', filename = "Philadelphia2024NitrogenDioxide"),
                  list(extend = 'excel', filename = "Philadelphia2024NitrogenDioxide")),
                pagelength=6),
              class="display")},server=F)
  
  ### 2024 OZONE ----
  
  output$O3_2024Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = O3_aqistats, 
                       fillColor = ifelse(O3_aqistats$Mean<=50, "#999933", ifelse(
                         O3_aqistats$Mean<=100, "#DDCC77", ifelse(
                           O3_aqistats$Mean<=150,  "#CC6677", ifelse(
                             O3_aqistats$Mean<=200,  "#882255", ifelse(
                               O3_aqistats$Mean$aqi<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(O3_aqistats$Mean, c(5,10)), 
                       popup = ~as.character(Ozone$SITE_NAME),
                       label = ~round(O3_aqistats$Mean,1), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .8, na.label = ""))
  })
  
  output$O3_2024Trends <- renderPlot({
    ##### Plot Ozone ppm Plot ----
    O3_2024valuesPlot <- ggplot()+
      geom_smooth(data = O3_2024_small, 
                  aes(date_local, arithmetic_mean, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA))+
      labs(title = "2024 Daily Ozone Trends",
           x = "",
           y = "Ozone Parts per Million")+
      theme(axis.text.y=element_text(size=14),
            axis.text.x = element_blank(),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(5,8,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))
    
    ##### Ozone AQI TREND PLOT -------
    O3_2024aqiPlot <- ggplot()+
      geom_smooth(data = O3_2024_small, 
                  aes(date_local, aqi, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA))+
      labs(x = "",
           y = "Air Quality Index",
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            legend.position = "right", vjust=-5,
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,5,35, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'), 
                          labels = c("NEW" = "NEW", "LABP" = "LABP", "NEA" = "NEA"))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'), 
                        labels =c("NEW" = "NEW", "LABP" = "LABP", "NEA" = "NEA"))
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      legend}
    o3legend <- g_legend(O3_2024aqiPlot)
    O3_2024Plots <- grid.arrange(O3_2024valuesPlot, O3_2024aqiPlot+
                                   theme(legend.position = 'none'),
                                 nrow=2, heights = c(4/9,5/9))
    O3_2024Plots <- grid.arrange(O3_2024Plots, o3legend, nrow=1, widths= c(5/6, 1/6))
  })
  
  output$O3_2024Trends1 <- renderPlot({
    ##### Plot Ozone ppm Plot ----
    O3_2024valuesPlot <- ggplot()+
      geom_smooth(data = O3_2024_small, 
                  aes(date_local, arithmetic_mean, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA))+
      labs(title = "2024 Daily Ozone Trends",
           x = "",
           y = "Ozone Parts per Million")+
      theme(axis.text.y=element_text(size=14),
            axis.text.x = element_blank(),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(5,8,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))
    
    ##### Ozone AQI TREND PLOT -------
    O3_2024aqiPlot <- ggplot()+
      geom_smooth(data = O3_2024_small, 
                  aes(date_local, aqi, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA))+
      labs(x = "",
           y = "Air Quality Index",
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            legend.position = "right", vjust=-5,
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,8,5,35, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'), 
                          labels =c("NEW" = "NEW", "LABP" = "LABP", "NEA" = "NEA"))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'), 
                        labels =c("NEW" = "NEW", "LABP" = "LABP", "NEA" = "NEA"))
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      legend}
    o3legend <- g_legend(O3_2024aqiPlot)
    O3_2024Plots <- grid.arrange(O3_2024valuesPlot, O3_2024aqiPlot+
                                   theme(legend.position = 'none'),
                                 nrow=2, heights = c(4/9,5/9))
    O3_2024Plots <- grid.arrange(O3_2024Plots, o3legend, nrow=1, widths= c(5/6, 1/6))
  })
  
  output$O3_boxplots <- renderPlot({
    #########  Ozone AQI BOXPLOT-----
    O3_2024_API_box <- ggplot(O3_2024_small, aes(aqi, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))+
      labs(y = "",
           x = "Air Quality Index",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,140,20))+
      scale_y_discrete(labels =c("NEW" = "NEW", "LABP" = "LABP", "NEA" = "NEA"))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    O3_2024_API_box
    
    
    #########  Ozone PPM BOXPLOT-----
    O3_2024_PPM_box <- ggplot(O3_2024_small, aes(arithmetic_mean, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))+
      labs(y = "",
           x = "Ozone Parts per Million",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA))+
      scale_y_discrete(labels =c("NEW" = "NEW", "LABP" = "LABP", "NEA" = "NEA"))+
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    O3_2024_PPM_box
    
    
    O3_2024_boxPlots <- grid.arrange(O3_2024_API_box, O3_2024_PPM_box,
                                     nrow=1, 
                                     top= textGrob("2024 Ozone Distributions",
                                                   gp=gpar(fontsize=20, fontface="bold")))
    
    
  })
  
  output$O3_boxplots1 <- renderPlot({
    #########  Ozone AQI BOXPLOT-----
    O3_2024_API_box <- ggplot(O3_2024_small, aes(aqi, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))+
      labs(y = "",
           x = "Air Quality Index",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,140,20))+
      scale_y_discrete(labels =c("NEW" = "NEW", "LABP" = "LABP", "NEA" = "NEA"))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    O3_2024_API_box
    
    
    #########  Ozone PPM BOXPLOT-----
    O3_2024_PPM_box <- ggplot(O3_2024_small, aes(arithmetic_mean, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))+
      labs(y = "",
           x = "Ozone Parts per Million",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA))+
      scale_y_discrete(labels =c("NEW" = "NEW", "LABP" = "LABP", "NEA" = "NEA"))+
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    O3_2024_PPM_box
    
    
    O3_2024_boxPlots <- grid.arrange(O3_2024_API_box, O3_2024_PPM_box,
                                     nrow=1, 
                                     top= textGrob("2024 Ozone Distributions",
                                                   gp=gpar(fontsize=20, fontface="bold")))
    
    
  })
  
  output$Ozone_Table_2024 <- DT::renderDataTable({
    O3_2024_small %>% 
      select(1,6,8,7,9) -> O3_2024_Display 
    datatable(O3_2024_Display, rownames = F,
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                dom = 'Bfrtip',
                ordering = TRUE,
                buttons = list(
                  list(extend = 'csv', filename = "Philadelphia2024Ozone"),
                  list(extend = 'excel', filename = "Philadelphia2024Ozone")),
                pagelength=6),
              class="display")},server=F)
  
  ### 2024 SULFUR DIOXIDE ----
  output$SO2_2024Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = SO2_aqistats, 
                       fillColor = ifelse(SO2_aqistats$Mean<=50, "#999933", ifelse(
                         SO2_aqistats$Mean<=100, "#DDCC77", ifelse(
                           SO2_aqistats$Mean<=150,  "#CC6677", ifelse(
                             SO2_aqistats$Mean<=200,  "#882255", ifelse(
                               SO2_aqistats$Mean$aqi<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(SO2_aqistats$Mean, c(5,10)), 
                       popup = ~as.character(SO2dat$SITE_NAME),
                       label = ~round(SO2_aqistats$Mean,1), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .8, na.label = ""))
  }) 
  
  output$SO2_2024Trends <- renderPlot({
    ##### Plot SO2 ppb Plot ----
    SO2_2024valuesPlot <- ggplot()+
      geom_smooth(data = SO2_2024_small, 
                  aes(date_local, arithmetic_mean, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks=seq(0, 1.25, 0.25))+
      labs(title = "2024 Daily Sulfur Dioxide Trends",
           x = "",
           y = expression("SO"["2"]*" Parts per Billion"))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x = element_blank(),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'))
    
    ##### SO2 AQI TREND PLOT -------
    SO2_2024aqiPlot <- ggplot()+
      geom_smooth(data = SO2_2024_small, 
                  aes(date_local, aqi, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,3,0.5))+
      labs(x = "",
           y = "Air Quality Index",
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,5,35, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'), 
                          labels = c("NEW" = "NEW", "RIT" = "RIT"))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'), 
                        labels =c("NEW" = "NEW", "RIT" = "RIT"))
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      legend}
    SO2legend <- g_legend(SO2_2024aqiPlot)
    SO2_2024Plots <- grid.arrange(SO2_2024valuesPlot, SO2_2024aqiPlot+
                                    theme(legend.position = 'none'),
                                  nrow=2, heights = c(4/9,5/9))
    SO2_2024Plots <- grid.arrange(SO2_2024Plots, SO2legend, nrow=1, widths= c(5/6, 1/6))
  })
  
  output$SO2_2024Trends1 <- renderPlot({
    ##### Plot SO2 ppb Plot ----
    SO2_2024valuesPlot <- ggplot()+
      geom_smooth(data = SO2_2024_small, 
                  aes(date_local, arithmetic_mean, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks=seq(0, 1.25, 0.25))+
      labs(title = "2024 Daily Sulfur Dioxide Trends",
           x = "",
           y = expression("SO"["2"]*" Parts per Billion"))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x = element_blank(),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'))+
      scale_fill_manual(values = SO2_2024_small$SiteName)
    
    ##### SO2 AQI TREND PLOT -------
    SO2_2024aqiPlot <- ggplot()+
      geom_smooth(data = SO2_2024_small, 
                  aes(date_local, aqi, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,3,0.5))+
      labs(x = "",
           y = "Air Quality Index",
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'), 
                          labels =c("NEW" = "NEW", "RIT" = "RIT"))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'), 
                        labels =c("NEW" = "NEW", "RIT" = "RIT"))
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      legend}
    SO2legend <- g_legend(SO2_2024aqiPlot)
    SO2_2024Plots <- grid.arrange(SO2_2024valuesPlot, SO2_2024aqiPlot+
                                    theme(legend.position = 'none'),
                                  nrow=2, heights = c(4/9,5/9))
    SO2_2024Plots <- grid.arrange(SO2_2024Plots, SO2legend, nrow=1, widths= c(5/6, 1/6))
  })
  
  output$SO2_boxplots <- renderPlot({
    #########  Sulfur Dioxide AQI BOXPLOT-----
    SO2_2024_API_box <- ggplot(SO2_2024_small, aes(aqi, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'))+
      labs(y = "",
           x = "Air Quality Index",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,11), breaks = seq(0,12,2))+
      scale_y_discrete(labels =c("NEW" = "NEW", "RIT" = "RIT"))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    SO2_2024_API_box
    
    
    #########  Sulfur Dioxide PPM BOXPLOT-----
    SO2_2024_PPB_box <- ggplot(SO2_2024_small, aes(arithmetic_mean, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'))+
      labs(y = "",
           x = expression("SO"["2"]*" Parts per Billion"),
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA))+
      scale_y_discrete(labels =c("NEW" = "NEW", "RIT" = "RIT"))+
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    SO2_2024_PPB_box
    
    
    SO2_2024_boxPlots <- grid.arrange(SO2_2024_API_box, SO2_2024_PPB_box,
                                      nrow=1, 
                                      top= textGrob("2024 Sulfur Dioxide Distributions",
                                                    gp=gpar(fontsize=20, fontface="bold")))
    
    
  })  
  
  output$SO2_boxplots1 <- renderPlot({
    #########  Sulfur Dioxide AQI BOXPLOT-----
    SO2_2024_API_box <- ggplot(SO2_2024_small, aes(aqi, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'))+
      labs(y = "",
           x = "Air Quality Index",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,11), breaks = seq(0,12,2))+
      scale_y_discrete(labels =c("NEW" = "NEW", "RIT" = "RIT"))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    SO2_2024_API_box
    
    
    #########  Sulfur Dioxide PPM BOXPLOT-----
    SO2_2024_PPB_box <- ggplot(SO2_2024_small, aes(arithmetic_mean, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'))+
      labs(y = "",
           x = expression("SO"["2"]*" Parts per Billion"),
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA))+
      scale_y_discrete(labels =c("NEW" = "NEW", "RIT" = "RIT"))+
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    SO2_2024_PPB_box
    
    
    SO2_2024_boxPlots <- grid.arrange(SO2_2024_API_box, SO2_2024_PPB_box,
                                      nrow=1, 
                                      top= textGrob("2024 Sulfur Dioxide Distributions",
                                                    gp=gpar(fontsize=20, fontface="bold")))
    
    
  })
  
  output$SO2_Table_2024 <- DT::renderDataTable({
    SO2_2024_small %>% 
      select(1,6,8,7,9) -> SO2_2024_Display 
    datatable(SO2_2024_Display, rownames = F,
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                dom = 'Bfrtip',
                ordering = TRUE,
                buttons = list(
                  list(extend = 'csv', filename = "Philadelphia2024OSulfurDioxide"),
                  list(extend = 'excel', filename = "Philadelphia2024SulfurDioxide")),
                pagelength=6),
              class="display")},server=F)
  
  ### 2024 PM 2.5 ----
  output$PM25_2024Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = PM25_aqistats, 
                       fillColor = ifelse(PM25_aqistats$Mean<=50, "#999933", ifelse(
                         PM25_aqistats$Mean<=100, "#DDCC77", ifelse(
                           PM25_aqistats$Mean<=150,  "#CC6677", ifelse(
                             PM25_aqistats$Mean<=200,  "#882255", ifelse(
                               PM25_aqistats$Mean$aqi<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(PM25_aqistats$Mean, c(5,10)), 
                       popup = ~as.character(PM25dat$SITE_NAME),
                       label = ~round(PM25_aqistats$Mean,1), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .8, na.label = ""))
  })
  
  output$PM25_2024Trends <- renderPlot({
    ##### Plot PM25 ppm Plot ----
    PM25_2024mgm3Plot <- ggplot()+
      geom_smooth(data = PM25_2024_small, 
                  aes(date_local, arithmetic_mean, 
                      color = SiteName,
                      fill = SiteName),se=F)+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(8, NA), breaks = seq(8,16, 2))+
      labs(title = "2024 Daily PM 2.5 Trends",
           x = "",
           y = "PM 2.5 µg/m³")+
      theme(axis.text.y=element_text(size=14),
            axis.text.x = element_blank(),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))
    
    ##### PM25 AQI TREND PLOT -------
    PM25_2024aqiPlot <- ggplot()+
      geom_smooth(data = PM25_2024_small, 
                  aes(date_local, aqi, 
                      color = SiteName), se=F)+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(40, NA), breaks = seq(40,70,5))+
      labs(x = "",
           y = "Air Quality Index",
           color = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            legend.position = "right", vjust=-5,
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'), 
                          labels =c("NEW" = 'NEW', "TOR" = 'TOR', "MON" = 'MON', "LABP" = 'LABP', "FAB" = 'FAB', "RIT" = 'RIT'))
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      legend}
    PM25legend <- g_legend(PM25_2024aqiPlot)
    PM25_2024Plots <- grid.arrange(PM25_2024mgm3Plot, PM25_2024aqiPlot+
                                     theme(legend.position = 'none'),
                                   nrow=2, heights = c(4/9,5/9))
    PM25_2024Plots <- grid.arrange(PM25_2024Plots, PM25legend, nrow=1, widths= c(5/6, 1/6))
  })
  
  output$PM25_2024Trends1 <- renderPlot({
    ##### Plot PM25 ppm Plot ----
    PM25_2024mgm3Plot <- ggplot()+
      geom_smooth(data = PM25_2024_small, 
                  aes(date_local, arithmetic_mean, 
                      color = SiteName,
                      fill = SiteName),se=F)+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(8, NA), breaks = seq(8,16, 2))+
      labs(title = "2024 Daily PM 2.5 Trends",
           x = "",
           y = "PM 2.5 µg/m³")+
      theme(axis.text.y=element_text(size=14),
            axis.text.x = element_blank(),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))
    
    ##### PM25 AQI TREND PLOT -------
    PM25_2024aqiPlot <- ggplot()+
      geom_smooth(data = PM25_2024_small, 
                  aes(date_local, aqi, 
                      color = SiteName), se=F)+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(40, NA), breaks = seq(40,70,5))+
      labs(x = "",
           y = "Air Quality Index",
           color = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            legend.position = "right", vjust=-5,
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'), 
                          labels = c("NEW" = 'NEW', "TOR" = 'TOR', "MON" = 'MON', "LABP" = 'LABP', "FAB" = 'FAB', "RIT" = 'RIT'))
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      legend}
    PM25legend <- g_legend(PM25_2024aqiPlot)
    PM25_2024Plots <- grid.arrange(PM25_2024mgm3Plot, PM25_2024aqiPlot+
                                     theme(legend.position = 'none'),
                                   nrow=2, heights = c(4/9,5/9))
    PM25_2024Plots <- grid.arrange(PM25_2024Plots, PM25legend, nrow=1, widths= c(5/6, 1/6))
  })
  
  output$PM25_boxplots <- renderPlot({
    #########  PM25 AQI BOXPLOT-----
    PM25_2024_API_box <- ggplot(PM25_2024_small, aes(aqi, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))+
      labs(y="",
           x = "Air Quality Index",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,100,20))+
      scale_y_discrete(labels =c("NEW" = 'NEW', "TOR" = 'TOR', "MON" = 'MON', "LABP" = 'LABP', "FAB" = 'FAB', "RIT" = 'RIT'))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    PM25_2024_API_box
    
    #########  PM25 mg/m3 BOXPLOT-----
    PM25_2024_mgm3_box <- ggplot(PM25_2024_small, aes(arithmetic_mean, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))+
      labs(x = "PM 2.5 µg/m³",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,35), breaks = seq(0,35,5))+
      scale_y_discrete(labels =c("NEW" = 'NEW', "TOR" = 'TOR', "MON" = 'MON', "LABP" = 'LABP', "FAB" = 'FAB', "RIT" = 'RIT'))+
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    PM25_2024_mgm3_box
    
    
    PM25_2024_boxPlots <- grid.arrange(PM25_2024_API_box, PM25_2024_mgm3_box,
                                       nrow=1, 
                                       top= textGrob("2024 PM 2.5 Distributions",
                                                     gp=gpar(fontsize=20, fontface="bold")))
    
    
  })
  
  output$PM25_boxplots1 <- renderPlot({
    #########  PM25 AQI BOXPLOT-----
    PM25_2024_API_box <- ggplot(PM25_2024_small, aes(aqi, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))+
      labs(y="",
           x = "Air Quality Index",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,100,20))+
      scale_y_discrete(labels =c("NEW" = 'NEW', "TOR" = 'TOR', "MON" = 'MON', "LABP" = 'LABP', "FAB" = 'FAB', "RIT" = 'RIT'))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    PM25_2024_API_box
    
    #########  PM25 mg/m3 BOXPLOT-----
    PM25_2024_mgm3_box <- ggplot(PM25_2024_small, aes(arithmetic_mean, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))+
      labs(x = "PM 2.5 µg/m³",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,35), breaks = seq(0,35,5))+
      scale_y_discrete(labels =c("NEW" = 'NEW', "TOR" = 'TOR', "MON" = 'MON', "LABP" = 'LABP', "FAB" = 'FAB', "RIT" = 'RIT'))+
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    PM25_2024_PPM_box
    
    
    PM25_2024_boxPlots <- grid.arrange(PM25_2024_API_box, PM25_2024_mgm3_box,
                                       nrow=1, 
                                       top= textGrob("2024 PM 2.5 Distributions",
                                                     gp=gpar(fontsize=20, fontface="bold")))
    
    
  })
  
  output$PM25_Table_2024 <- DT::renderDataTable({
    PM25_2024_small %>% 
      select(1,6,8,7,9) -> PM25_2024_Display 
    datatable(PM25_2024_Display, rownames = F,
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                dom = 'Bfrtip',
                ordering = TRUE,
                buttons = list(
                  list(extend = 'csv', filename = "Philadelphia2024PM25"),
                  list(extend = 'excel', filename = "Philadelphia2024PM25")),
                pagelength=6),
              class="display")},server=F)
  
  ### 2024 PM 10 ----
  output$PM10_2024Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = PM10_aqistats, 
                       fillColor = ifelse(PM10_aqistats$Mean<=50, "#999933", ifelse(
                         PM10_aqistats$Mean<=100, "#DDCC77", ifelse(
                           PM10_aqistats$Mean<=150,  "#CC6677", ifelse(
                             PM10_aqistats$Mean<=200,  "#882255", ifelse(
                               PM10_aqistats$Mean$aqi<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(PM10_aqistats$Mean, c(5,10)), 
                       popup = ~as.character(PM10dat$SITE_NAME),
                       label = ~round(PM10_aqistats$Mean,1), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .8, na.label = ""))
  })
  
  output$PM10_2024Trends <- renderPlot({
    ##### Plot PM10 ppm Plot ----
    PM10_2024mgm3Plot <- ggplot()+
      geom_smooth(data = PM10_2024_small, 
                  aes(date_local, arithmetic_mean, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(15, NA))+
      labs(title = "2024 Daily PM 10 Trends",
           x = "",
           y = "PM 10 µg/m³")+
      theme(axis.text.y=element_text(size=14),
            axis.text.x = element_blank(),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(5,0,5,10, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9'))
    
    ##### PM10 AQI TREND PLOT -------
    PM10_2024aqiPlot <- ggplot()+
      geom_smooth(data = PM10_2024_small, 
                  aes(date_local, aqi, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(15, NA))+
      labs(x = "",
           y = "Air Quality Index",
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            legend.position = "right", vjust=-5,
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,5,10, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9'), 
                          labels =c("NEW" = 'NEW'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9'), 
                        labels =c("NEW" = 'NEW'))
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      legend}
    PM10legend <- g_legend(PM10_2024aqiPlot)
    PM10_2024Plots <- grid.arrange(PM10_2024mgm3Plot, PM10_2024aqiPlot+
                                     theme(legend.position = 'none'),
                                   nrow=2, heights = c(4/9,5/9))
    PM10_2024Plots <- grid.arrange(PM10_2024Plots, PM10legend, nrow=1, widths= c(5/6, 1/6))
  })
  
  output$PM10_2024Trends1 <- renderPlot({
    ##### Plot PM10 ppm Plot ----
    PM10_2024mgm3Plot <- ggplot()+
      geom_smooth(data = PM10_2024_small, 
                  aes(date_local, arithmetic_mean, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(15, NA))+
      labs(title = "2024 Daily PM 10 Trends",
           x = "",
           y = "PM 10 µg/m³")+
      theme(axis.text.y=element_text(size=14),
            axis.text.x = element_blank(),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(5,0,5,10, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9'))
    
    ##### PM10 AQI TREND PLOT -------
    PM10_2024aqiPlot <- ggplot()+
      geom_smooth(data = PM10_2024_small, 
                  aes(date_local, aqi, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-01-01")), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(12, NA))+
      labs(x = "",
           y = "Air Quality Index",
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            legend.position = "right", vjust=-5,
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,5,10, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9'), 
                          labels =c("NEW" = 'NEW'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9'), 
                        labels =c("NEW" = 'NEW'))
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      legend}
    PM10legend <- g_legend(PM10_2024aqiPlot)
    PM10_2024Plots <- grid.arrange(PM10_2024mgm3Plot, PM10_2024aqiPlot+
                                     theme(legend.position = 'none'),
                                   nrow=2, heights = c(4/9,5/9))
    PM10_2024Plots <- grid.arrange(PM10_2024Plots, PM10legend, nrow=1, widths= c(5/6, 1/6))
  })
  
  output$PM10_boxplots <- renderPlot({
    #########  PM10 AQI BOXPLOT-----
    PM10_2024_API_box <- ggplot(PM10_2024_small, aes(aqi, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9'))+
      labs(y= "",
           x = "Air Quality Index",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,60,10))+
      scale_y_discrete(labels =c("NEW" = 'NEW'))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    PM10_2024_API_box
    
    #########  PM10 mg/m3 BOXPLOT-----
    PM10_2024_mgm3_box <- ggplot(PM10_2024_small, aes(arithmetic_mean, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9'))+
      labs(y= "",
           x = "PM 10 µg/m³",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,35), breaks = seq(0,35,5))+
      scale_y_discrete(labels =c("NEW" = 'NEW'))+
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    PM10_2024_mgm3_box
    
    
    PM10_2024_boxPlots <- grid.arrange(PM10_2024_API_box, PM10_2024_mgm3_box,
                                       nrow=1, 
                                       top= textGrob("2024 PM 10 Distributions",
                                                     gp=gpar(fontsize=20, fontface="bold")))
    
    
  })
  
  output$PM10_boxplots1 <- renderPlot({
    #########  PM10 AQI BOXPLOT-----
    PM10_2024_API_box <- ggplot(PM10_2024_small, aes(aqi, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9'))+
      labs(x = "Air Quality Index",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,100,20))+
      scale_y_discrete(labels =c("NEW" = 'NEW'))+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    PM10_2024_API_box
    
    #########  PM10 mg/m3 BOXPLOT-----
    PM10_2024_mgm3_box <- ggplot(PM10_2024_small, aes(arithmetic_mean, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9'))+
      labs(x = "PM 10 µg/m³",
           fill = "Site Name") +
      scale_x_continuous(limits = c(0,35), breaks = seq(1,35,5))+
      scale_y_discrete(labels =c("NEW" = 'NEW'))+
      theme(axis.text.y=element_blank(),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 16),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,15, "points"))
    PM10_2024_mgm3_box
    
    
    PM10_2024_boxPlots <- grid.arrange(PM10_2024_API_box, PM10_2024_mgm3_box,
                                       nrow=1, 
                                       top= textGrob("2024 PM 10 Distributions",
                                                     gp=gpar(fontsize=20, fontface="bold")))
    
    
  })
  
  output$PM10_Table_2024 <- DT::renderDataTable({
    PM10_2024_small %>% 
      select(1,6,8,7,9) -> PM10_2024_Display 
    datatable(PM10_2024_Display, rownames = F,
              extensions = 'Buttons',
              options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                dom = 'Bfrtip',
                ordering = TRUE,
                buttons = list(
                  list(extend = 'csv', filename = "Philadelphia2024PM10"),
                  list(extend = 'excel', filename = "Philadelphia2024PM10")),
                pagelength=6),
              class="display")},server=F)
  ###### 2025 TRENDS OUTPUTS -----
  #### 2025 CARBON MONOXIDE ----
  output$CO_2025Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = CO2025_PPMstats, 
                       fillColor = ifelse(CO2025_PPMstats$Mean<=50, "#999933", ifelse(
                         CO2025_PPMstats$Mean<=100, "#DDCC77", ifelse(
                           CO2025_PPMstats$Mean<=150,  "#CC6677", ifelse(
                             CO2025_PPMstats$Mean<=200,  "#882255", ifelse(
                               CO2025_PPMstats$Mean$aqi<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(CO2025_PPMstats$Mean, c(5,10)),
                       popup = ~as.character(CO2025_PPMstats$SiteName),
                       label = ~paste(sprintf("%.2f",CO2025_PPMstats$Mean), "PPM", sep = " "),
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .8, na.label = ""))
  })
  
  output$CO_2025Trends <- renderPlot({
    ##### Plot Carbon Monoxide ppm ----
    CO_2025valuesPlot <- ggplot()+
      geom_smooth(data = Phila2025_CO_8hr, 
                  aes(Date, Measurement, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2025-01-01"), Sys.Date()), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,0.7,0.1))+
      labs(x = "",
           y = "CO Parts Per Million",
           title = "2025 Carbon Monoxide Trends", 
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,0,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))
    CO_2025valuesPlot
  })
  
  output$CO_2025Trends1 <- renderPlot({
    ##### Plot Carbon Monoxide ppm ----
    CO_2025valuesPlot <- ggplot()+
      geom_smooth(data = Phila2025_CO_8hr, 
                  aes(Date, Measurement, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2025-01-01"), Sys.Date()), 
                   expand = c(0,0),
                   date_breaks = "2 weeks",
                   date_labels = "%B %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,0.7,0.1))+
      labs(x = "",
           y = "CO Parts Per Million",
           title = "2025 Carbon Monoxide Trends", 
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "bottom",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,5,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))
    CO_2025valuesPlot
  })
  
  output$CO_boxplots2025 <- renderPlot({
    #########  Carbon Monoxide  BOXPLOT-----
    Phila2025_CO_8hr$SiteName <- factor(Phila2025_CO_8hr$SiteName, levels = c("TOR", "NEW", "MON"))
    CO_2025_box <- ggplot(Phila2025_CO_8hr, aes(Measurement, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      labs(y = "",
           x = "CO Parts per Million",
           title = "2025 Carbon Monoxide Distributions",
           fill = "SiteName") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,1.75, 0.25))+
      scale_y_discrete()+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,0, "points"))
    CO_2025_box
  })
  
  output$CO_boxplots2025_1 <- renderPlot({
    #########  Carbon Monoxide  BOXPLOT-----
    CO_2025_box <- ggplot(Phila2025_CO_8hr, aes(Measurement, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      labs(y = "",
           x = "CO Parts per Million",
           title = "2025 Carbon Monoxide Distributions",
           fill = "SiteName") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,1.75, 0.25))+
      scale_y_discrete()+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,0,5,0, "points"))
    CO_2025_box
  })
  
  output$CO_DistTable <- renderDataTable({
    CO2025_PPMstats %>% 
      as.data.frame() %>% 
      mutate_if(is.numeric, ~round(., 3))-> forCOtable
    forCOtable %>% 
      select(1:7) -> forCOtable
    DT::datatable(forCOtable, 
                  rownames = F, 
                  colnames = c("Site Name", "Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum", "Mean"),
                  options = list(paging=F,
                                 searching=F),
                  class = "display") %>%
      formatStyle('SiteName',
                  target = "row",
                  backgroundColor = styleEqual(c("NEW", "TOR", "MON"), 
                                               c('#56B4E9', '#009E73', '#D55E00'))) -> forCOtable
    forCOtable
  })
  #### 2025 NITROGEN DIOXIDE ----
  output$NO2_2025Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = NO2_2025_PPMstats, 
                       fillColor = ifelse(NO2_2025_PPMstats$Mean<=50, "#999933", ifelse(
                         NO2_2025_PPMstats$Mean<=100, "#DDCC77", ifelse(
                           NO2_2025_PPMstats$Mean<=150,  "#CC6677", ifelse(
                             NO2_2025_PPMstats$Mean<=200,  "#882255", ifelse(
                               NO2_2025_PPMstats$Mean$aqi<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       popup = ~as.character(NO2_2025_PPMstats$SiteName),
                       label = ~paste(sprintf("%.2f",NO2_2025_PPMstats$Mean), "PPB", sep = " "),
                       radius = ~rescale(NO2_2025_PPMstats$Mean, c(5,10)),
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .8, na.label = ""))
  })
  
  output$NO2_2025Trends <- renderPlot({
    ##### Plot Nitrogen Dioxide ppb ----
    NO2_2025valuesPlot <- ggplot()+
      geom_smooth(data = Phila2025_NO, 
                  aes(Date, Measurement, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2025-01-01"), Sys.Date()), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,40,5))+
      labs(x = "",
           y = expression("NO"["2"]*" Parts per Billion"),
           title = "2025 Nitrogen Dioxide Trends", 
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,0,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))
    NO2_2025valuesPlot
  })
  
  output$NO2_2025Trends1 <- renderPlot({
    ##### Plot Nitrogen Dioxide ppb ----
    NO2_2025valuesPlot <- ggplot()+
      geom_smooth(data = Phila2025_NO, 
                  aes(Date, Measurement, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2025-01-01"), Sys.Date()), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,40,5))+
      labs(x = "",
           y = expression("NO"["2"]*" Parts per Billion"),
           title = "2025 Nitrogen Dioxide Trends", 
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,0,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))
    NO2_2025valuesPlot
  })
  
  output$NO2_boxplots2025 <- renderPlot({
    #########  Nitrogen Dioxide  BOXPLOT-----
    Phila2025_NO <- data.frame(Phila2025_NO)
    Phila2025_NO$SiteName <- factor(Phila2025_NO$SiteName, levels = c("TOR", "NEW", "MON"))
    NO2_2025_box <- ggplot(Phila2025_NO, aes(Measurement, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      labs(y = "",
           x = expression("NO"["2"]*" Parts per Billion"),
           title = "2025 Nitrogen Dioxide Distributions",
           fill = "SiteName") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,40, 5))+
      scale_y_discrete()+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,0, "points"))
    NO2_2025_box
  })
  
  output$NO2_boxplots2025_1 <- renderPlot({
    #########  Nitrogen Dioxide  BOXPLOT-----
    Phila2025_NO <- data.frame(Phila2025_NO)
    Phila2025_NO$SiteName <- factor(Phila2025_NO$SiteName, levels = c("TOR", "NEW", "MON"))
    NO2_2025_box <- ggplot(Phila2025_NO, aes(Measurement, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00'))+
      labs(y = "",
           x = expression("NO"["2"]*" Parts per Billion"),
           title = "2025 Nitrogen Dioxide Distributions",
           fill = "SiteName") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,40, 5))+
      scale_y_discrete()+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,0, "points"))
    NO2_2025_box
  })
  
  output$NO2_DistTable <- renderDataTable({
    NO2_2025_PPMstats %>% 
      as.data.frame() %>% 
      mutate_if(is.numeric, ~round(., 3))-> forNO2table
    forNO2table %>% 
      select(1:7) -> forNO2table
    DT::datatable(forNO2table, 
                  rownames = F, 
                  colnames = c("Site Name", "Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum", "Mean"),
                  options = list(paging=F,
                                 searching=F),
                  class = "display") %>%
      formatStyle('SiteName',
                  target = "row",
                  backgroundColor = styleEqual(c("NEW", "TOR", "MON"), 
                                               c('#56B4E9', '#009E73', '#D55E00'))) -> forNO2table
    forNO2table
  })
  #### 2025 OZONE ----
  output$O3_2025Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = O3_2025_PPMstats, 
                       fillColor = ifelse(O3_2025_PPMstats$Mean<=50, "#999933", ifelse(
                         O3_2025_PPMstats$Mean<=100, "#DDCC77", ifelse(
                           O3_2025_PPMstats$Mean<=150,  "#CC6677", ifelse(
                             O3_2025_PPMstats$Mean<=200,  "#882255", ifelse(
                               O3_2025_PPMstats$Mean$aqi<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(O3_2025_PPMstats$Mean, c(5,10)), 
                       popup = ~as.character(O3_2025_PPMstats$SiteName),
                       label = ~paste(sprintf("%.2f",O3_2025_PPMstats$Mean), "PPM", sep = " "), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .8, na.label = ""))
  })
  
  output$O3_2025Trends <- renderPlot({
    ##### Plot Ozone ppm ----
    O3_2025valuesPlot <- ggplot()+
      geom_smooth(data = Phila2025_OZONE_8HR, 
                  aes(Date, Measurement, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2025-01-01"), Sys.Date()), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(20, NA), breaks = seq(20,50,5))+
      labs(x = "",
           y = expression("O"["3"]*" Parts per Million"),
           title = "2025 Ozone Trends", 
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,0,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))
    O3_2025valuesPlot
  })
  
  output$O3_2025Trends1 <- renderPlot({
    ##### Plot Ozone ppm ----
    O3_2025valuesPlot <- ggplot()+
      geom_smooth(data = Phila2025_OZONE_8HR, 
                  aes(Date, Measurement, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2025-01-01"), Sys.Date()), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,140,20))+
      labs(x = "",
           y = "O3 Parts Per Million",
           title = "2025 Ozone Trends", 
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,0,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))
    O3_2025valuesPlot
  })    
  
  output$O3_boxplots2025 <- renderPlot({
    #########  Ozone BOXPLOT-----
    Phila2025_OZONE_8HR$SiteName <- factor(Phila2025_OZONE_8HR$SiteName, levels = c("NEA", "LABP", "NEW"))
    O3_2025_box <- ggplot(Phila2025_OZONE_8HR, aes(Measurement, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))+
      labs(y = "",
           x = "O3 Parts per Million",
           title = "2025 Ozone Distributions",
           fill = "SiteName") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,60, 10))+
      scale_y_discrete()+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,0, "points"))
    O3_2025_box
  })
  
  output$O3_boxplots2025_1 <- renderPlot({
    #########  Ozone BOXPLOT-----
    Phila2025_OZONE_8HR$SiteName <- factor(Phila2025_OZONE_8HR$SiteName, levels = c("NEA", "LABP", "NEW"))
    O3_2025_box <- ggplot(Phila2025_OZONE_8HR, aes(Measurement, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "LABP" = '#cccc99', "NEA" = '#336666'))+
      labs(y = "",
           x = "O3 Parts per Million",
           title = "2025 Ozone Distributions",
           fill = "SiteName") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,140, 20))+
      scale_y_discrete()+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,0, "points"))
    O3_2025_box
  })
  
  output$O3_DistTable <- renderDataTable({
    O3_2025_PPMstats %>% 
      as.data.frame() %>% 
      mutate_if(is.numeric, ~round(., 3))-> forO3table
    forO3table %>% 
      select(1:7) -> forO3table
    DT::datatable(forO3table, 
                  rownames = F, 
                  colnames = c("Site Name", "Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum", "Mean"),
                  options = list(paging=F,
                                 searching=F),
                  class = "display") %>%
      formatStyle('SiteName',
                  target = "row",
                  color = styleEqual("NEA", "white"),
                  backgroundColor = styleEqual(c("NEW", "LABP", "NEA"), 
                                               c('#56B4E9', '#cccc99', '#336666'))) -> forO3table
    forO3table
  })
  #### 2025 SULFUR DIOXIDE ----
  output$SO2_2025Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = SO2_2025_PPMstats, 
                       fillColor = ifelse(SO2_2025_PPMstats$Mean<=50, "#999933", ifelse(
                         SO2_2025_PPMstats$Mean<=100, "#DDCC77", ifelse(
                           SO2_2025_PPMstats$Mean<=150,  "#CC6677", ifelse(
                             SO2_2025_PPMstats$Mean<=200,  "#882255", ifelse(
                               SO2_2025_PPMstats$Mean$aqi<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(SO2_2025_PPMstats$Mean, c(5,10)),
                       popup = ~as.character(SO2_2025_PPMstats$SiteName),
                       label = ~paste(sprintf("%.2f",SO2_2025_PPMstats$Mean), "PPB", sep = " "), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .8, na.label = ""))
  })
  
  output$SO2_2025Trends <- renderPlot({
    ##### Plot Sulfur Dioxide ppb ----
    SO2_2025valuesPlot <- ggplot()+
      geom_smooth(data = Phila2025_SO2_24HR, 
                  aes(Date, Measurement, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2025-01-01"), Sys.Date()), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,1.5,0.25))+
      labs(x = "",
           y = expression("SO"["2"]*" Parts per Billion"),
           title = "2025 Sulfur Dioxide Trends", 
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,0,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633' ))
    SO2_2025valuesPlot
  })
  
  output$SO2_2025Trends1 <- renderPlot({
    ##### Plot Sulfur Dioxide ppb ----
    SO2_2025valuesPlot <- ggplot()+
      geom_smooth(data = Phila2025_SO2_24HR, 
                  aes(Date, Measurement, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2025-01-01"), Sys.Date()), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,1.75,0.25))+
      labs(x = "",
           y = expression("SO"["2"]*" Parts per Billion"),
           title = "2025 Sulfur Dioxide Trends", 
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,0,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633' ))
    SO2_2025valuesPlot
  })       
  
  output$SO2_boxplots2025 <- renderPlot({
    #########  Sulfur Dioxide BOXPLOT-----
    # Phila2025_SO2_24hr$SiteName <- factor(Phila2025_SO2_24hr$SiteName, levels = c("NEA", "LABP", "NEW"))
    SO2_2025_box <- ggplot(Phila2025_SO2_24HR, aes(Measurement, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'))+
      labs(y = "",
           x = expression("SO"["2"]*" Parts per Billion"),
           title = "2025 Sulfur Dioxide Distributions",
           fill = "SiteName") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,3, 0.25))+
      scale_y_discrete()+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,0, "points"))
    SO2_2025_box
  })
  
  output$SO2_boxplots2025_1 <- renderPlot({
    #########  Sulfur Dioxide BOXPLOT-----
    # Phila2025_SO2_24hr$SiteName <- factor(Phila2025_SO2_24hr$SiteName, levels = c("NEA", "LABP", "NEW"))
    SO2_2025_box <- ggplot(Phila2025_SO2_24HR, aes(Measurement, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "RIT" = '#996633'))+
      labs(y = "",
           x = expression("SO"["2"]*" Parts per Billion"),
           title = "2025 Sulfur Dioxide Distributions",
           fill = "SiteName") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,3, 0.5))+
      scale_y_discrete()+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,0, "points"))
    SO2_2025_box
  })
  
  output$SO2_DistTable <- renderDataTable({
    SO2_2025_PPMstats %>% 
      as.data.frame() %>% 
      mutate_if(is.numeric, ~round(., 3))-> forSO2table
    forSO2table %>% 
      select(1:7) -> forSO2table
    DT::datatable(forSO2table, 
                  rownames = F, 
                  colnames = c("Site Name", "Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum", "Mean"),
                  options = list(paging=F,
                                 searching=F),
                  class = "display") %>%
      formatStyle('SiteName',
                  target = "row",
                  backgroundColor = styleEqual(c("NEW", "RIT"), 
                                               c('#56B4E9', '#996633'))) -> forSO2table
    forSO2table
  })
  #### 2025 PM 2.5 ----
  output$PM25_2025Map <- renderLeaflet({
    pm25colors <- c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633')
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = PM25_2025_PPMstats, 
                       fillColor = ifelse(PM25_2025_PPMstats$Mean<=50, "#999933", ifelse(
                         PM25_2025_PPMstats$Mean<=100, "#DDCC77", ifelse(
                           PM25_2025_PPMstats$Mean<=150,  "#CC6677", ifelse(
                             PM25_2025_PPMstats$Mean<=200,  "#882255", ifelse(
                               PM25_2025_PPMstats$Mean$aqi<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(PM25_2025_PPMstats$Mean, c(5,10)),
                       popup = ~as.character(PM25_2025_PPMstats$SiteName),
                       label = ~paste(sprintf("%.2f",PM25_2025_PPMstats$Mean), "µg/m³", sep = " "), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .8, na.label = ""))
  })
  
  output$PM25_2025Trends <- renderPlot({
    ##### Plot PM 2.5 mg/m3 ----
    PM25_2025valuesPlot <- ggplot()+
      geom_smooth(data = Phila2025_PM25_24hr, 
                  aes(Date, Measurement, 
                      color = SiteName,
                      fill = SiteName), se = F)+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2025-01-01"), Sys.Date()), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(3, NA), breaks = seq(3,15,3))+
      labs(x = "",
           y = "PM 2.5 µg/m³",
           title = "2025 PM 2.5 Trends", 
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,0,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))
    PM25_2025valuesPlot
  })
  
  output$PM25_2025Trends1 <- renderPlot({
    ##### Plot PM 2.5 mg/m3 ----
    PM25_2025valuesPlot <- ggplot()+
      geom_smooth(data = Phila2025_PM25_24hr, 
                  aes(Date, Measurement, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2025-01-01"), Sys.Date()), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,3,0.5))+
      labs(x = "",
           y = "PM 2.5 µg/m³",
           title = "2025 PM 2.5 Trends", 
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,0,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))
    PM25_2025valuesPlot
  })   
  
  output$PM25_boxplots2025 <- renderPlot({
    #########  PM 2.5 BOXPLOT-----
    # Phila2025_SO2_24hr$SiteName <- factor(Phila2025_SO2_24hr$SiteName, levels = c("NEA", "LABP", "NEW"))
    PM25_2025_box <- ggplot(Phila2025_PM25_24hr, aes(Measurement, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))+
      labs(y = "",
           x = "PM 2.5 µg/m³",
           title = "2025 PM 2.5 Distributions",
           fill = "SiteName") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,30, 5))+
      scale_y_discrete()+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,0, "points"))
    PM25_2025_box
  })
  
  output$PM25_boxplots2025_1 <- renderPlot({
    #########  PM 2.5 BOXPLOT-----
    # Phila2025_SO2_24hr$SiteName <- factor(Phila2025_SO2_24hr$SiteName, levels = c("NEA", "LABP", "NEW"))
    PM25_2025_box <- ggplot(Phila2025_PM25_24hr, aes(Measurement, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))+
      labs(y = "",
           x = "PM 2.5 µg/m³",
           title = "2025 PM 2.5 Distributions",
           fill = "SiteName") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,3, 0.5))+
      scale_y_discrete()+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,0, "points"))
    PM25_2025_box
  })
  
  output$PM25_DistTable <- renderDataTable({
    PM25_2025_PPMstats %>% 
      as.data.frame() %>% 
      mutate_if(is.numeric, ~round(., 3))-> forPM25table
    forPM25table %>% 
      select(1:7) -> forPM25table
    DT::datatable(forPM25table, 
                  rownames = F, 
                  colnames = c("Site Name", "Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum", "Mean"),
                  options = list(paging=F,
                                 searching=F),
                  class = "display") %>%
      formatStyle('SiteName',
                  target = "row",
                  color = styleEqual("FAB", "white"),
                  backgroundColor = styleEqual(c("NEW", "TOR", "MON", "LABP", "FAB", "RIT"), 
                                               c("NEW" = '#56B4E9', "TOR" = '#009E73', "MON" = '#D55E00', "LABP" = '#cccc99', "FAB" = '#663399', "RIT" = '#996633'))) -> forPM25table
    forPM25table
  })
  #### 2025 PM 10 ----
  output$PM10_2025Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0.25)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -75.122, lat = 40, zoom = 11.25) %>% 
      addPolylines(data = CityLimits, weight = 2, color = 'black', opacity = 1) %>% 
      addPolylines(data = Neighborhoods, weight = 0.5, color = "navy", opacity = 1) %>% 
      addCircleMarkers(data = PM10_2025_PPMstats, 
                       fillColor = ifelse(PM10_2025_PPMstats$Mean<=50, "#999933", ifelse(
                         PM10_2025_PPMstats$Mean<=100, "#DDCC77", ifelse(
                           PM10_2025_PPMstats$Mean<=150,  "#CC6677", ifelse(
                             PM10_2025_PPMstats$Mean<=200,  "#882255", ifelse(
                               PM10_2025_PPMstats$Mean$aqi<300, "#330033"))))), 
                       fillOpacity = 0.55, 
                       stroke = FALSE, 
                       radius = ~rescale(PM10_2025_PPMstats$Mean, c(5,10)), 
                       popup = ~as.character(PM10_2025_PPMstats$SiteName),
                       label = ~paste(sprintf("%.2f",PM10_2025_PPMstats$Mean), "µg/m³", sep = " "), 
                       labelOptions = labelOptions(textsize = "12px", noHide = T, textOnly = F, opacity = .8, na.label = ""))
  })
  
  output$PM10_2025Trends <- renderPlot({
    ##### Plot PM 10 mg/m3 ----
    PM10_2025valuesPlot <- ggplot()+
      geom_smooth(data = Phila2025_PM10_24hr, 
                  aes(Date, Measurement, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2025-01-01"), Sys.Date()), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,30,5))+
      labs(x = "",
           y = "PM 10 µg/m³",
           title = "2025 PM 10 Trends", 
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,0,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9'))
    PM10_2025valuesPlot
  })
  
  output$PM10_2025Trends1 <- renderPlot({
    ##### Plot PM 10 mg/m3 ----
    PM10_2025valuesPlot <- ggplot()+
      geom_smooth(data = Phila2025_PM10_24hr, 
                  aes(Date, Measurement, 
                      color = SiteName,
                      fill = SiteName))+
      theme_bw()+
      scale_x_date(limits = c(as.Date("2025-01-01"), Sys.Date()), 
                   expand = c(0,0),
                   date_breaks = "1 month",
                   date_labels = "%b %d")+
      scale_y_continuous(limits = c(0, NA), breaks = seq(0,3,0.5))+
      labs(x = "",
           y = "PM 10 µg/m³",
           title = "2025 PM 10 Trends", 
           color = "Site Name",
           fill = "Site Name")+
      theme(axis.text.y=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.position = "right",
            legend.text=element_text(size=14),
            legend.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size=14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            plot.margin = margin(5,0,0,25, "points"))+
      scale_colour_manual(values = c("NEW" = '#56B4E9'))+
      scale_fill_manual(values = c("NEW" = '#56B4E9'))
    PM10_2025valuesPlot
  })
  
  output$PM10_boxplots2025 <- renderPlot({
    #########  PM 10 BOXPLOT-----
    # Phila2025_SO2_24hr$SiteName <- factor(Phila2025_SO2_24hr$SiteName, levels = c("NEA", "LABP", "NEW"))
    PM10_2025_box <- ggplot(Phila2025_PM10_24hr, aes(Measurement, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9'))+
      labs(y = "",
           x = "PM 10 µg/m³",
           title = "2025 PM 10 Distributions",
           fill = "SiteName") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,50, 10))+
      scale_y_discrete()+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,0, "points"))
    PM10_2025_box
  })
  
  output$PM10_boxplots2025_1 <- renderPlot({
    #########  PM 10 BOXPLOT-----
    # Phila2025_SO2_24hr$SiteName <- factor(Phila2025_SO2_24hr$SiteName, levels = c("NEA", "LABP", "NEW"))
    PM10_2025_box <- ggplot(Phila2025_PM10_24hr, aes(Measurement, SiteName, fill = SiteName))+
      geom_boxplot(notch = F, outlier.color = '#990066', outlier.shape=20, outlier.size = 3, linewidth=0.7)+
      stat_boxplot(geom = 'errorbar')+
      theme_bw()+
      scale_fill_manual(values = c("NEW" = '#56B4E9'))+
      labs(y = "",
           x = "PM 10 µg/m³",
           title = "2025 PM 10 Distributions",
           fill = "SiteName") +
      scale_x_continuous(limits = c(0,NA), breaks = seq(0,3, 0.5))+
      scale_y_discrete()+
      theme(axis.text.y=element_text(size=14),
            axis.text.x=element_text(size=14),
            panel.border=element_blank(),
            axis.line.x = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            axis.line.y = element_line(linewidth = 0.5, 
                                       linetype = "solid", colour = "black"),
            plot.title.position = 'plot',
            plot.subtitle= element_text(hjust=0.5),
            legend.position = "none",
            plot.title = element_text(face = "bold",hjust=0.5, size = 20),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.margin = margin(5,8,5,0, "points"))
    PM10_2025_box
  })
  
  output$PM10_DistTable <- renderDataTable({
    PM10_2025_PPMstats %>% 
      as.data.frame() %>% 
      mutate_if(is.numeric, ~round(., 3))-> forPM10table
    forPM10table %>% 
      select(1:7) -> forPM10table
    DT::datatable(forPM10table, 
                  rownames = F, 
                  colnames = c("Site Name", "Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum", "Mean"),
                  options = list(paging=F,
                                 searching=F),
                  class = "display") %>%
      formatStyle('SiteName',
                  target = "row",
                  backgroundColor = styleEqual(c("NEW"), 
                                               c("NEW" = '#56B4E9'))) -> forPM10table
    forPM10table
  })
}

shinyApp(ui = ui, server = server)
