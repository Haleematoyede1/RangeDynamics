# Title:    Single season  Occupancy Analysis (2008 - 2009)
# File:     01_01_SSOCM.R
# Project:  Range Expansion Of Hadedas Ibis In South Africa 2008 - 2023
# Author:   Oyede Haleemat

# Install required Packages
#install.packages("remotes")
#remotes::install_github("African/ABAP")

# Clear R environment
rm(list = ls())
getwd()
setwd("C:/Users/OYDHAL001/Desktop/RangeDynamics")
#ls()#list all the files in the environment
#rm(list = c("p1", "p2","p3","p4","p5","p6","p7","p8","p9"))#remove specific files
#file.rename("filename.r", "folder/filename.r")#move file to a folder


##  ================================Data Preparation==================================
#Load Required Libraries
#To begin,  load the necessary R libraries:

library(ABAP)
library(dplyr)

#Retrieve and Clean Data
# retrieve the bird data for Hadedas Ibis in South Africa for the year 2008 and perform initial data cleaning:

Hadedas <- getAbapData(.spp = 84, 
                       .region_type = "country", 
                       .region = "SouthAfrica",
                       .years = 2016:2017)

Hadedas <- select(Hadedas, -c(StartTime,Hour1,Hour2,Hour3,Hour4,Hour5,Hour6,Hour7,Hour8,Hour9,Hour10,
                              TotalSpp,InclNight,AllHabitats,Common_name,Taxonomic_name))

Hadedas <- Hadedas %>%
  mutate(Spp = ifelse(Spp == '84', 1, 
                      ifelse(Spp == '-', 0, NA_integer_)))

sum(is.na(Hadedas))

Hadedas <- Hadedas %>%
  as.data.frame() %>%
  mutate(Sequence = ifelse(Sequence == '-', 0, as.numeric(Sequence))) %>%
  rename(TotalSpecies = Sequence)


##  ================================Filter Protocols==================================
# filter out protocols that do not meet the standard requirements:

sum(Hadedas$TotalHours<2, na.rm = TRUE)
Hadedas <- Hadedas[!(Hadedas$TotalHours < 2 | is.na(Hadedas$TotalHours)), ]

Hadedas$StartDate <- as.Date(Hadedas$StartDate, format = '%Y-%m-%d')
Hadedas$EndDate <- as.Date(Hadedas$EndDate, format = '%Y-%m-%d')

Hadedas <- Hadedas %>%
  mutate(IntervalDays = as.numeric(difftime(EndDate, StartDate, units = "days")))

Hadedas <- Hadedas[Hadedas$IntervalDays >= 0 & Hadedas$IntervalDays <= 5, ]

##  ================================Geospatial Data Preparation==========================
# create latitude and longitude columns for each pentad:

Hadedas$Latitude <- -(as.numeric(substr(Hadedas$Pentad, 1, 2)) + (as.numeric(substr(Hadedas$Pentad, 3, 4)) + 2.5) / 60)
Hadedas$Longitude <- (as.numeric(substr(Hadedas$Pentad, 6, 7)) + (as.numeric(substr(Hadedas$Pentad, 8, 9)) + 2.5) / 60)

##  ================================Survey Period Categorization=========================
#Load Required Libraries
library(lubridate)
library(dplyr)

# categorize records into survey periods:
Hadedas <- Hadedas %>%
  mutate(Survey = case_when(
    year(StartDate) == 2016 ~ "2016",
    year(StartDate) == 2017 ~ "2017",
        TRUE ~ "Other"
  ))

##  ================================Duplicate Check======================================
# check for duplicates based on unique fields:
unique_fields <- c("Pentad", "CardNo", "ObserverNo")

survey_groups <- Hadedas %>%
  group_by(across(all_of(unique_fields))) %>%
  summarise(n = n(), .groups = 'drop')

potential_duplicates <- survey_groups %>%
  filter(n > 1)

print(potential_duplicates)

##  ================================Data Rearrangement and Exclusion====================
# rearrange the columns and exclude unnecessary ones:
Hadedas <- Hadedas %>%
  select(-c(StartDate, EndDate, IntervalDays))

Hadedas <- Hadedas[,c(1,2,7,8,3,9,4,5,6)]

##  ================================Reporting Rates Investigation=======================
# load the required package
library(data.table)

# calculate the total reporting rate per pentad:
Total_rr_tab <- Hadedas %>%
  group_by(Pentad, Survey) %>%
  summarise(total_visits = n(),
            detections = sum(Spp > 0),
            rr = mean(Spp > 0, na.rm = TRUE)) %>%
  mutate(rr = round(rr, 2),
         failures = total_visits - detections,
         rr_percent = rr * 100,rr_bin = cut(rr_percent,
                                            breaks = c(0, 20, 40, 60, 80, 100),
                                            labels = c("0-20%", ">20-40%", ">40-60%", ">60-80%", ">80-100%"),
                                            include.lowest = TRUE))

Total_rr_tab <- as.data.frame(Total_rr_tab)
Total_rr_tab[is.na(Total_rr_tab)] <- 0# Replace NA with 0

# Compute Latitude and Longitude from Pentad
Total_rr_tab$Latitude <- -(as.numeric(substr(Total_rr_tab$Pentad, 1, 2)) + (as.numeric(substr(Total_rr_tab$Pentad, 3, 4)) + 2.5) / 60)
Total_rr_tab$Longitude <- (as.numeric(substr(Total_rr_tab$Pentad, 6, 7)) + (as.numeric(substr(Total_rr_tab$Pentad, 8, 9)) + 2.5) / 60)

##  ================================Reporting Rate Visualization=========================
#load the required libraries
library(ggplot2)
library(sf)

# Load map
gpkg_path <- 'C:/Users/OYDHAL001/Desktop/RangeDynamics/rasterFiles/gadm41_ZAF.gpkg'
SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1")  # State/Province level

# Visualization using ggplot2
ggplot() +
  geom_tile(data = Total_rr_tab, aes(x = Longitude, y = Latitude, fill = rr_bin)) +
  geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +
  ggtitle("Hadeda Ibis Reporting Rate (2008)") +
  
  # Create discrete bins with custom colors and labels
  scale_fill_manual(name = "Report rate (%)",
                    values = c(
                      "0-20%" = "yellow",
                      ">20-40%" = "#FFC200",
                      ">40-60%" = "#FF7F00",
                      ">60-80%" = "#FF4500",
                      ">80-100%" = "black"),drop = FALSE) +
  theme_bw() +theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "white"),  # Light cream background
    panel.border = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right")

##  ================================Covariate Data Integration===========================
# load and merge site-specific and seasonal covariate data:
library(readr)
siteSpecificCov <- read.csv("C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/siteSpecificCov2016_2019.csv") %>%
  rename(Pentad = pentad)%>%
  select(-c(1))

seasonalSiteCov <- read_csv("processedCovariates/seasonalSiteCov2016_17.csv") %>%
  rename(Pentad = pentad)%>%
  select(-c(1))

covs <- merge(siteSpecificCov, seasonalSiteCov, by = "Pentad", all.x = TRUE)

reportRate <- Total_rr_tab[,c("Pentad","Latitude","Longitude","Survey","detections","failures")]
reportRate <- merge(reportRate, covs, by = c("Pentad","Survey"), all.x = TRUE)
reportRate <- na.omit(reportRate)
#reportRate <- merge(reportRate, siteSpecificCov, by = "Pentad",all.x = TRUE)

##  ================================Data Selection and Transformation====================

# log transform the required variables
reportRate$meanPrecp <- log(reportRate$meanPrecp + 1)
reportRate$mean_elevation <- log(reportRate$mean_elevation + 1)
reportRate$mean_slope <- log(reportRate$mean_slope + 1)

# create proportion variables for landcover
reportRate$wetland_prop <- reportRate$perc_wetland/100
reportRate$urban_prop <- reportRate$perc_urban/100
reportRate$others_prop <- reportRate$perc_others/100
reportRate$grassland_prop <- reportRate$perc_grassland/100
reportRate$forest_prop <- reportRate$perc_forest/100
reportRate$cropland_prop <- reportRate$perc_cropland/100

# select relevant variables and transform them as necessary:
reportRate <- reportRate[, c("Pentad", "Latitude", "Longitude", 
                             "Survey", "detections", "failures", "mean_elevation", "mean_slope", 
                             "cropland_prop", "forest_prop", "grassland_prop", 
                             "others_prop", "urban_prop", "wetland_prop",
                             "meanMaxTemp", "meanMinTemp", "meanNDWI", "meanNDVI", "meanPrecp")]

# scale the required variables
scaledVar <- scale(reportRate[, c("mean_elevation", "mean_slope","meanMaxTemp",
                                  "meanMinTemp", "meanNDWI", "meanNDVI", "meanPrecp")])

# extract the per-columns means and standard deviations
means <- attr(scaledVar, "scaled:center")
sds <- attr(scaledVar, "scaled:scale")

reportRate <- data.frame(Pentad = reportRate$Pentad, 
                               Latitude = reportRate$Latitude, 
                               Longitude = reportRate$Longitude,
                               Survey = reportRate$Survey, 
                               detections = reportRate$detections, 
                               failures = reportRate$failures,
                               mean_elevation = scaledVar[,1], 
                               mean_slope = scaledVar[,2],
                               meanMaxTemp = scaledVar[,3], 
                               meanMinTemp = scaledVar[,4],
                               meanNDWI = scaledVar[,5], 
                               meanNDVI = scaledVar[,6], 
                               meanPrecp = scaledVar[,7],
                         cropland_prop = reportRate$cropland_prop, 
                         forest_prop = reportRate$forest_prop,
                         grassland_prop = reportRate$grassland_prop, 
                         others_prop = reportRate$others_prop,
                         urban_prop = reportRate$urban_prop, 
                         wetland_prop = reportRate$wetland_prop)

##  ================================Collinearity Checks===================================
# perform correlation and VIF checks to identify collinear variables

numeric_vars <- c("meanPrecp", "meanMaxTemp", "meanMinTemp",
                  "meanNDVI", "meanNDWI",
                  "mean_slope", "mean_elevation",
                  "forest_prop", "cropland_prop", "grassland_prop",
                  "others_prop", "urban_prop", "wetland_prop")

cor_matrix <- cor(reportRate[, numeric_vars], use = "complete.obs")

library(corrplot)
corrplot(cor_matrix, method = "color")
mtext("Correlation Matrix (2008)", side = 1, line = 4, cex = 1.2)

# VIF ch0ecks
df_complete <- na.omit(reportRate[, c("meanPrecp", "meanMaxTemp", "meanMinTemp","meanNDVI", "meanNDWI", 
                                      "mean_slope", "forest_prop", "cropland_prop", "grassland_prop",
                                   "urban_prop", "wetland_prop")])

lm_dummy <- lm(meanNDVI ~ meanPrecp + meanMaxTemp + meanMinTemp + meanNDWI+
                 mean_slope+ forest_prop + 
                 cropland_prop + grassland_prop + 
                 urban_prop + 
                 wetland_prop, 
               data = df_complete)

library(car)
vif_values <- vif(lm_dummy)
vif_values

##  ================================General linear Modeling===================================
model1 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI + urban_prop, 
              data = reportRate, family = binomial)#Climate and Habitat Drivers
model2 <- glm(cbind(detections, failures) ~ meanPrecp * urban_prop + meanNDVI, 
              data = reportRate, family = binomial)#Interaction Between Precipitation and Urban Cover
model3 <- glm(cbind(detections, failures) ~ meanPrecp * meanNDVI + urban_prop, 
              data = reportRate, family = binomial)#Interaction Between Precipitation and NDVI
model4 <- glm(cbind(detections, failures) ~ meanNDVI * urban_prop + meanPrecp, 
              data = reportRate, family = binomial)#Combining NDVI and Urban Interactions
model5 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI + urban_prop + meanMaxTemp, 
              data = reportRate, family = binomial)# Adding Temperature Effects
model6 <- glm(cbind(detections, failures) ~ meanMaxTemp * meanNDVI + meanPrecp + urban_prop, 
              data = reportRate, family = binomial)# Interaction Between Temperature and NDVI
model7 <- glm(cbind(detections, failures) ~ meanMaxTemp + meanNDVI * urban_prop + meanPrecp, 
              data = reportRate, family = binomial)#Combined Temperature, NDVI, and Urban Effects
model8 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDWI + urban_prop, 
              data = reportRate, family = binomial)# NDWI as a Water Availability Proxy
model9 <- glm(cbind(detections, failures) ~ meanPrecp * meanNDWI + urban_prop, 
              data = reportRate, family = binomial)#NDWI × Precipitation Interaction
model10 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDWI + meanMaxTemp + urban_prop, 
               data = reportRate, family = binomial)#NDWI, Precipitation, and Temperature
model11 <- glm(cbind(detections, failures) ~ meanPrecp * meanMaxTemp + urban_prop + meanNDVI, 
               data = reportRate, family = binomial)#Temperature × Precipitation Interaction
model12 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI + meanNDWI + urban_prop, 
               data = reportRate, family = binomial)# Combined Precipitation, NDVI, and NDWI
model13 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI + urban_prop, 
               data = reportRate, family = binomial)#Grassland and Forest Effects Removed
model14 <- glm(cbind(detections, failures) ~ meanNDVI * urban_prop + meanPrecp, 
               data = reportRate, family = binomial)#Core Habitat Model + NDVI Interaction
model15 <- glm(cbind(detections, failures) ~ meanNDWI * urban_prop + meanPrecp, 
               data = reportRate, family = binomial)#NDWI and Urban Interaction
model16 <- glm(cbind(detections, failures) ~ meanPrecp * meanNDWI + meanMaxTemp + urban_prop, 
               data = reportRate, family = binomial)#Precipitation and NDWI Interaction + Temperature

model17 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDWI + meanMaxTemp + cropland_prop, 
               data = reportRate, family = binomial)#NDWI, Precipitation, and Temperature
model18 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI + meanMaxTemp + cropland_prop, 
               data = reportRate, family = binomial)#NDVI, Precipitation, and Temperature

model19 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDWI + meanMaxTemp + grassland_prop, 
               data = reportRate, family = binomial)#NDWI, Precipitation, and Temperature
model20 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI + meanMaxTemp + grassland_prop, 
               data = reportRate, family = binomial)#NDVI, Precipitation, and Temperature

models_refined <- list(
  model1, model2, model3, model4, model5, model6, model7, model8,
  model9, model10, model11, model12, model13, model14, model15, model16,
  model17,model18,model19,model20
)

model_summary <- data.frame(
  Model = paste("Model", 1:20),
  AIC = sapply(models_refined, AIC),
  Deviance = sapply(models_refined, function(x) x$deviance),
  Null_Deviance = sapply(models_refined, function(x) x$null.deviance),
  df_residual = sapply(models_refined, function(x) x$df.residual)
)

# Sort by AIC (best model first)
model_summary <- model_summary[order(model_summary$AIC),]
print(model_summary)
# the best model is model11 with the lowest AIC value
## ================================fitted regression lines=================================


##  ================================Model Diagnostics======================================
# Let's create a new dataset to be used for model diagnostics.
#Reload the covariates data, as it contains the entire grid 
#cells for the whole country and is sufficiently large to make predictions from.
# 
# #load the covariate data
# siteSpecificCov <- read.csv("C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/siteSpecificCov.csv") %>%
#   rename(Pentad = pentad)
# 
# seasonalSiteCov <- read_csv("processedCovariates/seasonalSiteCov2008_09.csv") %>%
#   rename(Pentad = pentad)

# lets use the covs datasets to create a new dataset for model diagnostics & validation
validationData <- covs
validationData <- validationData[,c("Pentad","perc_urban","perc_others", 
                                    "meanMaxTemp", "meanMinTemp",
                                    "meanNDWI", "meanNDVI", "meanPrecp")]


validationData$meanPrecp <- log(validationData$meanPrecp + 1)
validationData$meanPrecp <- (validationData$meanPrecp - means["meanPrecp"]) / sds["meanPrecp"]

validationData$meanMaxTemp <- (validationData$meanMaxTemp - means["meanMaxTemp"]) / sds["meanMaxTemp"]
validationData$meanMinTemp <- (validationData$meanMinTemp - means["meanMinTemp"]) / sds["meanMinTemp"]
validationData$meanNDVI <- (validationData$meanNDVI - means["meanNDVI"]) / sds["meanNDVI"]
validationData$meanNDWI <- (validationData$meanNDWI - means["meanNDWI"]) / sds["meanNDWI"]

validationData$urban_prop <- validationData$perc_urban/100
validationData[is.na(validationData)] <- 0
validationData$predictions <- predict(model11, newdata = validationData, type = "response")
summary(validationData$predictions)

# Compute Latitude and Longitude from Pentad
validationData$Latitude <- -(as.numeric(substr(validationData$Pentad, 1, 2)) + (as.numeric(substr(validationData$Pentad, 3, 4)) + 2.5) / 60)
validationData$Longitude <- (as.numeric(substr(validationData$Pentad, 6, 7)) + (as.numeric(substr(validationData$Pentad, 8, 9)) + 2.5) / 60)
summary(validationData$predictions)

##  ================================Model Visualization====================================
#load the required libraries
library(ggplot2)
library(raster)
library(sp)
library(sf)

# Load map
gpkg_path <- 'C:/Users/OYDHAL001/Desktop/RangeDynamics/rasterFiles/gadm41_ZAF.gpkg'
SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1")  # State/Province level
#SA_national <- st_read(gpkg_path, layer = "ADM_ADM_0")


# Define and transform projections
map_proj <- st_crs(4326)
SA_province <- st_transform(SA_province, crs = map_proj)

# Define custom colors
custom_colors <- c("#FCFAF9","#F3CF9F", "#26F111","#1F3716")
#c("#FCFAF9","#F3CF9F", "#9DF016","#26F111","#1F3716")

Spp <- as.numeric(cut(validationData$predictions, 20))  # grouping in 20

# Plot the predicted occupancy map

ggplot() +
  geom_tile(data = validationData, aes(x = Longitude, y = Latitude, fill = Spp/20)) +
  
  # Fix legend range from 0.0 to 1.0 and rename legend title
  scale_fill_gradientn(
    name = 'Predicted occupancy',  
    colors = custom_colors,
    na.value = "transparent",
    limits = c(0, 1),  
    breaks = seq(0, 1, by = 0.2)
  ) +
  
  geom_sf(data = SA_province, fill = NA, color = "black", size = 1) +
  
  theme_minimal() +
  labs(
    title = "Predicted occupancy map (2016/17)"
  ) +
  
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),   # Remove x-axis label
    axis.title.y = element_blank(),   # Remove y-axis label
    axis.text.x = element_blank(),    # Remove x-axis text
    axis.text.y = element_blank(),    # Remove y-axis text
    axis.ticks = element_blank(),     # Remove axis ticks
    panel.border = element_blank(),   # Remove panel border
    legend.key.size = unit(1.2, "cm"),
    legend.title = element_text(size = 12),  
    legend.text = element_text(size = 10)  
  )


