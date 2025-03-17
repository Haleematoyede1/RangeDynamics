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
setwd("C:/Users/OYDHAL001/Documents/DynamicOccupancyModels")
#ls()#list all the files in the environment
#rm(list = c("p1", "p2","p3","p4","p5","p6","p7","p8","p9"))#remove specific files


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
                       .years = 2008)

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
# categorize records into survey periods:
Hadedas <- Hadedas %>%
  mutate(Survey = case_when(
    year(StartDate) == 2008 ~ "2008",
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
# calculate the total reporting rate per pentad:
Total_rr_tab <- tapply(Hadedas$Spp, list(Hadedas$Pentad, Hadedas$Survey), mean)
print(Total_rr_tab, digits = 2)

Total_rr_tab <- as.data.frame(Total_rr_tab)
Total_rr_tab$Pentad <- rownames(Total_rr_tab)

Total_rr_tab <- data.table(Total_rr_tab)
Total_rr_tab <- melt(Total_rr_tab, id.vars='Pentad')
colnames(Total_rr_tab) <- c("Pentad", "Survey", "rr")

Total_rr_tab[is.na(Total_rr_tab)] <- 0

Total_rr_tab$Latitude <- -(as.numeric(substr(Total_rr_tab$Pentad, 1, 2)) + (as.numeric(substr(Total_rr_tab$Pentad, 3, 4)) + 2.5) / 60)
Total_rr_tab$Longitude <- (as.numeric(substr(Total_rr_tab$Pentad, 6, 7)) + (as.numeric(substr(Total_rr_tab$Pentad, 8, 9)) + 2.5) / 60)

##  ================================Reporting Rate Visualization=========================
# visualize the reporting rates using a heatmap:
gpkg_path <- 'C:/Users/OYDHAL001/Documents/DynamicOccupancyModels/DOCM_Analysis/GADM/gadm41_ZAF.gpkg'
SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1")  # State/Province level

ggplot() +
  geom_tile(data = Total_rr_tab, aes(x = Longitude, y = Latitude, fill = rr)) +
  geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +
  ggtitle("Hadeda ibis reporting rate 2008") +
  scale_fill_distiller('Report rate', palette = 'Spectral', na.value = "transparent") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_blank())

##  ================================Covariate Data Integration===========================
# load and merge site-specific and seasonal covariate data:
siteSpecificCov2008_2009 <- read.csv("C:/Users/OYDHAL001/Documents/DynamicOccupancyModels/siteSpecificCov2008_2009.csv") %>%
  rename(Pentad = pentad)

seasonalSiteCov2008 <- read.csv("C:/Users/OYDHAL001/Documents/DynamicOccupancyModels/seasonalSiteCov2008.csv") %>%
  rename(Pentad = pentad)

HadNew <- merge(Hadedas, siteSpecificCov2008_2009, by = "Pentad", all.x = TRUE)
HadNew <- merge(HadNew, seasonalSiteCov2008, by = "Pentad", all.x = TRUE)

##  ================================Data Selection and Transformation====================
# select relevant variables and transform them as necessary:
HadNew <- select(HadNew, Pentad, Latitude, Longitude, 
                 ObserverNo, Survey, TotalHours, Spp, 
                 TotalSpecies, mean_elevation, mean_slope, 
                 perc_cropland, perc_forest, perc_grassland, 
                 perc_others, perc_urban, perc_wetland,
                 meanMaxTemp, meanMinTemp, meanNDWI, meanNDVI, meanPrecp)

HadNew$meanPrecp <- log(HadNew$meanPrecp + 1)

HadNew$meanPrecp <- scale(HadNew$meanPrecp)

HadNew$meanMaxTemp <- scale(HadNew$meanMaxTemp)
HadNew$meanMinTemp <- scale(HadNew$meanMinTemp)

HadNew$meanNDVI <- scale(HadNew$meanNDVI)
HadNew$meanNDWI <- scale(HadNew$meanNDWI)

HadNew$mean_elevation <- log(HadNew$mean_elevation + 1)
HadNew$mean_elevation <- scale(HadNew$mean_elevation)

HadNew$mean_slope <- log(HadNew$mean_slope + 1)
HadNew$mean_slope <- scale(HadNew$mean_slope)

HadNew$wetland_prop <- HadNew$perc_wetland/100
HadNew$urban_prop <- HadNew$perc_urban/100
HadNew$others_prop <- HadNew$perc_others/100
HadNew$grassland_prop <- HadNew$perc_grassland/100
HadNew$forest_prop <- HadNew$perc_forest/100
HadNew$cropland_prop <- HadNew$perc_cropland/100

##  ================================Collinearity Checks===================================
# perform correlation and VIF checks to identify collinear variables

numeric_vars <- c("meanPrecp", "meanMaxTemp", "meanMinTemp",
                  "meanNDVI", "meanNDWI",
                  "mean_slope", "mean_elevation",
                  "forest_prop", "cropland_prop", "grassland_prop",
                  "others_prop", "urban_prop", "wetland_prop")

cor_matrix <- cor(HadNew[, numeric_vars], use = "complete.obs")

library(corrplot)
corrplot(cor_matrix, method = "color")

# VIF checks
df_complete <- na.omit(HadNew[, c("meanPrecp", "meanMaxTemp", "meanMinTemp",
                                  "meanNDVI", "meanNDWI",
                                  "mean_slope", "mean_elevation",
                                  "forest_prop", "cropland_prop", "grassland_prop",
                                  "others_prop", "urban_prop", "wetland_prop")])

lm_dummy <- lm(meanNDVI ~ meanPrecp + meanMaxTemp + meanMinTemp + meanNDWI+
                 mean_slope + mean_elevation + forest_prop + 
                 cropland_prop + grassland_prop + 
                 urban_prop + 
                 wetland_prop, 
               data = df_complete)

library(car)
vif_values <- vif(lm_dummy)
vif_values

##  ================================Principal Component Analysis (PCA) ===================
# perform PCA on highly correlated variables:
df_pca <- na.omit(HadNew[,c("meanMinTemp", "meanMaxTemp", "mean_elevation")])
pca_res <- prcomp(df_pca, scale = FALSE)
summary(pca_res)

HadNew$pc1 <- pca_res$x[,1]
HadNew$pc2 <- pca_res$x[,2]

##  ================================Final Data Preparation===============================
# finalize the dataset for occupancy modeling:
HadData2008 <- HadNew[,c("Pentad", "Latitude", "Longitude", "ObserverNo", 
                         "Survey", "TotalHours", "Spp", "TotalSpecies", 
                         "meanPrecp","meanNDVI", "meanNDWI", "pc1", "pc2",
                         "mean_slope", "cropland_prop",
                         "forest_prop", "grassland_prop", 
                         "urban_prop", "wetland_prop")]

#write.csv(HadData2008, "HadFinalData2008.csv", row.names = FALSE)

##  ================================Occupancy Modeling===================================
# load the prepared data and set up the occupancy