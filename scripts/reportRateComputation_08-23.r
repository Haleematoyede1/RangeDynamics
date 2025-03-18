# Title:    Single season  Occupancy Analysis (2008 - 2009)
# File:     GLM_2008-09.r
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
# #Load Required Libraries
# #To begin,  load the necessary R libraries:
# 
# library(ABAP)
# library(dplyr)
# 
# #Retrieve and Clean Data
# # retrieve the bird data for Hadedas Ibis in South Africa for the year 2008/09 and perform initial data cleaning:
# 
# Hadedas <- getAbapData(.spp = 84, 
#                        .region_type = "country", 
#                        .region = "SouthAfrica",
#                        .years = 2008:2009)
# 
# Hadedas <- select(Hadedas, -c(StartTime,Hour1,Hour2,Hour3,Hour4,Hour5,Hour6,Hour7,Hour8,Hour9,Hour10,
#                               TotalSpp,InclNight,AllHabitats,Common_name,Taxonomic_name))
# 
# Hadedas <- Hadedas %>%
#   mutate(Spp = ifelse(Spp == '84', 1, 
#                       ifelse(Spp == '-', 0, NA_integer_)))
# 
# sum(is.na(Hadedas))
# 
# Hadedas <- Hadedas %>%
#   as.data.frame() %>%
#   mutate(Sequence = ifelse(Sequence == '-', 0, as.numeric(Sequence))) %>%
#   rename(TotalSpecies = Sequence)
# 
# ##  ================================Filter Protocols==================================
# # filter out protocols that do not meet the standard requirements:
# 
# sum(Hadedas$TotalHours<2, na.rm = TRUE)
# Hadedas <- Hadedas[!(Hadedas$TotalHours < 2 | is.na(Hadedas$TotalHours)), ]
# 
# Hadedas$StartDate <- as.Date(Hadedas$StartDate, format = '%Y-%m-%d')
# Hadedas$EndDate <- as.Date(Hadedas$EndDate, format = '%Y-%m-%d')
# 
# Hadedas <- Hadedas %>%
#   mutate(IntervalDays = as.numeric(difftime(EndDate, StartDate, units = "days")))
# 
# Hadedas <- Hadedas[Hadedas$IntervalDays >= 0 & Hadedas$IntervalDays <= 5, ]
# 
# ##  ================================Geospatial Data Preparation==========================
# # create latitude and longitude columns for each pentad:
# 
# Hadedas$Latitude <- -(as.numeric(substr(Hadedas$Pentad, 1, 2)) + (as.numeric(substr(Hadedas$Pentad, 3, 4)) + 2.5) / 60)
# Hadedas$Longitude <- (as.numeric(substr(Hadedas$Pentad, 6, 7)) + (as.numeric(substr(Hadedas$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Survey Period Categorization=========================
# #Load Required Libraries
# library(lubridate)
# library(dplyr)
# 
# # categorize records into survey periods:
# Hadedas <- Hadedas %>%
#   mutate(Survey = case_when(
#     year(StartDate) == 2008 ~ "2008",
#     year(StartDate) == 2009 ~ "2009",
#     TRUE ~ "Other"
#   ))
# 
# ##  ================================Duplicate Check======================================
# # check for duplicates based on unique fields:
# unique_fields <- c("Pentad", "CardNo", "ObserverNo")
# 
# survey_groups <- Hadedas %>%
#   group_by(across(all_of(unique_fields))) %>%
#   summarise(n = n(), .groups = 'drop')
# 
# potential_duplicates <- survey_groups %>%
#   filter(n > 1)
# 
# print(potential_duplicates)
# 
# ##  ================================Data Rearrangement and Exclusion====================
# # rearrange the columns and exclude unnecessary ones:
# Hadedas <- Hadedas %>%
#   select(-c(StartDate, EndDate, IntervalDays))
# 
# Hadedas <- Hadedas[,c(1,2,7,8,3,9,4,5,6)]
# 
# ##  ================================Reporting Rates Investigation=======================
# # load the required package
# library(data.table)
# 
# # calculate the total reporting rate per pentad:
# Total_rr_tab <- Hadedas %>%
#   group_by(Pentad, Survey) %>%
#   summarise(total_visits = n(),
#             detections = sum(Spp > 0),
#             rr = mean(Spp > 0, na.rm = TRUE)) %>%
#   mutate(rr = round(rr, 2),
#          failures = total_visits - detections,
#          rr_percent = rr * 100,rr_bin = cut(rr_percent,
#                                             breaks = c(0, 20, 40, 60, 80, 100),
#                                             labels = c("0-20%", ">20-40%", ">40-60%", ">60-80%", ">80-100%"),
#                                             include.lowest = TRUE))
# 
# Total_rr_tab <- as.data.frame(Total_rr_tab)
# Total_rr_tab[is.na(Total_rr_tab)] <- 0# Replace NA with 0
# 
# # Compute Latitude and Longitude from Pentad
# Total_rr_tab$Latitude <- -(as.numeric(substr(Total_rr_tab$Pentad, 1, 2)) + (as.numeric(substr(Total_rr_tab$Pentad, 3, 4)) + 2.5) / 60)
# Total_rr_tab$Longitude <- (as.numeric(substr(Total_rr_tab$Pentad, 6, 7)) + (as.numeric(substr(Total_rr_tab$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Reporting Rate Visualization=========================
# #load the required libraries
# library(ggplot2)
# library(sf)
# library(RColorBrewer)
# 
# # Load map
# gpkg_path <- 'C:/Users/OYDHAL001/Desktop/RangeDynamics/rasterFiles/gadm41_ZAF.gpkg'
# SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1")  # State/Province level

# Visualization using ggplot2
# 
# ggplot() +
#   geom_tile(data = Total_rr_tab, aes(x = Longitude, y = Latitude, fill = rr_bin)) +
#   geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +
#   ggtitle("Reporting Rate (2008/09)") +
#   scale_fill_manual(name = "Report rate (%)",
#                     values = c(
#                       "0-20%" = "#ffff99",
#                       ">20-40%" = "#ffa366",
#                       ">40-60%" = "#FFC200",
#                       ">60-80%" = "#FF4500",
#                       ">80-100%" = "#4d4d00"),drop = FALSE) +
#   theme_bw() +theme(plot.title = element_text(hjust = 0.5),
#     panel.background = element_rect(fill = "white"),panel.border = element_blank(),
#     legend.key = element_rect(fill = "white"),legend.position = "right",
#     axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
# 
# #save the Total_rr_tab data
# write.csv(Total_rr_tab, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedData/Total_rr_2008-09.csv")
##########################################################################################
##########################################################################################
##########################################################################################


# # Clear R environment
# rm(list = ls())
# getwd()
# setwd("C:/Users/OYDHAL001/Desktop/RangeDynamics")
# #ls()#list all the files in the environment
# #rm(list = c("p1", "p2","p3","p4","p5","p6","p7","p8","p9"))#remove specific files
# #file.rename("filename.r", "folder/filename.r")#move file to a folder
# 
# ##  ================================Data Preparation==================================
# #Load Required Libraries
# #To begin,  load the necessary R libraries:
# 
# library(ABAP)
# library(dplyr)
# 
# #Retrieve and Clean Data
# # retrieve the bird data for Hadedas Ibis in South Africa for the year 2008/09 and perform initial data cleaning:
# 
# Hadedas <- getAbapData(.spp = 84, 
#                        .region_type = "country", 
#                        .region = "SouthAfrica",
#                        .years = 2010:2011)
# 
# Hadedas <- select(Hadedas, -c(StartTime,Hour1,Hour2,Hour3,Hour4,Hour5,Hour6,Hour7,Hour8,Hour9,Hour10,
#                               TotalSpp,InclNight,AllHabitats,Common_name,Taxonomic_name))
# 
# Hadedas <- Hadedas %>%
#   mutate(Spp = ifelse(Spp == '84', 1, 
#                       ifelse(Spp == '-', 0, NA_integer_)))
# 
# sum(is.na(Hadedas))
# 
# Hadedas <- Hadedas %>%
#   as.data.frame() %>%
#   mutate(Sequence = ifelse(Sequence == '-', 0, as.numeric(Sequence))) %>%
#   rename(TotalSpecies = Sequence)
# 
# ##  ================================Filter Protocols==================================
# # filter out protocols that do not meet the standard requirements:
# 
# sum(Hadedas$TotalHours<2, na.rm = TRUE)
# Hadedas <- Hadedas[!(Hadedas$TotalHours < 2 | is.na(Hadedas$TotalHours)), ]
# 
# Hadedas$StartDate <- as.Date(Hadedas$StartDate, format = '%Y-%m-%d')
# Hadedas$EndDate <- as.Date(Hadedas$EndDate, format = '%Y-%m-%d')
# 
# Hadedas <- Hadedas %>%
#   mutate(IntervalDays = as.numeric(difftime(EndDate, StartDate, units = "days")))
# 
# Hadedas <- Hadedas[Hadedas$IntervalDays >= 0 & Hadedas$IntervalDays <= 5, ]
# 
# ##  ================================Geospatial Data Preparation==========================
# # create latitude and longitude columns for each pentad:
# 
# Hadedas$Latitude <- -(as.numeric(substr(Hadedas$Pentad, 1, 2)) + (as.numeric(substr(Hadedas$Pentad, 3, 4)) + 2.5) / 60)
# Hadedas$Longitude <- (as.numeric(substr(Hadedas$Pentad, 6, 7)) + (as.numeric(substr(Hadedas$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Survey Period Categorization=========================
# #Load Required Libraries
# library(lubridate)
# library(dplyr)
# 
# # categorize records into survey periods:
# Hadedas <- Hadedas %>%
#   mutate(Survey = case_when(
#     year(StartDate) == 2010 ~ "2010",
#     year(StartDate) == 2011 ~ "2011",
#     TRUE ~ "Other"
#   ))
# 
# ##  ================================Duplicate Check======================================
# # check for duplicates based on unique fields:
# unique_fields <- c("Pentad", "CardNo", "ObserverNo")
# 
# survey_groups <- Hadedas %>%
#   group_by(across(all_of(unique_fields))) %>%
#   summarise(n = n(), .groups = 'drop')
# 
# potential_duplicates <- survey_groups %>%
#   filter(n > 1)
# 
# print(potential_duplicates)
# 
# ##  ================================Data Rearrangement and Exclusion====================
# # rearrange the columns and exclude unnecessary ones:
# Hadedas <- Hadedas %>%
#   select(-c(StartDate, EndDate, IntervalDays))
# 
# Hadedas <- Hadedas[,c(1,2,7,8,3,9,4,5,6)]
# 
# ##  ================================Reporting Rates Investigation=======================
# # load the required package
# library(data.table)
# 
# # calculate the total reporting rate per pentad:
# Total_rr_tab <- Hadedas %>%
#   group_by(Pentad, Survey) %>%
#   summarise(total_visits = n(),
#             detections = sum(Spp > 0),
#             rr = mean(Spp > 0, na.rm = TRUE)) %>%
#   mutate(rr = round(rr, 2),
#          failures = total_visits - detections,
#          rr_percent = rr * 100,rr_bin = cut(rr_percent,
#                                             breaks = c(0, 20, 40, 60, 80, 100),
#                                             labels = c("0-20%", ">20-40%", ">40-60%", ">60-80%", ">80-100%"),
#                                             include.lowest = TRUE))
# 
# Total_rr_tab <- as.data.frame(Total_rr_tab)
# Total_rr_tab[is.na(Total_rr_tab)] <- 0# Replace NA with 0
# 
# # Compute Latitude and Longitude from Pentad
# Total_rr_tab$Latitude <- -(as.numeric(substr(Total_rr_tab$Pentad, 1, 2)) + (as.numeric(substr(Total_rr_tab$Pentad, 3, 4)) + 2.5) / 60)
# Total_rr_tab$Longitude <- (as.numeric(substr(Total_rr_tab$Pentad, 6, 7)) + (as.numeric(substr(Total_rr_tab$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Reporting Rate Visualization=========================
# #load the required libraries
# library(ggplot2)
# library(sf)
# library(RColorBrewer)
# 
# # Load map
# gpkg_path <- 'C:/Users/OYDHAL001/Desktop/RangeDynamics/rasterFiles/gadm41_ZAF.gpkg'
# SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1")  # State/Province level
# 
# # Visualization using ggplot2
# 
# ggplot() +
#   geom_tile(data = Total_rr_tab, aes(x = Longitude, y = Latitude, fill = rr_bin)) +
#   geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +
#   ggtitle("Reporting Rate (2010/11)") +
#   scale_fill_manual(name = "Report rate (%)",
#                     values = c(
#                       "0-20%" = "#ffff99",
#                       ">20-40%" = "#ffa366",
#                       ">40-60%" = "#FFC200",
#                       ">60-80%" = "#FF4500",
#                       ">80-100%" = "#4d4d00"),drop = FALSE) +
#   theme_bw() +theme(plot.title = element_text(hjust = 0.5),
#                     panel.background = element_rect(fill = "white"),panel.border = element_blank(),
#                     legend.key = element_rect(fill = "white"),legend.position = "right",
#                     axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
# 
# #save the Total_rr_tab data
# write.csv(Total_rr_tab, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedData/Total_rr_2010-11.csv")
# ##########################################################################################
# ##########################################################################################
# ##########################################################################################

# 
# # Clear R environment
# rm(list = ls())
# getwd()
# setwd("C:/Users/OYDHAL001/Desktop/RangeDynamics")
# #ls()#list all the files in the environment
# #rm(list = c("p1", "p2","p3","p4","p5","p6","p7","p8","p9"))#remove specific files
# #file.rename("filename.r", "folder/filename.r")#move file to a folder
# 
# ##  ================================Data Preparation==================================
# #Load Required Libraries
# #To begin,  load the necessary R libraries:
# 
# library(ABAP)
# library(dplyr)
# 
# #Retrieve and Clean Data
# # retrieve the bird data for Hadedas Ibis in South Africa for the year 2008/09 and perform initial data cleaning:
# 
# Hadedas <- getAbapData(.spp = 84, 
#                        .region_type = "country", 
#                        .region = "SouthAfrica",
#                        .years = 2012:2013)
# 
# Hadedas <- select(Hadedas, -c(StartTime,Hour1,Hour2,Hour3,Hour4,Hour5,Hour6,Hour7,Hour8,Hour9,Hour10,
#                               TotalSpp,InclNight,AllHabitats,Common_name,Taxonomic_name))
# 
# Hadedas <- Hadedas %>%
#   mutate(Spp = ifelse(Spp == '84', 1, 
#                       ifelse(Spp == '-', 0, NA_integer_)))
# 
# sum(is.na(Hadedas))
# 
# Hadedas <- Hadedas %>%
#   as.data.frame() %>%
#   mutate(Sequence = ifelse(Sequence == '-', 0, as.numeric(Sequence))) %>%
#   rename(TotalSpecies = Sequence)
# 
# ##  ================================Filter Protocols==================================
# # filter out protocols that do not meet the standard requirements:
# 
# sum(Hadedas$TotalHours<2, na.rm = TRUE)
# Hadedas <- Hadedas[!(Hadedas$TotalHours < 2 | is.na(Hadedas$TotalHours)), ]
# 
# Hadedas$StartDate <- as.Date(Hadedas$StartDate, format = '%Y-%m-%d')
# Hadedas$EndDate <- as.Date(Hadedas$EndDate, format = '%Y-%m-%d')
# 
# Hadedas <- Hadedas %>%
#   mutate(IntervalDays = as.numeric(difftime(EndDate, StartDate, units = "days")))
# 
# Hadedas <- Hadedas[Hadedas$IntervalDays >= 0 & Hadedas$IntervalDays <= 5, ]
# 
# ##  ================================Geospatial Data Preparation==========================
# # create latitude and longitude columns for each pentad:
# 
# Hadedas$Latitude <- -(as.numeric(substr(Hadedas$Pentad, 1, 2)) + (as.numeric(substr(Hadedas$Pentad, 3, 4)) + 2.5) / 60)
# Hadedas$Longitude <- (as.numeric(substr(Hadedas$Pentad, 6, 7)) + (as.numeric(substr(Hadedas$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Survey Period Categorization=========================
# #Load Required Libraries
# library(lubridate)
# library(dplyr)
# 
# # categorize records into survey periods:
# Hadedas <- Hadedas %>%
#   mutate(Survey = case_when(
#     year(StartDate) == 2012 ~ "2012",
#     year(StartDate) == 2013 ~ "2013",
#     TRUE ~ "Other"
#   ))
# 
# ##  ================================Duplicate Check======================================
# # check for duplicates based on unique fields:
# unique_fields <- c("Pentad", "CardNo", "ObserverNo")
# 
# survey_groups <- Hadedas %>%
#   group_by(across(all_of(unique_fields))) %>%
#   summarise(n = n(), .groups = 'drop')
# 
# potential_duplicates <- survey_groups %>%
#   filter(n > 1)
# 
# print(potential_duplicates)
# 
# ##  ================================Data Rearrangement and Exclusion====================
# # rearrange the columns and exclude unnecessary ones:
# Hadedas <- Hadedas %>%
#   select(-c(StartDate, EndDate, IntervalDays))
# 
# Hadedas <- Hadedas[,c(1,2,7,8,3,9,4,5,6)]
# 
# ##  ================================Reporting Rates Investigation=======================
# # load the required package
# library(data.table)
# 
# # calculate the total reporting rate per pentad:
# Total_rr_tab <- Hadedas %>%
#   group_by(Pentad, Survey) %>%
#   summarise(total_visits = n(),
#             detections = sum(Spp > 0),
#             rr = mean(Spp > 0, na.rm = TRUE)) %>%
#   mutate(rr = round(rr, 2),
#          failures = total_visits - detections,
#          rr_percent = rr * 100,rr_bin = cut(rr_percent,
#                                             breaks = c(0, 20, 40, 60, 80, 100),
#                                             labels = c("0-20%", ">20-40%", ">40-60%", ">60-80%", ">80-100%"),
#                                             include.lowest = TRUE))
# 
# Total_rr_tab <- as.data.frame(Total_rr_tab)
# Total_rr_tab[is.na(Total_rr_tab)] <- 0# Replace NA with 0
# 
# # Compute Latitude and Longitude from Pentad
# Total_rr_tab$Latitude <- -(as.numeric(substr(Total_rr_tab$Pentad, 1, 2)) + (as.numeric(substr(Total_rr_tab$Pentad, 3, 4)) + 2.5) / 60)
# Total_rr_tab$Longitude <- (as.numeric(substr(Total_rr_tab$Pentad, 6, 7)) + (as.numeric(substr(Total_rr_tab$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Reporting Rate Visualization=========================
# #load the required libraries
# library(ggplot2)
# library(sf)
# library(RColorBrewer)
# 
# # Load map
# gpkg_path <- 'C:/Users/OYDHAL001/Desktop/RangeDynamics/rasterFiles/gadm41_ZAF.gpkg'
# SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1")  # State/Province level
# 
# # Visualization using ggplot2
# 
# ggplot() +
#   geom_tile(data = Total_rr_tab, aes(x = Longitude, y = Latitude, fill = rr_bin)) +
#   geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +
#   ggtitle("Reporting Rate (2012/13)") +
#   scale_fill_manual(name = "Report rate (%)",
#                     values = c(
#                       "0-20%" = "#ffff99",
#                       ">20-40%" = "#ffa366",
#                       ">40-60%" = "#FFC200",
#                       ">60-80%" = "#FF4500",
#                       ">80-100%" = "#4d4d00"),drop = FALSE) +
#   theme_bw() +theme(plot.title = element_text(hjust = 0.5),
#                     panel.background = element_rect(fill = "white"),panel.border = element_blank(),
#                     legend.key = element_rect(fill = "white"),legend.position = "right",
#                     axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
# 
# #save the Total_rr_tab data
# write.csv(Total_rr_tab, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedData/Total_rr_2012-13.csv")
# ##########################################################################################
# ##########################################################################################
# ##########################################################################################
# 
# 
# # Clear R environment
# rm(list = ls())
# getwd()
# setwd("C:/Users/OYDHAL001/Desktop/RangeDynamics")
# #ls()#list all the files in the environment
# #rm(list = c("p1", "p2","p3","p4","p5","p6","p7","p8","p9"))#remove specific files
# #file.rename("filename.r", "folder/filename.r")#move file to a folder
# 
# ##  ================================Data Preparation==================================
# #Load Required Libraries
# #To begin,  load the necessary R libraries:
# 
# library(ABAP)
# library(dplyr)
# 
# #Retrieve and Clean Data
# # retrieve the bird data for Hadedas Ibis in South Africa for the year 2008/09 and perform initial data cleaning:
# 
# Hadedas <- getAbapData(.spp = 84, 
#                        .region_type = "country", 
#                        .region = "SouthAfrica",
#                        .years = 2014:2015)
# 
# Hadedas <- select(Hadedas, -c(StartTime,Hour1,Hour2,Hour3,Hour4,Hour5,Hour6,Hour7,Hour8,Hour9,Hour10,
#                               TotalSpp,InclNight,AllHabitats,Common_name,Taxonomic_name))
# 
# Hadedas <- Hadedas %>%
#   mutate(Spp = ifelse(Spp == '84', 1, 
#                       ifelse(Spp == '-', 0, NA_integer_)))
# 
# sum(is.na(Hadedas))
# 
# Hadedas <- Hadedas %>%
#   as.data.frame() %>%
#   mutate(Sequence = ifelse(Sequence == '-', 0, as.numeric(Sequence))) %>%
#   rename(TotalSpecies = Sequence)
# 
# ##  ================================Filter Protocols==================================
# # filter out protocols that do not meet the standard requirements:
# 
# sum(Hadedas$TotalHours<2, na.rm = TRUE)
# Hadedas <- Hadedas[!(Hadedas$TotalHours < 2 | is.na(Hadedas$TotalHours)), ]
# 
# Hadedas$StartDate <- as.Date(Hadedas$StartDate, format = '%Y-%m-%d')
# Hadedas$EndDate <- as.Date(Hadedas$EndDate, format = '%Y-%m-%d')
# 
# Hadedas <- Hadedas %>%
#   mutate(IntervalDays = as.numeric(difftime(EndDate, StartDate, units = "days")))
# 
# Hadedas <- Hadedas[Hadedas$IntervalDays >= 0 & Hadedas$IntervalDays <= 5, ]
# 
# ##  ================================Geospatial Data Preparation==========================
# # create latitude and longitude columns for each pentad:
# 
# Hadedas$Latitude <- -(as.numeric(substr(Hadedas$Pentad, 1, 2)) + (as.numeric(substr(Hadedas$Pentad, 3, 4)) + 2.5) / 60)
# Hadedas$Longitude <- (as.numeric(substr(Hadedas$Pentad, 6, 7)) + (as.numeric(substr(Hadedas$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Survey Period Categorization=========================
# #Load Required Libraries
# library(lubridate)
# library(dplyr)
# 
# # categorize records into survey periods:
# Hadedas <- Hadedas %>%
#   mutate(Survey = case_when(
#     year(StartDate) == 2014 ~ "2014",
#     year(StartDate) == 2015 ~ "2015",
#     TRUE ~ "Other"
#   ))
# 
# ##  ================================Duplicate Check======================================
# # check for duplicates based on unique fields:
# unique_fields <- c("Pentad", "CardNo", "ObserverNo")
# 
# survey_groups <- Hadedas %>%
#   group_by(across(all_of(unique_fields))) %>%
#   summarise(n = n(), .groups = 'drop')
# 
# potential_duplicates <- survey_groups %>%
#   filter(n > 1)
# 
# print(potential_duplicates)
# 
# ##  ================================Data Rearrangement and Exclusion====================
# # rearrange the columns and exclude unnecessary ones:
# Hadedas <- Hadedas %>%
#   select(-c(StartDate, EndDate, IntervalDays))
# 
# Hadedas <- Hadedas[,c(1,2,7,8,3,9,4,5,6)]
# 
# ##  ================================Reporting Rates Investigation=======================
# # load the required package
# library(data.table)
# 
# # calculate the total reporting rate per pentad:
# Total_rr_tab <- Hadedas %>%
#   group_by(Pentad, Survey) %>%
#   summarise(total_visits = n(),
#             detections = sum(Spp > 0),
#             rr = mean(Spp > 0, na.rm = TRUE)) %>%
#   mutate(rr = round(rr, 2),
#          failures = total_visits - detections,
#          rr_percent = rr * 100,rr_bin = cut(rr_percent,
#                                             breaks = c(0, 20, 40, 60, 80, 100),
#                                             labels = c("0-20%", ">20-40%", ">40-60%", ">60-80%", ">80-100%"),
#                                             include.lowest = TRUE))
# 
# Total_rr_tab <- as.data.frame(Total_rr_tab)
# Total_rr_tab[is.na(Total_rr_tab)] <- 0# Replace NA with 0
# 
# # Compute Latitude and Longitude from Pentad
# Total_rr_tab$Latitude <- -(as.numeric(substr(Total_rr_tab$Pentad, 1, 2)) + (as.numeric(substr(Total_rr_tab$Pentad, 3, 4)) + 2.5) / 60)
# Total_rr_tab$Longitude <- (as.numeric(substr(Total_rr_tab$Pentad, 6, 7)) + (as.numeric(substr(Total_rr_tab$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Reporting Rate Visualization=========================
# #load the required libraries
# library(ggplot2)
# library(sf)
# library(RColorBrewer)
# 
# # Load map
# gpkg_path <- 'C:/Users/OYDHAL001/Desktop/RangeDynamics/rasterFiles/gadm41_ZAF.gpkg'
# SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1")  # State/Province level
# 
# # Visualization using ggplot2
# 
# ggplot() +
#   geom_tile(data = Total_rr_tab, aes(x = Longitude, y = Latitude, fill = rr_bin)) +
#   geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +
#   ggtitle("Reporting Rate (2014/15)") +
#   scale_fill_manual(name = "Report rate (%)",
#                     values = c(
#                       "0-20%" = "#ffff99",
#                       ">20-40%" = "#ffa366",
# #                       ">40-60%" = "#FFC200",
# #                       ">60-80%" = "#FF4500",
# #                       ">80-100%" = "#4d4d00"),drop = FALSE) +
# #   theme_bw() +theme(plot.title = element_text(hjust = 0.5),
# #                     panel.background = element_rect(fill = "white"),panel.border = element_blank(),
# #                     legend.key = element_rect(fill = "white"),legend.position = "right",
# #                     axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
# # 
# # #save the Total_rr_tab data
# # write.csv(Total_rr_tab, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedData/Total_rr_2014-15.csv")
# ##########################################################################################
# ##########################################################################################
# ##########################################################################################
# 
# 
# # Clear R environment
# rm(list = ls())
# getwd()
# setwd("C:/Users/OYDHAL001/Desktop/RangeDynamics")
# #ls()#list all the files in the environment
# #rm(list = c("p1", "p2","p3","p4","p5","p6","p7","p8","p9"))#remove specific files
# #file.rename("filename.r", "folder/filename.r")#move file to a folder
# 
# ##  ================================Data Preparation==================================
# #Load Required Libraries
# #To begin,  load the necessary R libraries:
# 
# library(ABAP)
# library(dplyr)
# 
# #Retrieve and Clean Data
# # retrieve the bird data for Hadedas Ibis in South Africa for the year 2008/09 and perform initial data cleaning:
# 
# Hadedas <- getAbapData(.spp = 84, 
#                        .region_type = "country", 
#                        .region = "SouthAfrica",
#                        .years = 2016:2017)
# 
# Hadedas <- select(Hadedas, -c(StartTime,Hour1,Hour2,Hour3,Hour4,Hour5,Hour6,Hour7,Hour8,Hour9,Hour10,
#                               TotalSpp,InclNight,AllHabitats,Common_name,Taxonomic_name))
# 
# Hadedas <- Hadedas %>%
#   mutate(Spp = ifelse(Spp == '84', 1, 
#                       ifelse(Spp == '-', 0, NA_integer_)))
# 
# sum(is.na(Hadedas))
# 
# Hadedas <- Hadedas %>%
#   as.data.frame() %>%
#   mutate(Sequence = ifelse(Sequence == '-', 0, as.numeric(Sequence))) %>%
#   rename(TotalSpecies = Sequence)
# 
# ##  ================================Filter Protocols==================================
# # filter out protocols that do not meet the standard requirements:
# 
# sum(Hadedas$TotalHours<2, na.rm = TRUE)
# Hadedas <- Hadedas[!(Hadedas$TotalHours < 2 | is.na(Hadedas$TotalHours)), ]
# 
# Hadedas$StartDate <- as.Date(Hadedas$StartDate, format = '%Y-%m-%d')
# Hadedas$EndDate <- as.Date(Hadedas$EndDate, format = '%Y-%m-%d')
# 
# Hadedas <- Hadedas %>%
#   mutate(IntervalDays = as.numeric(difftime(EndDate, StartDate, units = "days")))
# 
# Hadedas <- Hadedas[Hadedas$IntervalDays >= 0 & Hadedas$IntervalDays <= 5, ]
# 
# ##  ================================Geospatial Data Preparation==========================
# # create latitude and longitude columns for each pentad:
# 
# Hadedas$Latitude <- -(as.numeric(substr(Hadedas$Pentad, 1, 2)) + (as.numeric(substr(Hadedas$Pentad, 3, 4)) + 2.5) / 60)
# Hadedas$Longitude <- (as.numeric(substr(Hadedas$Pentad, 6, 7)) + (as.numeric(substr(Hadedas$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Survey Period Categorization=========================
# #Load Required Libraries
# library(lubridate)
# library(dplyr)
# 
# # categorize records into survey periods:
# Hadedas <- Hadedas %>%
#   mutate(Survey = case_when(
#     year(StartDate) == 2016 ~ "2016",
#     year(StartDate) == 2017 ~ "2017",
#     TRUE ~ "Other"
#   ))
# 
# ##  ================================Duplicate Check======================================
# # check for duplicates based on unique fields:
# unique_fields <- c("Pentad", "CardNo", "ObserverNo")
# 
# survey_groups <- Hadedas %>%
#   group_by(across(all_of(unique_fields))) %>%
#   summarise(n = n(), .groups = 'drop')
# 
# potential_duplicates <- survey_groups %>%
#   filter(n > 1)
# 
# print(potential_duplicates)
# 
# ##  ================================Data Rearrangement and Exclusion====================
# # rearrange the columns and exclude unnecessary ones:
# Hadedas <- Hadedas %>%
#   select(-c(StartDate, EndDate, IntervalDays))
# 
# Hadedas <- Hadedas[,c(1,2,7,8,3,9,4,5,6)]
# 
# ##  ================================Reporting Rates Investigation=======================
# # load the required package
# library(data.table)
# 
# # calculate the total reporting rate per pentad:
# Total_rr_tab <- Hadedas %>%
#   group_by(Pentad, Survey) %>%
#   summarise(total_visits = n(),
#             detections = sum(Spp > 0),
#             rr = mean(Spp > 0, na.rm = TRUE)) %>%
#   mutate(rr = round(rr, 2),
#          failures = total_visits - detections,
#          rr_percent = rr * 100,rr_bin = cut(rr_percent,
#                                             breaks = c(0, 20, 40, 60, 80, 100),
#                                             labels = c("0-20%", ">20-40%", ">40-60%", ">60-80%", ">80-100%"),
#                                             include.lowest = TRUE))
# 
# Total_rr_tab <- as.data.frame(Total_rr_tab)
# Total_rr_tab[is.na(Total_rr_tab)] <- 0# Replace NA with 0
# 
# # Compute Latitude and Longitude from Pentad
# Total_rr_tab$Latitude <- -(as.numeric(substr(Total_rr_tab$Pentad, 1, 2)) + (as.numeric(substr(Total_rr_tab$Pentad, 3, 4)) + 2.5) / 60)
# Total_rr_tab$Longitude <- (as.numeric(substr(Total_rr_tab$Pentad, 6, 7)) + (as.numeric(substr(Total_rr_tab$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Reporting Rate Visualization=========================
# #load the required libraries
# library(ggplot2)
# library(sf)
# library(RColorBrewer)
# 
# # Load map
# gpkg_path <- 'C:/Users/OYDHAL001/Desktop/RangeDynamics/rasterFiles/gadm41_ZAF.gpkg'
# SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1")  # State/Province level
# 
# # Visualization using ggplot2
# 
# ggplot() +
#   geom_tile(data = Total_rr_tab, aes(x = Longitude, y = Latitude, fill = rr_bin)) +
#   geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +
#   ggtitle("Reporting Rate (2016/17)") +
#   scale_fill_manual(name = "Report rate (%)",
#                     values = c(
#                       "0-20%" = "#ffff99",
#                       ">20-40%" = "#ffa366",
#                       ">40-60%" = "#FFC200",
#                       ">60-80%" = "#FF4500",
#                       ">80-100%" = "#4d4d00"),drop = FALSE) +
# #   theme_bw() +theme(plot.title = element_text(hjust = 0.5),
# #                     panel.background = element_rect(fill = "white"),panel.border = element_blank(),
# #                     legend.key = element_rect(fill = "white"),legend.position = "right",
# #                     axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
# # 
# # #save the Total_rr_tab data
# # write.csv(Total_rr_tab, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedData/Total_rr_2016-17.csv")
# ##########################################################################################
# ##########################################################################################
# ##########################################################################################
# 
# 
# # Clear R environment
# rm(list = ls())
# getwd()
# setwd("C:/Users/OYDHAL001/Desktop/RangeDynamics")
# #ls()#list all the files in the environment
# #rm(list = c("p1", "p2","p3","p4","p5","p6","p7","p8","p9"))#remove specific files
# #file.rename("filename.r", "folder/filename.r")#move file to a folder
# 
# ##  ================================Data Preparation==================================
# #Load Required Libraries
# #To begin,  load the necessary R libraries:
# 
# library(ABAP)
# library(dplyr)
# 
# #Retrieve and Clean Data
# # retrieve the bird data for Hadedas Ibis in South Africa for the year 2008/09 and perform initial data cleaning:
# 
# Hadedas <- getAbapData(.spp = 84, 
#                        .region_type = "country", 
#                        .region = "SouthAfrica",
#                        .years = 2018:2019)
# 
# Hadedas <- select(Hadedas, -c(StartTime,Hour1,Hour2,Hour3,Hour4,Hour5,Hour6,Hour7,Hour8,Hour9,Hour10,
#                               TotalSpp,InclNight,AllHabitats,Common_name,Taxonomic_name))
# 
# Hadedas <- Hadedas %>%
#   mutate(Spp = ifelse(Spp == '84', 1, 
#                       ifelse(Spp == '-', 0, NA_integer_)))
# 
# sum(is.na(Hadedas))
# 
# Hadedas <- Hadedas %>%
#   as.data.frame() %>%
#   mutate(Sequence = ifelse(Sequence == '-', 0, as.numeric(Sequence))) %>%
#   rename(TotalSpecies = Sequence)
# 
# ##  ================================Filter Protocols==================================
# # filter out protocols that do not meet the standard requirements:
# 
# sum(Hadedas$TotalHours<2, na.rm = TRUE)
# Hadedas <- Hadedas[!(Hadedas$TotalHours < 2 | is.na(Hadedas$TotalHours)), ]
# 
# Hadedas$StartDate <- as.Date(Hadedas$StartDate, format = '%Y-%m-%d')
# Hadedas$EndDate <- as.Date(Hadedas$EndDate, format = '%Y-%m-%d')
# 
# Hadedas <- Hadedas %>%
#   mutate(IntervalDays = as.numeric(difftime(EndDate, StartDate, units = "days")))
# 
# Hadedas <- Hadedas[Hadedas$IntervalDays >= 0 & Hadedas$IntervalDays <= 5, ]
# 
# ##  ================================Geospatial Data Preparation==========================
# # create latitude and longitude columns for each pentad:
# 
# Hadedas$Latitude <- -(as.numeric(substr(Hadedas$Pentad, 1, 2)) + (as.numeric(substr(Hadedas$Pentad, 3, 4)) + 2.5) / 60)
# Hadedas$Longitude <- (as.numeric(substr(Hadedas$Pentad, 6, 7)) + (as.numeric(substr(Hadedas$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Survey Period Categorization=========================
# #Load Required Libraries
# library(lubridate)
# library(dplyr)
# 
# # categorize records into survey periods:
# Hadedas <- Hadedas %>%
#   mutate(Survey = case_when(
#     year(StartDate) == 2018 ~ "2018",
#     year(StartDate) == 2019 ~ "2019",
#     TRUE ~ "Other"
#   ))
# 
# ##  ================================Duplicate Check======================================
# # check for duplicates based on unique fields:
# unique_fields <- c("Pentad", "CardNo", "ObserverNo")
# 
# survey_groups <- Hadedas %>%
#   group_by(across(all_of(unique_fields))) %>%
#   summarise(n = n(), .groups = 'drop')
# 
# potential_duplicates <- survey_groups %>%
#   filter(n > 1)
# 
# print(potential_duplicates)
# 
# ##  ================================Data Rearrangement and Exclusion====================
# # rearrange the columns and exclude unnecessary ones:
# Hadedas <- Hadedas %>%
#   select(-c(StartDate, EndDate, IntervalDays))
# 
# Hadedas <- Hadedas[,c(1,2,7,8,3,9,4,5,6)]
# 
# ##  ================================Reporting Rates Investigation=======================
# # load the required package
# library(data.table)
# 
# # calculate the total reporting rate per pentad:
# Total_rr_tab <- Hadedas %>%
#   group_by(Pentad, Survey) %>%
#   summarise(total_visits = n(),
#             detections = sum(Spp > 0),
#             rr = mean(Spp > 0, na.rm = TRUE)) %>%
#   mutate(rr = round(rr, 2),
#          failures = total_visits - detections,
#          rr_percent = rr * 100,rr_bin = cut(rr_percent,
#                                             breaks = c(0, 20, 40, 60, 80, 100),
#                                             labels = c("0-20%", ">20-40%", ">40-60%", ">60-80%", ">80-100%"),
#                                             include.lowest = TRUE))
# 
# Total_rr_tab <- as.data.frame(Total_rr_tab)
# Total_rr_tab[is.na(Total_rr_tab)] <- 0# Replace NA with 0
# 
# # Compute Latitude and Longitude from Pentad
# Total_rr_tab$Latitude <- -(as.numeric(substr(Total_rr_tab$Pentad, 1, 2)) + (as.numeric(substr(Total_rr_tab$Pentad, 3, 4)) + 2.5) / 60)
# Total_rr_tab$Longitude <- (as.numeric(substr(Total_rr_tab$Pentad, 6, 7)) + (as.numeric(substr(Total_rr_tab$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Reporting Rate Visualization=========================
# #load the required libraries
# library(ggplot2)
# library(sf)
# library(RColorBrewer)
# 
# # Load map
# gpkg_path <- 'C:/Users/OYDHAL001/Desktop/RangeDynamics/rasterFiles/gadm41_ZAF.gpkg'
# SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1")  # State/Province level
# 
# # Visualization using ggplot2
# 
# ggplot() +
#   geom_tile(data = Total_rr_tab, aes(x = Longitude, y = Latitude, fill = rr_bin)) +
#   geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +
#   ggtitle("Reporting Rate (2018/19)") +
#   scale_fill_manual(name = "Report rate (%)",
#                     values = c(
#                       "0-20%" = "#ffff99",
#                       ">20-40%" = "#ffa366",
#                       ">40-60%" = "#FFC200",
#                       ">60-80%" = "#FF4500",
#                       ">80-100%" = "#4d4d00"),drop = FALSE) +
#   theme_bw() +theme(plot.title = element_text(hjust = 0.5),
#                     panel.background = element_rect(fill = "white"),panel.border = element_blank(),
#                     legend.key = element_rect(fill = "white"),legend.position = "right",
#                     axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
# 
# #save the Total_rr_tab data
# write.csv(Total_rr_tab, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedData/Total_rr_2018-19.csv")
##########################################################################################
##########################################################################################
##########################################################################################

# 
# # Clear R environment
# rm(list = ls())
# getwd()
# setwd("C:/Users/OYDHAL001/Desktop/RangeDynamics")
# #ls()#list all the files in the environment
# #rm(list = c("p1", "p2","p3","p4","p5","p6","p7","p8","p9"))#remove specific files
# #file.rename("filename.r", "folder/filename.r")#move file to a folder
# 
# ##  ================================Data Preparation==================================
# #Load Required Libraries
# #To begin,  load the necessary R libraries:
# 
# library(ABAP)
# library(dplyr)
# 
# #Retrieve and Clean Data
# # retrieve the bird data for Hadedas Ibis in South Africa for the year 2008/09 and perform initial data cleaning:
# 
# Hadedas <- getAbapData(.spp = 84, 
#                        .region_type = "country", 
#                        .region = "SouthAfrica",
#                        .years = 2020:2021)
# 
# Hadedas <- select(Hadedas, -c(StartTime,Hour1,Hour2,Hour3,Hour4,Hour5,Hour6,Hour7,Hour8,Hour9,Hour10,
#                               TotalSpp,InclNight,AllHabitats,Common_name,Taxonomic_name))
# 
# Hadedas <- Hadedas %>%
#   mutate(Spp = ifelse(Spp == '84', 1, 
#                       ifelse(Spp == '-', 0, NA_integer_)))
# 
# sum(is.na(Hadedas))
# 
# Hadedas <- Hadedas %>%
#   as.data.frame() %>%
#   mutate(Sequence = ifelse(Sequence == '-', 0, as.numeric(Sequence))) %>%
#   rename(TotalSpecies = Sequence)
# 
# ##  ================================Filter Protocols==================================
# # filter out protocols that do not meet the standard requirements:
# 
# sum(Hadedas$TotalHours<2, na.rm = TRUE)
# Hadedas <- Hadedas[!(Hadedas$TotalHours < 2 | is.na(Hadedas$TotalHours)), ]
# 
# Hadedas$StartDate <- as.Date(Hadedas$StartDate, format = '%Y-%m-%d')
# Hadedas$EndDate <- as.Date(Hadedas$EndDate, format = '%Y-%m-%d')
# 
# Hadedas <- Hadedas %>%
#   mutate(IntervalDays = as.numeric(difftime(EndDate, StartDate, units = "days")))
# 
# Hadedas <- Hadedas[Hadedas$IntervalDays >= 0 & Hadedas$IntervalDays <= 5, ]
# 
# ##  ================================Geospatial Data Preparation==========================
# # create latitude and longitude columns for each pentad:
# 
# Hadedas$Latitude <- -(as.numeric(substr(Hadedas$Pentad, 1, 2)) + (as.numeric(substr(Hadedas$Pentad, 3, 4)) + 2.5) / 60)
# Hadedas$Longitude <- (as.numeric(substr(Hadedas$Pentad, 6, 7)) + (as.numeric(substr(Hadedas$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Survey Period Categorization=========================
# #Load Required Libraries
# library(lubridate)
# library(dplyr)
# 
# # categorize records into survey periods:
# Hadedas <- Hadedas %>%
#   mutate(Survey = case_when(
#     year(StartDate) == 2020 ~ "2020",
#     year(StartDate) == 2021 ~ "2021",
#     TRUE ~ "Other"
#   ))
# 
# ##  ================================Duplicate Check======================================
# # check for duplicates based on unique fields:
# unique_fields <- c("Pentad", "CardNo", "ObserverNo")
# 
# survey_groups <- Hadedas %>%
#   group_by(across(all_of(unique_fields))) %>%
#   summarise(n = n(), .groups = 'drop')
# 
# potential_duplicates <- survey_groups %>%
#   filter(n > 1)
# 
# print(potential_duplicates)
# 
# ##  ================================Data Rearrangement and Exclusion====================
# # rearrange the columns and exclude unnecessary ones:
# Hadedas <- Hadedas %>%
#   select(-c(StartDate, EndDate, IntervalDays))
# 
# Hadedas <- Hadedas[,c(1,2,7,8,3,9,4,5,6)]
# 
# ##  ================================Reporting Rates Investigation=======================
# # load the required package
# library(data.table)
# 
# # calculate the total reporting rate per pentad:
# Total_rr_tab <- Hadedas %>%
#   group_by(Pentad, Survey) %>%
#   summarise(total_visits = n(),
#             detections = sum(Spp > 0),
#             rr = mean(Spp > 0, na.rm = TRUE)) %>%
#   mutate(rr = round(rr, 2),
#          failures = total_visits - detections,
#          rr_percent = rr * 100,rr_bin = cut(rr_percent,
#                                             breaks = c(0, 20, 40, 60, 80, 100),
#                                             labels = c("0-20%", ">20-40%", ">40-60%", ">60-80%", ">80-100%"),
#                                             include.lowest = TRUE))
# 
# Total_rr_tab <- as.data.frame(Total_rr_tab)
# Total_rr_tab[is.na(Total_rr_tab)] <- 0# Replace NA with 0
# 
# # Compute Latitude and Longitude from Pentad
# Total_rr_tab$Latitude <- -(as.numeric(substr(Total_rr_tab$Pentad, 1, 2)) + (as.numeric(substr(Total_rr_tab$Pentad, 3, 4)) + 2.5) / 60)
# Total_rr_tab$Longitude <- (as.numeric(substr(Total_rr_tab$Pentad, 6, 7)) + (as.numeric(substr(Total_rr_tab$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Reporting Rate Visualization=========================
# #load the required libraries
# library(ggplot2)
# library(sf)
# library(RColorBrewer)
# 
# # Load map
# gpkg_path <- 'C:/Users/OYDHAL001/Desktop/RangeDynamics/rasterFiles/gadm41_ZAF.gpkg'
# SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1")  # State/Province level
# 
# # Visualization using ggplot2
# 
# ggplot() +
#   geom_tile(data = Total_rr_tab, aes(x = Longitude, y = Latitude, fill = rr_bin)) +
#   geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +
#   ggtitle("Reporting Rate (2020/21)") +
#   scale_fill_manual(name = "Report rate (%)",
#                     values = c(
#                       "0-20%" = "#ffff99",
#                       ">20-40%" = "#ffa366",
#                       ">40-60%" = "#FFC200",
#                       ">60-80%" = "#FF4500",
#                       ">80-100%" = "#4d4d00"),drop = FALSE) +
#   theme_bw() +theme(plot.title = element_text(hjust = 0.5),
#                     panel.background = element_rect(fill = "white"),panel.border = element_blank(),
#                     legend.key = element_rect(fill = "white"),legend.position = "right",
#                     axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
# 
# #save the Total_rr_tab data
# write.csv(Total_rr_tab, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedData/Total_rr_2020-21.csv")
# ##########################################################################################
# ##########################################################################################
# ##########################################################################################

# 
# # Clear R environment
# rm(list = ls())
# getwd()
# setwd("C:/Users/OYDHAL001/Desktop/RangeDynamics")
# #ls()#list all the files in the environment
# #rm(list = c("p1", "p2","p3","p4","p5","p6","p7","p8","p9"))#remove specific files
# #file.rename("filename.r", "folder/filename.r")#move file to a folder
# 
# ##  ================================Data Preparation==================================
# #Load Required Libraries
# #To begin,  load the necessary R libraries:
# 
# library(ABAP)
# library(dplyr)
# 
# #Retrieve and Clean Data
# # retrieve the bird data for Hadedas Ibis in South Africa for the year 2008/09 and perform initial data cleaning:
# 
# Hadedas <- getAbapData(.spp = 84, 
#                        .region_type = "country", 
#                        .region = "SouthAfrica",
#                        .years = 2022:2023)
# 
# Hadedas <- select(Hadedas, -c(StartTime,Hour1,Hour2,Hour3,Hour4,Hour5,Hour6,Hour7,Hour8,Hour9,Hour10,
#                               TotalSpp,InclNight,AllHabitats,Common_name,Taxonomic_name))
# 
# Hadedas <- Hadedas %>%
#   mutate(Spp = ifelse(Spp == '84', 1, 
#                       ifelse(Spp == '-', 0, NA_integer_)))
# 
# sum(is.na(Hadedas))
# 
# Hadedas <- Hadedas %>%
#   as.data.frame() %>%
#   mutate(Sequence = ifelse(Sequence == '-', 0, as.numeric(Sequence))) %>%
#   rename(TotalSpecies = Sequence)
# 
# ##  ================================Filter Protocols==================================
# # filter out protocols that do not meet the standard requirements:
# 
# sum(Hadedas$TotalHours<2, na.rm = TRUE)
# Hadedas <- Hadedas[!(Hadedas$TotalHours < 2 | is.na(Hadedas$TotalHours)), ]
# 
# Hadedas$StartDate <- as.Date(Hadedas$StartDate, format = '%Y-%m-%d')
# Hadedas$EndDate <- as.Date(Hadedas$EndDate, format = '%Y-%m-%d')
# 
# Hadedas <- Hadedas %>%
#   mutate(IntervalDays = as.numeric(difftime(EndDate, StartDate, units = "days")))
# 
# Hadedas <- Hadedas[Hadedas$IntervalDays >= 0 & Hadedas$IntervalDays <= 5, ]
# 
# ##  ================================Geospatial Data Preparation==========================
# # create latitude and longitude columns for each pentad:
# 
# Hadedas$Latitude <- -(as.numeric(substr(Hadedas$Pentad, 1, 2)) + (as.numeric(substr(Hadedas$Pentad, 3, 4)) + 2.5) / 60)
# Hadedas$Longitude <- (as.numeric(substr(Hadedas$Pentad, 6, 7)) + (as.numeric(substr(Hadedas$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Survey Period Categorization=========================
# #Load Required Libraries
# library(lubridate)
# library(dplyr)
# 
# # categorize records into survey periods:
# Hadedas <- Hadedas %>%
#   mutate(Survey = case_when(
#     year(StartDate) == 2022 ~ "2022",
#     year(StartDate) == 2023 ~ "2023",
#     TRUE ~ "Other"
#   ))
# 
# ##  ================================Duplicate Check======================================
# # check for duplicates based on unique fields:
# unique_fields <- c("Pentad", "CardNo", "ObserverNo")
# 
# survey_groups <- Hadedas %>%
#   group_by(across(all_of(unique_fields))) %>%
#   summarise(n = n(), .groups = 'drop')
# 
# potential_duplicates <- survey_groups %>%
#   filter(n > 1)
# 
# print(potential_duplicates)
# 
# ##  ================================Data Rearrangement and Exclusion====================
# # rearrange the columns and exclude unnecessary ones:
# Hadedas <- Hadedas %>%
#   select(-c(StartDate, EndDate, IntervalDays))
# 
# Hadedas <- Hadedas[,c(1,2,7,8,3,9,4,5,6)]
# 
# ##  ================================Reporting Rates Investigation=======================
# # load the required package
# library(data.table)
# 
# # calculate the total reporting rate per pentad:
# Total_rr_tab <- Hadedas %>%
#   group_by(Pentad, Survey) %>%
#   summarise(total_visits = n(),
#             detections = sum(Spp > 0),
#             rr = mean(Spp > 0, na.rm = TRUE)) %>%
#   mutate(rr = round(rr, 2),
#          failures = total_visits - detections,
#          rr_percent = rr * 100,rr_bin = cut(rr_percent,
#                                             breaks = c(0, 20, 40, 60, 80, 100),
#                                             labels = c("0-20%", ">20-40%", ">40-60%", ">60-80%", ">80-100%"),
#                                             include.lowest = TRUE))
# 
# Total_rr_tab <- as.data.frame(Total_rr_tab)
# Total_rr_tab[is.na(Total_rr_tab)] <- 0# Replace NA with 0
# 
# # Compute Latitude and Longitude from Pentad
# Total_rr_tab$Latitude <- -(as.numeric(substr(Total_rr_tab$Pentad, 1, 2)) + (as.numeric(substr(Total_rr_tab$Pentad, 3, 4)) + 2.5) / 60)
# Total_rr_tab$Longitude <- (as.numeric(substr(Total_rr_tab$Pentad, 6, 7)) + (as.numeric(substr(Total_rr_tab$Pentad, 8, 9)) + 2.5) / 60)
# 
# ##  ================================Reporting Rate Visualization=========================
# #load the required libraries
# library(ggplot2)
# library(sf)
# library(RColorBrewer)
# 
# # Load map
# gpkg_path <- 'C:/Users/OYDHAL001/Desktop/RangeDynamics/rasterFiles/gadm41_ZAF.gpkg'
# SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1")  # State/Province level
# 
# # Visualization using ggplot2
# 
# ggplot() +
#   geom_tile(data = Total_rr_tab, aes(x = Longitude, y = Latitude, fill = rr_bin)) +
#   geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +
#   ggtitle("Reporting Rate (2022/23)") +
#   scale_fill_manual(name = "Report rate (%)",
#                     values = c(
#                       "0-20%" = "#ffff99",
#                       ">20-40%" = "#ffa366",
#                       ">40-60%" = "#FFC200",
#                       ">60-80%" = "#FF4500",
#                       ">80-100%" = "#4d4d00"),drop = FALSE) +
#   theme_bw() +theme(plot.title = element_text(hjust = 0.5),
#                     panel.background = element_rect(fill = "white"),panel.border = element_blank(),
#                     legend.key = element_rect(fill = "white"),legend.position = "right",
#                     axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
# 
# #save the Total_rr_tab data
# write.csv(Total_rr_tab, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedData/Total_rr_2022-23.csv")
# ##########################################################################################
# ##########################################################################################
# ##########################################################################################
