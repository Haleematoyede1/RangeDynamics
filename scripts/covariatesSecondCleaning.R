# Title:    Single season  Occupancy Analysis (2010 - 2011)
# File:     01_01_SSOCM.R
# Project:  Range Expansion Of Hadedas Ibis In South Africa 2010 - 2023
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

##  ================================Covariate Data Preparation==================================
# Load the data
library(readr)
seasonalSiteCov2022 <- read_csv("processedCovariates/seasonalSiteCov2022.csv")
seasonalSiteCov2023 <- read_csv("processedCovariates/seasonalSiteCov2023.csv")
head(seasonalSiteCov2022)
head(seasonalSiteCov2023)

library(dplyr)
seasonalSiteCov2022 <- seasonalSiteCov2022 %>%
  mutate(Survey = 2022)

seasonalSiteCov2023 <- seasonalSiteCov2023 %>%
  mutate(Survey = 2023)

seasonalSiteCov2022_23 <- bind_rows(seasonalSiteCov2022, seasonalSiteCov2023)
#save the data
write.csv(seasonalSiteCov2022_23, "processedCovariates/seasonalSiteCov2022_23.csv", row.names = FALSE)
