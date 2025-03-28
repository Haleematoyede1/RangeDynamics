rm(list = ls())

#cleaning of the first batch of covariates 
#load the seasonal site covariates 
#2020-2023
#Maximum temperature
library(dplyr)
library(readxl)
maxTemp2020 <- read_excel("Annual_max_trmp_2020.xlsx")
maxTemp2020 <- maxTemp2020 %>%
  rename(meanMaxTemp = maxTemp_mean)

maxTemp2021 <- read_excel("Annual_max_trmp_2021.xlsx")
maxTemp2021 <- maxTemp2021 %>%
  rename(meanMaxTemp = maxTemp_mean)

maxTemp2022 <- read_excel("Annual_max_trmp_2022.xlsx")
maxTemp2022 <- maxTemp2022 %>%
  rename(meanMaxTemp =`_mean`)

maxTemp2023 <- read_excel("Annual_max_trmp_2023.xlsx")
maxTemp2023 <- maxTemp2023 %>%
  rename(meanMaxTemp = `_mean`)

head(maxTemp2020)
#minimum temperature
minTemp2020 <- read_excel("annual_mean_min_temp_2020.xlsx")
minTemp2020 <- minTemp2020 %>%
  rename(meanMinTemp =minTemp_mean)

minTemp2021 <- read_excel("annual_mean_min_temp_2021.xlsx")
minTemp2021 <- minTemp2021 %>%
  rename(meanMinTemp =minTemp_mean)

minTemp2022 <- read_excel("annual_mean_min_temp_2022.xlsx")
minTemp2022 <- minTemp2022 %>%
  rename(meanMinTemp = `_mean`)

minTemp2023 <- read_excel("annual_mean_min_temp_2023.xlsx")
minTemp2023 <- minTemp2023 %>%
  rename(meanMinTemp = MinTempmin)

#Average preciptation
precp2020 <- read_excel("Total_precp_2020.xlsx")
precp2020 <- precp2020 %>%
  rename(meanPrecp = totalprecp_sum)

precp2021 <- read_excel("Total_precp_2021.xlsx")
precp2021 <- precp2021 %>%
  rename(meanPrecp = totalPrecp_sum)

precp2022 <- read_excel("Total_precp_2022.xlsx")
precp2022 <- precp2022 %>%
  rename(meanPrecp = `_sum`)

precp2023 <- read_excel("Total_precp_2023.xlsx")
precp2023 <- precp2023 %>%
  rename(meanPrecp = `_sum`)


#mean ndvi
ndvi2020 <- read_excel("annual_mean_ndvi_2020.xlsx")
ndvi2020 <- ndvi2020 %>%
  rename(meanNDVI = mean)

ndvi2021 <- read_excel("annual_mean_ndvi_2021.xlsx")
ndvi2021 <- ndvi2021 %>%
  rename(meanNDVI = mean)

ndvi2022 <- read_excel("annual_mean_ndvi_2022.xlsx")
ndvi2022 <- ndvi2022 %>%
  rename(meanNDVI = mean)

ndvi2023 <- read_excel("annual_mean_ndvi_2023.xlsx")
ndvi2023 <- ndvi2023 %>%
  rename(meanNDVI = mean)

#mean ndwi
ndwi2020 <- read_excel("annual_mean_ndwi_2020.xlsx")
ndwi2020 <- ndwi2020 %>%
  rename(meanNDWI = mean)

ndwi2021 <- read_excel("annual_mean_ndwi_2021.xlsx")
ndwi2021 <- ndwi2021 %>%
  rename(meanNDWI = mean)

ndwi2022 <- read_excel("annual_mean_ndwi_2022.xlsx")
ndwi2022 <- ndwi2022 %>%
  rename(meanNDWI = mean)

ndwi2023 <- read_excel("annual_mean_ndwi_2023.xlsx")
ndwi2023 <- ndwi2023 %>%
  rename(meanNDWI = mean)

#merge the covariates to create the seasonal site  covariates
#this is computed for each year
#merge min and max temp together by pentad and province  which are the same
# maxTemp2020, minTemp2020, ndwi2020, ndvi2020, precp2020
seasonalSiteCov2020 <- merge(maxTemp2020, minTemp2020, by = c("pentad", "province"))
seasonalSiteCov2020 <- merge(seasonalSiteCov2020, ndwi2020, by = c("pentad", "province"))
seasonalSiteCov2020 <- merge(seasonalSiteCov2020, ndvi2020, by = c("pentad", "province"))
seasonalSiteCov2020 <- merge(seasonalSiteCov2020, precp2020, by = c("pentad", "province"))
#summary(seasonalSiteCov2020)
#save  the seasonal site covariates as csv
write.csv(seasonalSiteCov2020, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/seasonalSiteCov2020.csv")

seasonalSiteCov2021 <- merge(maxTemp2021, minTemp2021, by = c("pentad", "province"))
seasonalSiteCov2021 <- merge(seasonalSiteCov2021, ndwi2021, by = c("pentad", "province"))
seasonalSiteCov2021 <- merge(seasonalSiteCov2021, ndvi2021, by = c("pentad", "province"))
seasonalSiteCov2021 <- merge(seasonalSiteCov2021, precp2021, by = c("pentad", "province"))
#summary(seasonalSiteCov2021)
write.csv(seasonalSiteCov2021, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/seasonalSiteCov2021.csv")

seasonalSiteCov2022 <- merge(maxTemp2022, minTemp2022, by = c("pentad", "province"))
seasonalSiteCov2022 <- merge(seasonalSiteCov2022, ndwi2022, by = c("pentad", "province"))
seasonalSiteCov2022 <- merge(seasonalSiteCov2022, ndvi2022, by = c("pentad", "province"))
seasonalSiteCov2022 <- merge(seasonalSiteCov2022, precp2022, by = c("pentad", "province"))
#summary(seasonalSiteCov2022)
write.csv(seasonalSiteCov2022, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/seasonalSiteCov2022.csv")

seasonalSiteCov2023 <- merge(maxTemp2023, minTemp2023, by = c("pentad", "province"))
seasonalSiteCov2023 <- merge(seasonalSiteCov2023, ndwi2023, by = c("pentad", "province"))
seasonalSiteCov2023 <- merge(seasonalSiteCov2023, ndvi2023, by = c("pentad", "province"))
seasonalSiteCov2023 <- merge(seasonalSiteCov2023, precp2023, by = c("pentad", "province"))
#summary(seasonalSiteCov2023)
write.csv(seasonalSiteCov2023, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/seasonalSiteCov2023.csv")


#load the  site specific covariates
#2008-2011
#Elevation
library(readxl)
Elev_Slope <- read_excel("HadedaIbis_Elevation_Slope_Analysis (1).xlsx")
head(Elev_Slope)

#Landcover
LC2020 <-  read_excel("C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/LULC_2022_CLASS_PERCENTAGES.xlsx")
head(LC2020)

#merge the site specific covariates to create the site specific covariates
#this is computed in batches of 4 years
#merge elevation and landcover together by pentad and province  which are the same
#Elev_Slope, LC2020
siteSpecificCov2020_2023 <- merge(Elev_Slope, LC2020, by = c("pentad"))
#summary(siteSpecificCov2020_2023)

# round colums to 2 decimal places
# List of column names to format
columns_to_format <- c("mean_elevation", "mean_slope")

# Use lapply to round the specified columns to 2 decimal places
siteSpecificCov2020_2023[columns_to_format] <- lapply(siteSpecificCov2020_2023[columns_to_format], round, digits = 2)
head(siteSpecificCov2020_2023)

#save  the site specific covariates as csv
write.csv(siteSpecificCov2020_2023, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/siteSpecificCov2020_2023.csv")
