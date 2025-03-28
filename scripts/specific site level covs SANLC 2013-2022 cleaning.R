getwd()
library(readxl)
land_cover_2022 <- read_excel("rawData/calculated lc 2022.xlsx")
land_cover_2022[4:8] <- round(land_cover_2022[4:8], 2) 
siteSpecificCov2022 <- land_cover_2022[,c(1, 4:8)]
head(siteSpecificCov2022)
write.csv(siteSpecificCov2022, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/siteSpecificCov2022.csv", row.names = FALSE)


land_cover_2018 <- read_excel("rawData/land cover 2018.xlsx")
land_cover_2018$totalArea <- rowSums(land_cover_2018[,c(3:7)])
library(dplyr)
land_cover_2018 <- land_cover_2018 %>% 
  mutate(Forestpct = round((Forest / totalArea) * 100, 2),
         Grasslandpct = round((Grassland / totalArea) * 100, 2),
         Wetlandpct = round((Wetland / totalArea) * 100, 2),
         Croplandpct = round((Cultivated / totalArea) * 100, 2),
         Urbanpct = round((Urban / totalArea) * 100, 2))

siteSpecificCov2018 <- land_cover_2018[,c(1, 9:13)]
head(siteSpecificCov2018)
write.csv(siteSpecificCov2018, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/siteSpecificCov2018.csv", row.names = FALSE)


land_cover_2013 <- read_excel("rawData/New land cover 2013-2014.xlsx")
land_cover_2013$totalArea <- rowSums(land_cover_2013[,c(4:8)])
land_cover_2013 <- land_cover_2013 %>% 
  mutate(Forestpct = round((Forest / totalArea) * 100, 2),
         Grasslandpct = round((Grassland / totalArea) * 100, 2),
         Wetlandpct = round((Wetland / totalArea) * 100, 2),
         Croplandpct = round((Cultivated / totalArea) * 100, 2),
         Urbanpct = round((Urban / totalArea) * 100, 2))

siteSpecificCov2013_2014 <- land_cover_2013[,c(1, 10:14)]
head(siteSpecificCov2013_2014)
write.csv(siteSpecificCov2013_2014, "C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/siteSpecificCov2013_2014.csv", row.names = FALSE)