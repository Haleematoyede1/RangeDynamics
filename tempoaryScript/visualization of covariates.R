##  ================================Covariates visualizations===================================


seasonalSiteCov2008 <- read.csv("C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/seasonalSiteCov2008.csv") %>%
  rename(Pentad = pentad)

sitecov <- seasonalSiteCov2008[,c("Pentad","meanMaxTemp","meanMinTemp","meanPrecp","meanNDVI","meanNDWI")]
# Compute Latitude and Longitude from Pentad
sitecov$Latitude <- -(as.numeric(substr(sitecov$Pentad, 1, 2)) + (as.numeric(substr(sitecov$Pentad, 3, 4)) + 2.5) / 60)
sitecov$Longitude <- (as.numeric(substr(sitecov$Pentad, 6, 7)) + (as.numeric(substr(sitecov$Pentad, 8, 9)) + 2.5) / 60)
head(sitecov)

library(raster)
library(sp)

df_precp <- sitecov
coordinates(df_precp) <- ~Longitude + Latitude
proj4string(df_precp) <- CRS("+proj=longlat +datum=WGS84")
r_extent <- extent(df_precp)
r <- raster(r_extent, res = 0.5)
r_precipitation <- rasterize(df_precp, r, field = "meanPrecp", fun = mean)
plot(r_precipitation, main = "Mean Precipitation (2008)")

r_ndvi <- rasterize(df_precp, r, field = "meanNDVI", fun = mean)
plot(r_ndvi, main = "Mean NDVI (2008)")

r_ndwi <- rasterize(df_precp, r, field = "meanNDWI", fun = mean)
plot(r_ndwi, main = "Mean NDWI (2008)")

r_maxtemp <- rasterize(df_precp, r, field = "meanMaxTemp", fun = mean)
plot(r_maxtemp, main = "Mean Max Temp (2008)")

r_mintemp <- rasterize(df_precp, r, field = "meanMinTemp", fun = mean)
plot(r_mintemp, main = "Mean Min Temp (2008)")

siteSpecificCov2008_2009 <- read.csv("C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/siteSpecificCov2008_2009.csv") %>%
  rename(Pentad = pentad)
seanonSite <- siteSpecificCov2008_2009[,c("Pentad","mean_elevation", "mean_slope", 
                                          "perc_cropland", "perc_forest", "perc_grassland", 
                                          "perc_others", "perc_urban", "perc_wetland")]
seanonSite$Latitude <- -(as.numeric(substr(seanonSite$Pentad, 1, 2)) + (as.numeric(substr(seanonSite$Pentad, 3, 4)) + 2.5) / 60)
seanonSite$Longitude <- (as.numeric(substr(seanonSite$Pentad, 6, 7)) + (as.numeric(substr(seanonSite$Pentad, 8, 9)) + 2.5) / 60)

df_seanonSit <- seanonSite
coordinates(df_seanonSit) <- ~Longitude + Latitude
proj4string(df_seanonSit) <- CRS("+proj=longlat +datum=WGS84")
r_extent <- extent(df_seanonSit)
r <- raster(r_extent, res = 0.5)
r_precipitation <- rasterize(df_seanonSit, r, field = "mean_elevation", fun = mean)
plot(r_precipitation, main = "Mean Elevation (2008)")

r_slope <- rasterize(df_seanonSit, r, field = "mean_slope", fun = mean)
plot(r_slope, main = "Mean Slope (2008)")

r_cropland <- rasterize(df_seanonSit, r, field = "perc_cropland", fun = mean)
plot(r_cropland, main = "Percentage Cropland (2008)")

r_forest <- rasterize(df_seanonSit, r, field = "perc_forest", fun = mean)
plot(r_forest, main = "Percentage Forest (2008)")

r_grassland <- rasterize(df_seanonSit, r, field = "perc_grassland", fun = mean)
plot(r_grassland, main = "Percentage Grassland (2008)")

r_others <- rasterize(df_seanonSit, r, field = "perc_others", fun = mean)
plot(r_others, main = "Percentage Others (2008)")


r_urban <- rasterize(df_seanonSit, r, field = "perc_urban", fun = mean)
plot(r_urban, main = "Percentage Urban (2008)")

r_wetland <- rasterize(df_seanonSit, r, field = "perc_wetland", fun = mean)
plot(r_wetland, main = "Percentage Wetland (2008)")
