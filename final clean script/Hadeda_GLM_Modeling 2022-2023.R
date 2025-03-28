# Title:    Single Season Occupancy Analysis (2008 - 2009)
# File:     Hadeda_GLM_Modeling.R
# Project:  Range Expansion Of Hadedas Ibis In South Africa (2008 - 2023)
# Author:   Oyede Haleemat

# ============================ Setup & Libraries ====================================

# Clear workspace
rm(list = ls())
setwd("C:/Users/OYDHAL001/Desktop/RangeDynamics")

# Load libraries
library(ABAP)
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(sf)
library(readr)
library(car)
library(corrplot)
library(raster)
library(sp)

# ============================ 1. Data Retrieval & Cleaning ==========================
library(ABAP)
library(dplyr)

Hadedas <- getAbapData(.spp = 84, 
                       .region_type = "country", 
                       .region = "SouthAfrica",
                       .years = 2022:2023) %>%
  dplyr::select(-c(StartTime, Hour1:Hour10, TotalSpp, InclNight, AllHabitats, Common_name, Taxonomic_name)) %>%
  mutate(
    Spp = ifelse(Spp == '84', 1, ifelse(Spp == '-', 0, NA_integer_)),
    Sequence = ifelse(Sequence == '-', 0, as.numeric(Sequence)),
    TotalSpecies = Sequence
  )

# Filter protocols
sum(Hadedas$TotalHours<2, na.rm = TRUE)
Hadedas <- Hadedas[!(Hadedas$TotalHours < 2 | is.na(Hadedas$TotalHours)), ]

Hadedas$StartDate <- as.Date(Hadedas$StartDate, format = '%Y-%m-%d')
Hadedas$EndDate <- as.Date(Hadedas$EndDate, format = '%Y-%m-%d')

Hadedas <- Hadedas %>%
  mutate(IntervalDays = as.numeric(difftime(EndDate, StartDate, units = "days")))

Hadedas <- Hadedas[Hadedas$IntervalDays >= 0 & Hadedas$IntervalDays <= 5, ]

# Add coordinates
Hadedas <- Hadedas %>%
  mutate(
    Latitude = -(as.numeric(substr(Pentad, 1, 2)) + (as.numeric(substr(Pentad, 3, 4)) + 2.5) / 60),
    Longitude =  (as.numeric(substr(Pentad, 6, 7)) + (as.numeric(substr(Pentad, 8, 9)) + 2.5) / 60)
  )

# Survey period categorization
library(lubridate)

Hadedas <- Hadedas %>%
  mutate(Survey = case_when(
    year(StartDate) == 2022 ~ "2022",
    year(StartDate) == 2023 ~ "2023",
    TRUE ~ "Other"
  ))

# Remove unused columns
library(dplyr)
Hadedas <- Hadedas %>% select(-StartDate, -EndDate, -IntervalDays)

# ============================ 2. Reporting Rates ====================================
library(dplyr)

Total_rr_tab <- Hadedas %>%
  group_by(Pentad, Survey) %>%
  summarise(
    total_visits = n(),
    detections = sum(Spp > 0),
    rr = round(mean(Spp > 0, na.rm = TRUE), 2),
    failures = total_visits - detections,
    rr_percent = rr * 100,
    rr_bin = cut(rr_percent, breaks = c(0, 20, 40, 60, 80, 100),
                 labels = c("0-20%", ">20-40%", ">40-60%", ">60-80%", ">80-100%"),
                 include.lowest = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Latitude = -(as.numeric(substr(Pentad, 1, 2)) + (as.numeric(substr(Pentad, 3, 4)) + 2.5) / 60),
    Longitude = (as.numeric(substr(Pentad, 6, 7)) + (as.numeric(substr(Pentad, 8, 9)) + 2.5) / 60)
  )

# ============================ 3a. Spatial Effort Bias Check Mapping ===========================
ggplot(Total_rr_tab, aes(x = Longitude, y = Latitude)) +
  geom_tile(aes(fill = total_visits)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Survey Effort per Pentad")


# ============================ 3b. Visualization ======================================
library(ggplot2)
library(sf)
library(raster)
library(sp)
# Load South Africa province boundaries
SA_province <- st_read('rasterFiles/gadm41_ZAF.gpkg', layer = "ADM_ADM_1")

# Plot reporting rate
ggplot() +
  geom_tile(data = Total_rr_tab, aes(x = Longitude, y = Latitude, fill = rr_bin)) +
  geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +
  scale_fill_manual(
    name = "Report rate (%)",
    values = c(
      "0-20%" = "#E5F5E0",
      ">20-40%" =  "#A1D99B",
      ">40-60%" = "#41AB5D",
      ">60-80%" = "#1B7837",
      ">80-100%" = "#00441B"
    ), drop = FALSE
  ) +
  theme_bw() +
  labs(title = "Hadeda Ibis Reporting Rate (2022-2023)") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

# ============================ 4. Merge Covariates ===================================
library(readr)
library(readxl)
library(dplyr)

siteSpecificCov <- read_csv("C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/siteSpecificCov2020_2023.csv") %>%
  rename(Pentad = pentad) %>% 
  dplyr::select(-1)

seasonalSiteCov <- read_csv("C:/Users/OYDHAL001/Desktop/RangeDynamics/processedCovariates/seasonalSiteCov2022_23.csv") %>%
  rename(Pentad = pentad)

covs <- merge(siteSpecificCov, seasonalSiteCov, by = "Pentad", all.x = TRUE)

reportRate <- Total_rr_tab[, c("Pentad", "Latitude", "Longitude", "Survey", "detections", "failures")]
reportRate <- merge(reportRate, covs, by = c("Pentad", "Survey"), all.x = TRUE) %>%
  dplyr::select(-province.x, -province.y)


# ============================ 5a. Covariate Outlier Check ============================

# Define covariates to check
covariate_vars <- c("mean_elevation", "mean_slope", "meanMaxTemp", "meanMinTemp",
                    "meanNDWI", "meanNDVI", "meanPrecp")

# Loop through each covariate
for (var in covariate_vars) {
  cat("\n---", var, "---\n")
  var_data <- reportRate[[var]]
  
  # Summary stats
  print(summary(var_data))
  
  # Identify IQR-based outliers
  Q1 <- quantile(var_data, 0.25, na.rm = TRUE)
  Q3 <- quantile(var_data, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  outliers <- var_data[var_data < lower | var_data > upper]
  
  cat("Outlier count:", length(outliers), "\n")
  
  # Plot
  par(mfrow = c(1, 3))
  hist(var_data, main = paste("Histogram:", var), xlab = var, col = "gray")
  boxplot(var_data, main = paste("Boxplot:", var), horizontal = TRUE)
  plot(density(var_data, na.rm = TRUE), main = paste("Density:", var))
}

# ============================ 5b. Data Transformation ================================
library(dplyr)
# Log-transform
reportRate <- reportRate %>%
  mutate(
    meanPrecp = log(meanPrecp + 1),
    mean_elevation = log(mean_elevation + 1),
    mean_slope = log(mean_slope + 1)
  )

# Landcover proportions
reportRate <- reportRate %>%
  mutate(
    wetland_prop = WetlandPct / 100,
    urban_prop = BuiltupPct / 100,
    others_prop = OthersPct / 100,
    grassland_prop = GrasslandPct / 100,
    forest_prop = TreecovPct / 100,
    cropland_prop = FarmlandPct / 100
  )

# Select and scale numeric variables
scale_vars <- c("mean_elevation", "mean_slope", "meanMaxTemp", "meanMinTemp", "meanNDWI", "meanNDVI", "meanPrecp")
scaledVar <- scale(reportRate[, scale_vars])
means <- attr(scaledVar, "scaled:center")
sds <- attr(scaledVar, "scaled:scale")

# Final reporting rate dataset
reportRate <- data.frame(
  reportRate[, c("Pentad", "Latitude", "Longitude", "Survey", "detections", "failures")],
  scaledVar,
  reportRate[, c("cropland_prop", "forest_prop", "grassland_prop", "others_prop", "urban_prop", "wetland_prop")]
)

# ============================ 6. Collinearity Diagnostics ===========================

# Define numeric predictors to check for collinearity
numeric_vars <- c("meanPrecp", "meanMaxTemp", "meanMinTemp",
                  "meanNDVI", "meanNDWI", "mean_slope", "mean_elevation",
                  "forest_prop", "cropland_prop", "grassland_prop",
                  "others_prop", "urban_prop", "wetland_prop")

# Compute Pearson correlation matrix
cor_matrix <- cor(reportRate[, numeric_vars], use = "complete.obs")

# Plot correlation matrix
corrplot::corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)
mtext("Correlation Matrix (2022/23)", side = 1, line = 4, cex = 1.2)

# ===================== 6b. VIF Check on Sample GLM ==========================
# Fit a linear model for VIF calculation using a subset of predictors
lm_model <- lm(detections ~ meanNDVI + meanNDWI + meanPrecp + mean_elevation + forest_prop,
               data = reportRate)

# Compute VIF scores
vif_scores <- car::vif(lm_model)
print(round(vif_scores, 2))

# ============================ 7. GLM Model Fitting ===========================


habitatMod <- glm(cbind(detections, failures) ~ forest_prop + grassland_prop + urban_prop,
                  data = reportRate, family = binomial)  # Habitat Preference

prodMod <- glm(cbind(detections, failures) ~ meanNDVI + meanNDWI + meanPrecp,
               data = reportRate, family = binomial)  # Productivity & Moisture

humanMod <- glm(cbind(detections, failures) ~ urban_prop + cropland_prop + forest_prop,
                data = reportRate, family = binomial)  # Human-modified Landscapes

climateMod <- glm(cbind(detections, failures) ~ meanMaxTemp + meanMinTemp + meanPrecp,
                  data = reportRate, family = binomial)  # Climatic Tolerance

nonlinearMod <- glm(cbind(detections, failures) ~ meanMinTemp + I(meanMinTemp^2),
                    data = reportRate, family = binomial)  # Non-linear Temperature

topoMod <- glm(cbind(detections, failures) ~ mean_elevation + mean_slope,
               data = reportRate, family = binomial)  # Topographic Preference

integratedMod <- glm(cbind(detections, failures) ~ meanNDVI + meanPrecp + urban_prop + forest_prop + mean_elevation,
                     data = reportRate, family = binomial)  # Integrated Ecological Model

# Interaction models
urbanNDVI <- glm(cbind(detections, failures) ~ urban_prop * meanNDVI,
                 data = reportRate, family = binomial)

forestElevation <- glm(cbind(detections, failures) ~ forest_prop * mean_elevation,
                       data = reportRate, family = binomial)

croplandNDWI <- glm(cbind(detections, failures) ~ cropland_prop * meanNDWI,
                    data = reportRate, family = binomial)

# Store models in a named list
models <- list(
  Habitat = habitatMod,
  Productivity = prodMod,
  HumanImpact = humanMod,
  Climate = climateMod,
  NonLinearTemp = nonlinearMod,
  Topography = topoMod,
  Integrated = integratedMod,
  Urban_NDVI = urbanNDVI,
  Forest_Elevation = forestElevation,
  Cropland_NDWI = croplandNDWI
)

# ============================ 8. Model Comparison (AIC) ===========================

# Extract AIC values for each model
aic_values <- sapply(models, AIC)

# Calculate ΔAIC relative to the best model
delta_aic <- aic_values - min(aic_values)

# Build comparison table
model_comparison <- data.frame(
  Model = names(aic_values),
  AIC = round(aic_values, 2),
  Delta_AIC = round(delta_aic, 2)
)

# Sort by AIC (best to worst)
model_comparison <- model_comparison[order(model_comparison$AIC), ]
print(model_comparison)

best_model <- model_comparison$Model[1]#BEST MODEL
cat("Best model based on AIC:", best_model)
summary(models[[best_model]])
summary(humanMod)

# ============================ 9. Fitted Regression Line Plots (L-Shaped, Auto Y-Limits) ===========================

# Setup: 1 row, 3 plots with room for shared caption
par(mfrow = c(1, 3), oma = c(5, 0, 0, 0))  # bottom outer margin = 5 lines

# Plot 1: Urban Proportion
urban_seq <- seq(0, 1, length.out = 100)
urban_data <- data.frame(
  urban_prop = urban_seq,
  cropland_prop = mean(reportRate$cropland_prop, na.rm = TRUE),
  forest_prop = mean(reportRate$forest_prop, na.rm = TRUE)
)
urban_data$fit <- predict(models$HumanImpact, newdata = urban_data, type = "response")
urban_ylim <- range(urban_data$fit)

plot(urban_seq, urban_data$fit, type = "l", lwd = 2, col = "black",
     xlab = "Urban Cover (proportion)", ylab = "Predicted Probability",
     main = "Effect of Urban Cover", ylim = urban_ylim, bty = "l")

# Plot 2: Cropland Proportion
crop_seq <- seq(0, 1, length.out = 100)
crop_data <- data.frame(
  urban_prop = mean(reportRate$urban_prop, na.rm = TRUE),
  cropland_prop = crop_seq,
  forest_prop = mean(reportRate$forest_prop, na.rm = TRUE)
)
crop_data$fit <- predict(models$HumanImpact, newdata = crop_data, type = "response")
crop_ylim <- range(crop_data$fit)

plot(crop_seq, crop_data$fit, type = "l", lwd = 2, col = "black",
     xlab = "Cropland Cover (proportion)", ylab = "Predicted Probability",
     main = "Effect of Cropland", ylim = crop_ylim, bty = "l")

# Plot 3: Forest Proportion
forest_seq <- seq(0, 1, length.out = 100)
forest_data <- data.frame(
  urban_prop = mean(reportRate$urban_prop, na.rm = TRUE),
  cropland_prop = mean(reportRate$cropland_prop, na.rm = TRUE),
  forest_prop = forest_seq
)
forest_data$fit <- predict(models$HumanImpact, newdata = forest_data, type = "response")
forest_ylim <- range(forest_data$fit)

plot(forest_seq, forest_data$fit, type = "l", lwd = 2, col = "black",
     xlab = "Forest Cover (proportion)", ylab = "Predicted Probability",
     main = "Effect of Tree Cover", ylim = forest_ylim, bty = "l")

# Caption: Add year info under all three plots
mtext("Survey Years: 2022–2023", side = 1, line = 2, outer = TRUE, cex = 1)

# Reset layout
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

# ============================ 10. Prediction Mapping ===========================

# Extract land cover columns from full covariate grid
prediction_data <- covs[, c("Pentad", "BuiltupPct", "FarmlandPct", "TreecovPct")]

# Create required proportion variables
prediction_data <- prediction_data %>%
  mutate(
    urban_prop = BuiltupPct / 100,
    cropland_prop = FarmlandPct / 100,
    forest_prop = TreecovPct / 100
  ) %>%
  dplyr::select(Pentad, urban_prop, cropland_prop, forest_prop)

# Replace any missing values with 0
prediction_data[is.na(prediction_data)] <- 0

# Predict probabilities using the best model (HumanImpact)
prediction_data$occupancy_prob <- predict(models$HumanImpact,
                                          newdata = prediction_data,
                                          type = "response")
# Convert Pentad code into Lat-Long for plotting
prediction_data$Latitude <- -(as.numeric(substr(prediction_data$Pentad, 1, 2)) +
                                (as.numeric(substr(prediction_data$Pentad, 3, 4)) + 2.5) / 60)

prediction_data$Longitude <- (as.numeric(substr(prediction_data$Pentad, 6, 7)) +
                                (as.numeric(substr(prediction_data$Pentad, 8, 9)) + 2.5) / 60)

# ============================ 10. Model Visualization ===========================

library(ggplot2)
library(sf)

# Load and transform province boundaries
gpkg_path <- "C:/Users/OYDHAL001/Desktop/RangeDynamics/rasterFiles/gadm41_ZAF.gpkg"
SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1") |>
  st_transform(crs = 4326)

# Define custom color palette
custom_colors <- c("#fbf9fc", "#FCFAF9", "#F3CF9F", "#26F111", "#1F3716")

# Bin model predictions into 20 equal-width intervals
prediction_data$prob_bin <- as.numeric(cut(prediction_data$occupancy_prob, breaks = 20))

# Plot detection probability as percentages
ggplot() +
  geom_tile(data = prediction_data,
            aes(x = Longitude, y = Latitude, fill = prob_bin / 20)) +
  
  geom_sf(data = SA_province, fill = NA, color = "black", size = 0.6) +
  
  scale_fill_gradientn(
    name = "Detection Probability (%)",
    colors = custom_colors,
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    labels = scales::percent_format(accuracy = 1),  # ← this converts 0.1 → "10%"
    na.value = "transparent"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.key.size = unit(1.2, "cm")
  )

# ============================ 11. model verification ===========================

library(pROC)
reportRate$predicted <- predict(humanMod, newdata = reportRate, type = "response")
reportRate$observed <- ifelse(reportRate$detections > 0, 1, 0)
roc_obj <- roc(reportRate$observed, reportRate$predicted)
auc_val <- auc(roc_obj)
print(auc_val)
plot(roc_obj, col = "black", lwd = 3,
     main = "ROC Curve for GLM: Detection vs. Non-detection")
abline(a = 0, b = 1, lty = 2, col = "gray")
