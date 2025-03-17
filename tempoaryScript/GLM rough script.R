# GLM Analysis for 2007-2008

# Load required libraries
library(readr)
library(dplyr)
library(DescTools)


# Load and prepare environmental data
Env_2007 <- read_csv("updated env covs/cleaned_Env_2007.csv")
head(Env_2007)

# Load and prepare bird detection data
Bird7_8 <- read_csv("cleanedAbap-data sets- 2007-2023/cleanData_bird7_8.csv")
head(Bird7_8)

Bird7_8 <- Bird7_8 %>%
  select(-c(1,8)) %>%  # Exclude the unwanted column
  na.omit()  # Remove rows with missing values

# get the environmental data for each pentad
Bird7_8_merged <- Bird7_8 %>%
  left_join(
    Env_2007 %>% select(Pentad, Elev, Precip, Soil_Mois, Min_Temp, Max_Temp, Wind_Speed, NDVI, MNDWI),
    by = "Pentad"
  )
head(Bird7_8_merged)


#lets see how elevation affects the proportion of hadedas detected
par(mfrow = c(1, 2))

plot(Spp ~ Elev, data = Bird7_8_merged, las = 1,
     xlab = "Elev", ylab = "Hadedas",
     cex.lab = 1.5, cex.axis = 1.5, yaxt = "n")
axis(2, at = c(0, 1), cex.axis = 1.5, las = 1)

plot(jitter(Spp) ~ jitter(Elev), data = Bird7_8_merged,
     las = 1, xlab = "Slope", pch = 20,
     col = "firebrick3", ylab = "lawn grass",
     cex.lab = 1.5, cex.axis = 1.5, yaxt = "n")
axis(2, at = c(0, 1), cex.axis = 1.5, las = 1)
# Calculate reporting rate per pentad
rep_rate_7_8 <- Bird7_8 %>%
  group_by(Pentad) %>%
  summarise(
    total_visits = n(),
    detections = sum(Spp > 0),
    Rr = mean(Spp > 0, na.rm = TRUE)
  ) %>%
  mutate(Rr = round(Rr, 1))

# Create successes and failures from the reporting rate
rep_rate_7_8$failures <- rep_rate_7_8$total_visits - rep_rate_7_8$detections

# Extract environmental covariates into the reporting rates data
rep_rate_7_8 <- rep_rate_7_8 %>%
  left_join(
    Env_2007 %>% select(Pentad, Elev, Precip, Soil_Mois, Min_Temp, Max_Temp, Wind_Speed, NDVI, MNDWI),
    by = "Pentad"
  )

head(rep_rate_7_8)

# Scale variables
scaled <- c('Elev', 'Precip', 'MNDWI', 'Soil_Mois', 'Min_Temp', 'NDVI', 'Max_Temp', 'Wind_Speed')

scaledData <- rep_rate_7_8 %>%
  select(all_of(scaled)) %>% 
  scale(center = TRUE, scale = TRUE)

# Store original means and standard deviations for back-transformation
origMeans <- attr(scaledData, "scaled:center")
origStds <- attr(scaledData, "scaled:scale")

rep_rate_7_8[scaled] <- scaledData  # Replace existing data with the new scaled data

# Check correlation between variables
library(corrplot)
library(AICcmodavg)

numeric_vars <- rep_rate_7_8[, sapply(rep_rate_7_8, is.numeric)]
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black", tl.srt = 45)

# Fit general linear models
mod1 <- glm(cbind(detections, failures) ~ Precip + Max_Temp + Min_Temp + Wind_Speed, data = rep_rate_7_8, family = binomial)
mod2 <- glm(cbind(detections, failures) ~ Elev + Precip + NDVI + Max_Temp, data = rep_rate_7_8, family = binomial)
mod3 <- glm(cbind(detections, failures) ~ Min_Temp + Soil_Mois + MNDWI + Wind_Speed, data = rep_rate_7_8, family = binomial)
mod4 <- glm(cbind(detections, failures) ~ Elev + Precip + MNDWI + Max_Temp, data = rep_rate_7_8, family = binomial)
mod5 <- glm(cbind(detections, failures) ~ NDVI + Soil_Mois + Elev + Wind_Speed, data = rep_rate_7_8, family = binomial)
mod6 <- glm(cbind(detections, failures) ~ Elev + Wind_Speed + Soil_Mois + Max_Temp, data = rep_rate_7_8, family = binomial)
mod7 <- glm(cbind(detections, failures) ~ MNDWI + Precip + Max_Temp + Elev, data = rep_rate_7_8, family = binomial)
mod8 <- glm(cbind(detections, failures) ~ Elev + Precip + NDVI, data = rep_rate_7_8, family = binomial)
mod9 <- glm(cbind(detections, failures) ~ Soil_Mois + NDVI + Min_Temp + Wind_Speed, data = rep_rate_7_8, family = binomial)

# Compare AIC values
models <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9)
model.names <- c('mod1', 'mod2', 'mod3', 'mod4', 'mod5', 'mod6', 'mod7', 'mod8', 'mod9')
aictab(cand.set = models, modnames = model.names)

# Plotting function for fitted regression lines
plot_predictor <- function(model, data, predictor, ylim = c(0, 1), main = NULL) {
  pred_seq <- seq(min(data[[predictor]]), max(data[[predictor]]), length.out = 100)
  new_data <- data.frame(setNames(list(pred_seq), predictor))
  new_data[setdiff(names(data), predictor)] <- lapply(data[setdiff(names(data), predictor)], mean)
  
  predictions <- predict(model, newdata = new_data, type = "response")
  
  plot(pred_seq, predictions, type = 'l', axes = FALSE, 
       xlab = predictor, ylab = 'Predicted Response', 
       ylim = ylim, lwd = 2, cex.lab = 1.5)
  
  x_ticks <- quantile(pred_seq, c(0, 0.5, 1))
  axis(1, at = x_ticks, labels = round(x_ticks, 1), lwd = 2, cex.axis = 1.5)
  
  y_ticks <- seq(0, 1, 0.2)
  axis(2, at = y_ticks, labels = round(y_ticks, 1), las = 1, lwd = 2, cex.axis = 1.5)
  
  if (is.null(main)) main <- paste("Fitted Regression Line for", predictor)
  title(main = main, col.main = "black", font.main = 2)
}

# Create regression plots
pdf("Regression_Plots_GLM_2007_2008.pdf", width = 10, height = 8)
op <- par(mfrow = c(2, 2), mar = c(4, 1, 1, 2), oma = c(1, 5, 1, 0))

plot_predictor(mod2, rep_rate_7_8, "Elev")
plot_predictor(mod2, rep_rate_7_8, "Precip")
plot_predictor(mod2, rep_rate_7_8, "NDVI")
plot_predictor(mod2, rep_rate_7_8, "Max_Temp")

mtext("Fitted Regression Lines for Species 2007-2008 (GLM)", outer = TRUE, side = 1, line = 0, cex = 1.2, font = 2)

par(op)
dev.off()

cat("Regression plots have been saved to 'Regression_Plots_GLM_2007_2008.pdf'\n")

# Prepare data for prediction
Env_2007_scaled <- Env_2007

for(var in scaled) {
  Env_2007_scaled[[var]] <- (Env_2007[[var]] - origMeans[var]) / origStds[var]
}

# Make predictions
Env_2007_scaled$predicted_occupancy <- predict(mod2, newdata = Env_2007_scaled, type = "response")

# Load map data
library(ggplot2)
library(raster)
library(sp)
library(sf)

gpkg_path <- 'C:/Users/OYDHAL001/OneDrive - University of Cape Town/CODES2/GADM/gadm41_ZAF.gpkg'
SA_national <- st_read(gpkg_path, layer = "ADM_ADM_0")

# Define and transform projections
map_proj <- st_crs(4326)
SA_national <- st_transform(SA_national, crs = map_proj)

# Define custom colors
custom_colors <- c("#FCFAF9","#EAC797","#F3CF9F","#CED869", "#D3E426", "#9DF016","#26F111","#1F3716")

EC <- as.numeric(cut(Env_2007_scaled$predicted_occupancy, 20))  # grouping in 20

# Plot the predictions
plot7_8 <- ggplot() +
  geom_tile(data = Env_2007_scaled, aes(x = Longitude, y = Latitude, fill = EC/20)) +
  scale_fill_gradientn(name = '', colors = custom_colors, na.value = "transparent") +
  geom_sf(data = SA_national, fill = NA, color = "black", size = 1) +
  theme_minimal() +
  labs(title = "Species prediction for 2007-2008 (GLM)",
       x = "Longitude", 
       y = "Latitude") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    legend.key.size = unit(1.2, "cm")
  )

# Save the plot
ggsave("Species_prediction_GLM_2007_2008.pdf", plot7_8, 
       width = 7.27, height = 10.69, units = "in", 
       device = cairo_pdf)

# Save predictions
write.csv(Env_2007_scaled, file = "Env_2007_with_GLM_predictions.csv", row.names = FALSE)
saveRDS(Env_2007_scaled, file = "Env_2007_with_GLM_predictions.rds")