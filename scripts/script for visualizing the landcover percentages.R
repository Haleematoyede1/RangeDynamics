library(readxl)
LC_2018 <- read_excel("rawData/SANLAC_2018_LCPCT.xlsx")
head(LC_2018)

library(terra)
library(ggplot2)
library(dplyr)


LC_2018 <- LC_2018 %>%
  mutate(lat = as.numeric(substr(pentad, 1, 4)) / 100,
         lon = as.numeric(substr(pentad, 6, 9)) / 100)

# Compute Latitude and Longitude from pentad
LC_2018$Latitude <- -(as.numeric(substr(LC_2018$pentad, 1, 2)) + (as.numeric(substr(LC_2018$pentad, 3, 4)) + 2.5) / 60)
LC_2018$Longitude <- (as.numeric(substr(LC_2018$pentad, 6, 7)) + (as.numeric(substr(LC_2018$pentad, 8, 9)) + 2.5) / 60)

# Create a raster
library(ggplot2)
library(sf)

gpkg_path <- 'C:/Users/OYDHAL001/Documents/DynamicOccupancyModels/DOCM_Analysis/GADM/gadm41_ZAF.gpkg'
SA_province <- st_read(gpkg_path, layer = "ADM_ADM_1")  # State/Province level

### Climate related covarites - visualization===================================================

# Rain site covarites
# Rain site covariates plot
ggplot(LC_2018, aes(x = Longitude, y = Latitude, fill = Agric_pct)) +
  ggtitle("Agric_pct 2018") +
  geom_tile() +
  # geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +  # Add provincial borders
  scale_fill_gradientn(
    colours = c('#ffffe5', '#fdae61', '#d73027'),
    na.value = "transparent",
    guide = guide_colorbar(frame.colour = "black")
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12, family = "serif"),
    axis.text = element_text(size = 12, family = "serif"),
    legend.position = "bottom",
    legend.key.width = unit(10, 'mm'),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )



ggplot(LC_2018, aes(x = Longitude, y = Latitude, fill = Forest_pct)) +
  ggtitle("Forest_pct 2018") +
  geom_tile() +
  # geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +  # Add provincial borders
  scale_fill_gradientn(
    colours = c('#fffdfd',"#4D9221","#173007"),
    na.value = "transparent",
    guide = guide_colorbar(frame.colour = "black")
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12, family = "serif"),
    axis.text = element_text(size = 12, family = "serif"),
    legend.position = "bottom",
    legend.key.width = unit(10, 'mm'),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )


#visualization for urban areas
ggplot(LC_2018, aes(x = Longitude, y = Latitude, fill = Urban_pct)) +
  ggtitle("Urban_pct 2018") +
  geom_tile() +
  # geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +  # Add provincial borders
  scale_fill_gradientn(
    colours = c('#0c0c0c', '#071aff', '#ff0000', '#ffbd03', '#fbff05', '#fffdfd'),
    na.value = "transparent",
    guide = guide_colorbar(frame.colour = "black")
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12, family = "serif"),
    axis.text = element_text(size = 12, family = "serif"),
    legend.position = "bottom",
    legend.key.width = unit(10, 'mm'),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )




ggplot(LC_2018, aes(x = Longitude, y = Latitude, fill = Rivers_pct)) +
  ggtitle("Rivers_pct 2018") +
  geom_tile() +
  #geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +  # Add provincial borders
  scale_fill_distiller('', palette = 'Blues', direction = 1,
                       na.value = "transparent", guide = guide_colorbar(frame.colour = "black")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12, family = "serif"),
        axis.text = element_text(size = 12, family = "serif"),
        legend.position = "bottom",
        legend.key.width = unit(10, 'mm'),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20))

#visualization for grass lands
ggplot(LC_2018, aes(x = Longitude, y = Latitude, fill = Grass_pct)) +
  ggtitle("Grass_pct 2018") +
  geom_tile() +
  # geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +  # Add provincial borders
  scale_fill_gradientn(
    colours = c('#fffdfd',"#ffcd73","#ff9916"),
    na.value = "transparent",
    guide = guide_colorbar(frame.colour = "black")
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12, family = "serif"),
    axis.text = element_text(size = 12, family = "serif"),
    legend.position = "bottom",
    legend.key.width = unit(10, 'mm'),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )



ggplot(LC_2018, aes(x = Longitude, y = Latitude, fill = FloodeVeg_pct)) +
  ggtitle("FloodeVeg_pct 2018") +
  geom_tile() +
  # geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +  # Add provincial borders
  scale_fill_distiller('', palette = "BuPu", direction = 1,
                       na.value = "transparent", guide = guide_colorbar(frame.colour = "black")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12, family = "serif"),
        axis.text = element_text(size = 12, family = "serif"),
        legend.position = "bottom",
        legend.key.width = unit(10, 'mm'),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20))


ggplot(LC_2018, aes(x = Longitude, y = Latitude, fill = Others_pct)) +
  ggtitle("Others_pct 2018") +
  geom_tile() +
  # geom_sf(data = SA_province, fill = NA, color = "black", size = 0.5) +  # Add provincial borders
  scale_fill_distiller('', palette = "YlOrBr", direction = 1,
                       na.value = "transparent", guide = guide_colorbar(frame.colour = "black")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12, family = "serif"),
        axis.text = element_text(size = 12, family = "serif"),
        legend.position = "bottom",
        legend.key.width = unit(10, 'mm'),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
