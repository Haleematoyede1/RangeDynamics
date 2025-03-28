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

