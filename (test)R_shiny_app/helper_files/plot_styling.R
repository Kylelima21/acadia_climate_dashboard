#plot styling

# Theme and style definitions
PLOT_THEME <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_line(color = "grey90"),
    panel.grid.major = element_line(color = "grey85")
  )

COLOR_PALETTE <- c(
  "NOAA Average Temp." = "#000000",
  "NOAA Average Maximum Temp." = "#CC3300",
  "NOAA Average Minimum Temp." = "#003399",
  "McFarland Average Temp." = "#00CC00"
)

PLOT_SETTINGS <- list(
  date_breaks = "5 years",
  date_labels = "%Y",
  y_label_temp = "Temperature (°C)",
  y_label_precip = "Precipitation (in)",
  plot_height = "600px"
)
