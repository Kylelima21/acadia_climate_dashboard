#plot functions

create_temperature_plot <- function(data, selected_lines, models = NULL) {
  p <- ggplot(data, aes(x = Year)) +
    PLOT_THEME +
    scale_x_continuous(breaks = pretty(data$Year)) +
    labs(
      title = "Average Temperature (1895-2024)",
      x = "Year",
      y = "Temperature (°C)"
    )
  
  # Add temperature lines based on selection
  if ("NOAA Average Max Temp" %in% selected_lines) {
    p <- p + geom_line(
      aes(y = `NOAA Average Max Temp`, color = "NOAA Average Maximum Temp.")
    )
    if (!is.null(models$noaa_max)) {
      p <- add_model_line(p, models$noaa_max, "NOAA Average Max Temp")
    }
  }
  
  p + scale_color_manual(values = COLOR_PALETTE)
}

create_anomaly_plot <- function(data, station = "NOAA") {
  col_name <- paste0(station, " Temp Anom")
  
  ggplot(data, aes(x = `Year-Month`)) +
    geom_bar(
      aes(
        y = .data[[col_name]],
        fill = .data[[col_name]] > 0,
        text = hover_text
      ),
      stat = "identity"
    ) +
    PLOT_THEME +
    scale_fill_manual(
      values = c("TRUE" = "red", "FALSE" = "blue"),
      labels = c("Above baseline", "Below baseline")
    )
}

add_model_line <- function(plot, model, var_name) {
  plot +
    geom_smooth(
      aes(y = .data[[var_name]]),
      method = "lm",
      se = TRUE,
      fill = "grey80",
      alpha = 0.5,
      color = NA
    ) +
    geom_line(
      aes(y = .data[[var_name]]),
      stat = "smooth",
      method = "lm",
      color = "black",
      linewidth = 0.8
    )
}
